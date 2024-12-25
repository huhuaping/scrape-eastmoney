# I want to OCR and extract text from picture file (such as .png file). 
# I use R programming language. 
# And as I know we can use R package `magick ` and `tesseract`.

# load packages====
library(tidyverse)
library(magrittr)
library(magick)
library(tesseract)
library(here)
library(glue)
# Load the image====
date_tar <- "2024-12-23"
file_path <- here(glue("code/bid/603306-hwkj-{date_tar}-s60h100.png"))
## other demo picture
#file_path <- here("code/bid/bid-demo-02.png")
image <- image_read(file_path)

# Option 1: tesseract====

## prepare tesseract data====
### download chinese language data automatedly, 
### but it may fail sometimes.
if(is.na(match("chi", tesseract_info()$available))){
  tesseract_download("chi")
}
### now you should download the language data manually
### in the url: https://www.google.com/url?sa=E&source=gmail&q=https://github.com/tesseract-ocr/tessdata
### check the data path, and copy the downloaded tesseract data to the path directory
tesseract_info()$datapath

### check the available language data
isTRUE(match("chi_sim", tesseract_info()$available)
)
## Preprocessing image by using magick====
image <- image_resize(image, "200%") # Scale up
image <- image_convert(image, type = "grayscale") 
image <- image_normalize(image) # Normalize the image
image <- image_enhance(image) # Enhance the image
image <- image_deskew(image, threshold = 50) # Adjust the threshold 
image <- image_quantize(image, colorspace = 'gray') # reduce to 2 colors
image <- image_reducenoise(image, radius = 2) # reduce noise
# image <- image_threshold(image, type = "otsu") # Adaptive thresholding

## save processed image
(file_proc <- here(glue("code/bid/603306-hwkj-{date_tar}-s60h100-proc.png")))
image_write(image, file_proc)

## split picture and ORC with loop procedure====
info <- image_info(image)
tile_height <- 2000 # Adjust based on tesseract's limits and experimentation
overlap <- 200
num_tiles <- ceiling((info$height - overlap) / (tile_height - overlap))
cat(num_tiles)

all_text <- ""
i <- 1
for (i in 1:num_tiles) {
  y_offset <- (i - 1) * (tile_height - overlap)
  
  # Prevent going beyond image height for the last tile
  current_tile_height <- min(tile_height, info$height - y_offset)
  
  tile <- image_crop(image, paste0(info$width, "x", current_tile_height, "+0+", y_offset))
  
  # Optional: Resize if still too large or to improve OCR accuracy
  # tile <- image_scale(tile, "x1000") # Example: Scale to a height of 1000
  
  tile_text <- tesseract::ocr(tile, engine = "chi_sim") # Chinese simplified
  all_text <- paste(all_text, tile_text, sep = "\n")
  cat("Tile ", i, "of toal", num_tiles," done.\n")
}

#cat(all_text)

## tidy text and save as tibble====
### split text by line
text_split <- strsplit(all_text, "\n")[[1]]

tbl_text <- tibble(text = text_split) %>%
  filter(text != "") %>%
  mutate(text = str_trim(text)) %>%
  # remove rows which the string is not start as format "09::34::35",
  # which is the time format
  filter(str_detect(text, "^\\d{2}:\\d{2}:\\d{2}")) %>%
  # remove any white space after character of "撤"
  mutate(text = str_replace_all(text, "撤\\s+", "撤")) %>%
  # replace any "-" between any number strings with "."
  mutate(text = str_replace_all(text, "(\\d)-(\\d)", "\\1.\\2")) %>%
  # remove any white space before character of "万"
  mutate(text = str_replace_all(text, "\\s+万", "万")) %>%
  # remove all character "“" or "_"
  mutate(text = str_remove_all(text, "“|_")) %>%
  # extract the time string
  mutate(time = str_extract(text, "^\\d{2}:\\d{2}:\\d{2}")) %>%
  # extract the hour string which has maxim two digits before the character "h"
  mutate(hour = str_extract(text, "\\d{1,2}h")) %>%
  # exract the minute string which has maxim two digits before the character "m"
  mutate(minute = str_extract(text, "\\d{1,2}m")) %>%
  # extract the second string which has maxim two digits before the character "s"
  mutate(second = str_extract(text, "\\d{1,2}s")) %>%
  # extract the price string before a white space and have maxmim 3 numbers and a "." and then maxim two numbers
  # which may be after the time string with a white space,
  # or be after the character "s" or "m" with a white space
  mutate(price = str_extract(text, "(?<=\\s)\\d{1,3}\\.\\d{1,2}|(?<=s\\s)\\d{1,3}\\.\\d{1,2}|(?<=m\\s)\\d{1,3}\\.\\d{1,2}")) %>%
  # extract the volume string which has maxim 3 numbers before the character "万"
  mutate(volume = str_extract(text, "(\\d{1,10}\\.\\d{1,2})(?=万)")) %>%
  # extract the hands numbers after the character "万"
  mutate(hands = str_extract(text, "(?<=万\\s)\\d{1,10}"))  %>%
  # remove any character "-" in text
  mutate(text = str_remove_all(text, "-")) %>%
  # extract the volume string which has maxim 3 numbers before the character "万"
  mutate(volume = str_extract(text, "(\\d{1,10}\\.?\\d{1,2})(?=万)")) %>%
  # replace any character similar to "撤" including: "撞"
  mutate(text = str_replace_all(text, "撞|撒", "撤")) %>%
  # remove any white space after "撤"
  mutate(text = str_replace_all(text, "撤\\s+", "撤")) %>%
  # re extract from text if hands numbers has value NA, 
  # and extract hands numbers before the character "卖" or "买" or "撤"
  mutate(hands = ifelse(is.na(hands), str_extract(text, "\\d{1,10}(?=\\s买|\\s卖|\\s撤)"), hands)) %>%
  # extract bid direction with character "撤卖"or "撤买" or "买" or "卖"
  mutate(direction = str_extract(text, "撤卖|撤买|买|卖")) %>%
  # re extract bid direction if it has value NA,
  # and replace bid direction NA if text it is end with character "E"
  mutate(direction = ifelse(is.na(direction)&str_detect(text, "E.?\\d{0,2}$"), "买",direction)) %>%
  # convert columns to numeric: price, volume, hands
  mutate(price = as.numeric(price),
         volume = as.numeric(volume),
         hands = as.numeric(hands)) %>%
  # round volume to 1 decimal
  mutate(volume = round(volume, digits = 1)) %>%
  # calculate NA value of hands by equation: floor(100*volume/price) if hands is NA
  mutate(hands = ifelse(is.na(hands), floor(100*volume/price), hands)) %>%
  # calculate NA value of volume by equation: hands*price if volume is NA
  mutate(volume = ifelse(is.na(volume), hands*price/100, volume)) %>%
  # filter if hands or price is not NA
  filter(!is.na(hands), !is.na(price)) %>%
  # remove dulplicate rows
  distinct() %>%
  # create calculated volume by equation: hands*price
  mutate(volume_cal = round(hands*price/100,digits = 1)) %>%
  # check raw volume and calculated volume
  mutate(volume_diff = volume - volume_cal) %>%
  # logics check if volume is equal to calculated volume
  mutate(volume_check = ifelse(abs(volume_diff)<1.1, "TRUE", "FALSE")) %>%
  # adjust wrong detected hands if volume is FALSE and hands is larger than 5000
  # and divide hands by 100 and then floor it
  mutate(hands = ifelse(volume_check == FALSE&hands>5000, floor(hands/100), hands))# %>%
  

## save clean table of text to rdata file====
(file_out <- here(glue("code/bid/603306-hwkj-{date_tar}-s60h100-clean.rds")))
tbl_text %>%
  # add date column
  mutate(date = date_tar) %>%
  saveRDS(., file_out)



## try R package `tabulizer` to extract table====

## Convert the magick image to a suitable format
##(e.g., a matrix of pixel values)
## install packages("tabulizer"), 
## see url: https://blog.djnavarro.net/posts/2023-06-16_tabulizer/
# renv::install("ropensci/tabulizer") # Requires rJava
library("tabulapdf")

# Convert the magick image to a suitable format (e.g., a matrix of pixel values)
image_matrix <- as.matrix(image_data(image, 'gray')) # Example

# Try to extract tables using tabulizer (experiment with parameters)
tbl_extract <- extract_tables(image_matrix, method = "lattice")

# Process the extracted tables
# ...

# Option 2: google vision API====

# install packages("RoogleVision")
# renv::install("cloudyr/RoogleVision")

library("RoogleVision")
library("keyring")

### plugin your credentials
#keyring::keyring_create("gg-vision") 4422
#keyring::keyring_list()
#keyring::key_set("id", keyring = "gg-vision")
#keyring::key_set("secret", keyring = "gg-vision")
# keyring::key_delete("gg-vision")
options("googleAuthR.client_id" = keyring::key_get("id", keyring = "gg-vision"))
options("googleAuthR.client_secret" = keyring::key_get("secret", keyring = "gg-vision"))

## use the fantastic Google Auth R package
### define scope!
options("googleAuthR.scopes.selected" = c("https://www.googleapis.com/auth/cloud-platform"))
googleAuthR::gar_auth()


#Basic: you can provide both, local as well as online images:
file_proc <- here("code/bid/603306-hwkj-2024-12-20-s60h100-proc.png")
file_proc <- here("code/bid/bid-demo-02.png")
txt <- getGoogleVisionResponse(imagePath=file_proc, feature="TEXT_DETECTION",numResults = 1)

# option 3: imagerExtra====
## You need to install the tesseract package first, 
## as imagerExtra uses it for OCR tasks.
## renv::install("imagerExtra")
library(imagerExtra)

# Load and preprocess image
img <- load.image(file_proc)
clean_img <- DenoiseDCT(img, 0.01) %>% ThresholdAdaptive(., 0.1, range = c(0,1))

# Perform OCR
text <- ocr(image)

# option 4: daiR====
## renv::install("daiR")
library(daiR)

## synchronous processing
result <- dai_sync(file_proc)
text <- get_text(result)

