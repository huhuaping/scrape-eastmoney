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
library(jsonlite)

# step 1: down scrolling the picture from desktop software of "eastmoney"
##  and save it as .png file

# step 2: read the json file which contains the stock id, name and date
## load stock list====
##read the json file which contains the stock id, name and date
## the table named as `tbl_json`
source(here("code/00-prepare-json.R"))


# step 3: load the picture file and preprocess it
## we will preprocess pictures day by day
## date_process <- "2024-12-26"
(date_process <- max(tbl_json$date)) # the latest date

## now create file path list for all pictures which need to be processed
tbl_file <- tbl_json %>%
  filter(date == date_process) %>%
  mutate(file_path = here(glue("code/bid/{stock_id}-{date_process}.png"))) 

file_list <- tbl_file %>%
  pull(file_path)
num_tiles <- length(file_list)
cat("Total picture files need to be processed: ", num_tiles, "\n")

# Function to create output paths
create_output_path <- function(image_path) {
  out_path <- str_replace(image_path, "\\.png$", "-proc\\.png")
}
# output_paths <- map(file_list, create_output_path)
output_paths <- sapply(file_list, create_output_path) %>%
  unlist() %>%
  unname()

# process image using `imager`====
library(imager)
process_image_imager <- function(image_path, 
                                 output_path) {
  library(imager)
  tryCatch({
    im <- load.image(image_path)
    width <- dim(im)[1]
    height <- dim(im)[2]
    scale <- 1076 / width
    # Resize using imager's resize function
    im <- resize(im, size_x = width*scale, size_y = height*scale)  # e.g. resize to 600 width, adjust height
    
    # Convert to grayscale
    im_gray <- grayscale(im)
    
    # Apply thresholding to make it black and white
    # threshold <- mean(im_gray)*1.15 # Or choose your specific threshold value.
    # im_bw <- im_gray < threshold # use the ">" method to get the binary image
    # 
    # # Convert to numeric array, not binary
    # im_bw <- as.cimg(as.numeric(im_bw)) # convert binary image to numeric for further process
    # 
    # Save as PNG using imager
    save.image(im_gray, output_path)
    
    return(TRUE)
  }, error = function(e) {
    message(paste("Error processing", image_path, ":", e$message))
    return(FALSE)
  })
}

## loop method to process image
for (i in 1:length(file_list)){
  process_image_imager(file_list[i], output_paths[i])
  cat("Picture ", file_list[i], "of total", length(file_list), " done.\n")
  gc()
}



# loop method to process image====
## now we will process the picture file one by one with loop
## and save the processed picture file with appending "-proc" to the file name.
## also we need a process bar to show the process status.
library(tictoc)
i <- 2
for (i in 2:seq_along(file_list)) {
  # begin tick tok
  
  file_path <- file_list[i]
  file_path <- "D:/github/scrape-eastmoney/code/bid/002364-2024-12-27-backup-s10-rgt.png"
  cat(file_path, "\n")
  stock_id <- tbl_file$stock_id[i]
  stock_name <- tbl_file$stock_name[i]
  image <- image_read(file_path)
  width <- image_info(image)$width
  scale <- scales::percent(1920/width) # unify the scale and adjust width to 1920 
  image <- image_resize(image, scale) # Scale up
  cat("Scale up ", scale, "\n")
  # image <- image_convert(image, type = "grayscale") # Convert to grayscale
  # cat("Convert to grayscale\n")
  # image <- image_normalize(image) # Normalize the image
  # cat("Normalize the image\n")
  # image <- image_enhance(image) # Enhance the image
  # cat("Enhance the image\n")
  # image <- image_deskew(image, threshold = 50) # Adjust the threshold
  # cat("Adjust the threshold\n")
  image <- image_quantize(image, colorspace = 'gray') # reduce to 2 colors
  cat("Reduce to 2 colors\n")
  # image <- image_reducenoise(image, radius = 2) # reduce noise
  # cat("Reduce noise\n")
  file_proc <- here(glue("code/bid/{stock_id}-{date_process}-proc.png"))
  image_write(image, file_proc)
  cat("\n Picture: ",stock_id,"(",stock_name,") on ",date_process,
      ". The ",i, "of total", num_tiles, " done.\n", file_proc, "\n")
}



# process image by parallel method====
library(parallel)
library(purrr)



# function to process image
process_image <- function(image_path, output_path) {
  tryCatch({
    image <- image_read(image_path)
    width <- image_info(image)$width
    scale <- scales::percent(1920/width) # unify the scale and adjust width to 1920 
    # Resize first
    image <- image_resize(image, scale) # Scale up
    cat("Scale up ", scale, "\n")
   # Quantize image
    image <- image_quantize(image, colorspace = 'gray') # reduce to 2 colors
    cat("Reduce to 2 colors\n")
    # Convert if needed - If there is an output type that is compatible, just skip it.
    #image <- image_convert(image, "png")  # Consider if need the convert here.
    
    image_write(image, output_path)
    return(TRUE) # return TRUE to indicate success
  }, error = function(e) {
    message(paste("Error processing", image_path, ":", e$message))
    return(FALSE)  #return FALSE to indicate error
  })
}


process_image(file_list[3], output_paths[3])



# Get the number of CPU cores
num_cores <- detectCores() - 1 # leave one core free

# Create clusters
cl <- makeCluster(num_cores) # to improve the efficiency


# export environment variables
clusterExport(cl, list("image_paths","output_paths","process_image"))

# Parallel processing with cluster
start_time <- Sys.time()

results <- parLapply(cl,seq_along(image_paths), function(i){
  process_image(image_paths[[i]], output_paths[[i]])
})
stopCluster(cl) # clean cluster to release core usage
end_time <- Sys.time()
print(paste("Processing time", end_time - start_time, sep = ": "))

# Print summary
print(paste("Processed", sum(unlist(results))," images out of", length(image_paths), sep=" "))

# OCR the image====
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

