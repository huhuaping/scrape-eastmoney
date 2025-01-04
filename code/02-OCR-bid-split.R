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
## (date_process <- "2024-12-20")
(date_process <- max(tbl_json$date)) # the latest date

## now create file path list for all pictures which need to be processed
tbl_file <- tbl_json %>%
  filter(date == date_process) %>%
  mutate(file_path = here(glue("code/bid/{stock_id}-{date_process}.png"))) 

file_list <- tbl_file %>%
  pull(file_path)# %>%
  #.[1] # only process the first two pictures for testing
# check files exist or not
file_exists <- file_list %>% 
  map(~file.exists(.)) %>% 
  unlist() 
if (sum(file_exists) == length(file_list)){
  cat("All files exist.\n")
} else {
  cat("Some files",which(!file_exists), " do not exist.\n")
}
# total images need process
num_image <- length(file_list)
cat("Total picture files need to be processed: ", num_image, "\n")


# OCR the image by loop====
## loop through all the pictures
k <- 1
for (k in 1:num_image){
  ## read image====
  image <- image_read(file_list[k])
  cat("OCR Image ", k,"of",num_image, " begin.\n")
  ## prepare to split image ====
  info <- image_info(image)
  height_raw <- info$height
  tile_width_raw <- info$width # tile width is the same as the original image
  tile_scale <- 1024 / tile_width_raw # calculate the scale factor
  tile_width <- tile_width_raw*tile_scale # scale the tile width to target width 1024
  height_scale <- height_raw* tile_scale # scale the height by the same factor
  
  # Adjust based on tesseract's limits and experimentation
  ## set the tile's height and overlap
  tile_height <- 2000/tile_scale 
  overlap <- tile_height*0.1 # 10% overlap
  num_tiles <- ceiling((height_raw - overlap) / (tile_height - overlap))
  cat(num_tiles, "tiles will be processed.\n")
  
  ## loop through all the tiles====
  tbl_ocr <- tibble(text = character())
  for (i in 1:num_tiles) {
    ## split the image into tiles====
    y_offset <- (i - 1) * (tile_height - overlap)
    # Prevent going beyond image height for the last tile
    current_tile_height <- min(tile_height, info$height - y_offset)
    # crop the image with the tile height and overlap
    tile <- image_crop(image, paste0(info$width, "x", current_tile_height, "+0+", y_offset))
    ## process the tile====
    # resize the tile
    tile <- image_resize(tile, scales::percent(tile_scale))
    # Normalize the image
    tile <- image_normalize(tile) 
    # reduce tile to 2 colors
    tile <- image_quantize(tile, colorspace = 'gray')
    # # apply threshold
    # ## Lower values will generally result in darker images
    # ## Higher values will generally results in brighter images.
    # tile <- tile |> 
    #   image_threshold( threshold = "50%", type = "black")|> 
    #   image_threshold( threshold = "50%", type = "white")
    # Enhance the tile
    tile <- image_enhance(tile)
    # reduce noise
    tile <- image_reducenoise(tile, radius = 1)
    # deskew the tile
    tile <- image_deskew(tile, threshold = 50)
    # get the information of the tile
    ## image_info(tile)
    
    tile_text <- tesseract::ocr(tile, engine = "chi_sim") # Chinese simplified
    #all_text <- paste(all_text, tile_text, sep = "\n")
    cat("Tile ", i, "of toal", num_tiles," OCR done.\n")
    
    ## tidy text and combind as tibble====
    ### split text by line
    tile_split <- strsplit(tile_text, "\n")[[1]]
    
    tbl_tile <- tibble(text = tile_split) %>%
      filter(text != "") %>%
      mutate(text = str_trim(text)) %>%
      # remove first row
      .[-1,]
    
    tbl_ocr <- bind_rows(tbl_ocr, tbl_tile)
    # explicit remove tile
    rm(tile)
    # Force garbage collection (optional, but good to do)
    gc()
  }
  
  ## save the OCR result as .csv file====
  file_name <- str_extract(file_list[k], "\\d{6}-\\d{4}-\\d{2}-\\d{2}")
  (file_out <- here(glue("data/bid-ocr/{file_name}.csv")))
  write_csv(tbl_ocr, file_out)
  # explicit remove image
  rm(image)
  # Force garbage collection (optional, but good to do)
  gc()
  # print information
  cat("The OCR result is saved as ", file_out, "\n")
  cat("The OCR of image",k,"(",file_name,") is finished\n")
}


