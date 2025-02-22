## load stocks list from json file====

## step1: write the new coming stocks to the json file by hand

## step2: read the json file which contains the stock id, name and date
library(jsonlite)
library(tidyverse)
file_json <- here("data/bid-stocks-new.json")
# Read the JSON file
json_data <- fromJSON(file_json)

## step3: Convert to tibble and rename columns
names_new <- c("date","stock_id", "market", "stock_name")
tbl_json <- json_data %>%
  imap_dfr(~ tibble(
    date = .y,
    stocks = .x
  )
  ) %>%
  unnest(stocks) %>%
  # rename columns
  rename_all(., ~names_new)
