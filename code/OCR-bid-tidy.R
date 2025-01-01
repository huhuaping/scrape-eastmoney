# read csv files and tidy the data set

# load packages====
library(tidyverse)
library(here)
library(glue)
library(lubridate)

# read csv files====
dir_tar <- here("data/bid-ocr")
files_tar <- list.files(dir_tar)
paths_tar <- list.files(dir_tar, full.names = TRUE)
ymd_tar <- ymd(c("2024-12-27", "2024-12-30", "2024-12-31"))
tbl_read <- tibble(files = files_tar, path = paths_tar) %>%
  mutate(stock_id= str_sub(files, 1, 6),
         date = str_sub(files, 8, 17) %>% ymd()) %>%
  # you can select which files to read 
  filter(date %in% ymd_tar) %>%
  mutate(data = map(path, read_csv) ) 

tbl_unnest <- tbl_read  %>%
  select(-path, -files) %>%
  unnest(data)

# function for tidy ====

## demo data set====
tbl_demo <- tbl_unnest %>%
  filter(stock_id == "002334")

## custom function====
## create a function to tidy the data set

tbl_test <- tidy_bid(tbl = tbl_demo)

tidy_bid <- function(tbl){
  ## tidy the table
  ## 1. remove unexpected rows
  ## 1.1 column "text" should contain time format string "\\d{2}:\\d{2}:\\d{2}",
  ## and column "text" should contain chinese character "万"
  result <- tbl %>%
    # trim the text
    mutate(text = str_trim(text)) %>%
    # filter rows of incorrect time strings
    filter(str_detect(text, "\\d{2}:\\d{2}:\\d{2}")) %>%
    ## detect the chinese characters and extract bid direction
    # extract all chinese characters
    mutate(
      han = str_extract_all(text, "\\p{Han}+") %>% 
        map_chr(paste, collapse = "")
      ) %>%
    # filter rows of does not contain "万"
    filter(str_detect(han, "万")) %>%
    # replace "E$" to "卖" due to the incorrect OCR
    mutate(text = str_replace(text, "E$", "卖")) %>%
    # filter rows of does not contain any capital English characters 
    filter(!str_detect(text, "[A-Z]")) %>%
    # replace unexpected ocr character "“ " to empty
    mutate(text = str_replace(text, "“ ", "")) %>%
    # replace time format follows immediately with a digit,
    ## and add a white space after the time format string
    mutate(text = str_replace(text, "\\d{2}:\\d{2}:\\d{2}(\\d)", " \\1"))
  
  ## 2. detect the incorrect bid direction characters,
  ## and create a regular expression which prepare to be replaced by the correct ones 
  ## 2.1 create the regular expression
  ptn_incorrect <- result %>%
    mutate(
      char_withdraw = str_extract(han, "(?<=万).+(?=买|卖)")
    ) %>%
    select(char_withdraw) %>%
    filter(!is.na(char_withdraw)) %>%
    unique() %>%
    pull(char_withdraw) %>%
    paste(., collapse = "|")
  
  ## 2.2 replace and tidy
  result <- result %>%
    ## replace the incorrect characters
    mutate(
      text = str_replace_all(text, ptn_incorrect, "撤"),
      han = str_replace_all(han, ptn_incorrect, "撤")
    ) %>%
    ## clean white spaces after "撤"
    mutate(text = str_replace(text, "(?<=撤)(\\s)", "")) %>%
    ## replace any "_" to ""
    mutate(text = str_replace_all(text, "_", "")) %>%
    ## clean white spaces before "万"
    mutate(text = str_replace(text, "(\\s)(?=万)", "")) %>%
    # extract any chinese characters not in "万|买|卖|撤" in column "han"
    mutate(
      han_not_expect = str_extract_all(han, "[^万买卖撤]+") %>% 
        map_chr(paste, collapse = ""),
      is_not_expect = str_detect(han_not_expect, "\\p{Han}")
    ) %>%
    # filter rows of has not expected chinese characters in column "han"
    filter(!is_not_expect) #%>%
    
  
  ## 3. detect the duration of the bid withdraw
  ## 3.1 if no duration, add a place hold symbol "#" after the time format string
  result <- result %>%
    ## extract the duration moments
    mutate(
      hh = str_extract(text, "\\d{1,2}(?=h)"), # hours
      mm = str_extract(text, "\\d{1,2}(?=m)"), # minutes
      ss = str_extract(text, "\\d{1,3}(?=s)")  # seconds, may be wrong ocr "s" to "5"
    ) %>%
    ## create a column to indicate no duration
    mutate(
      no_duration = ifelse(is.na(hh) & is.na(mm) & is.na(ss), TRUE, FALSE)
    ) %>%
    ## if no duration, add a place hold symbol "#" after the time format string
    mutate(
      text = ifelse(
        no_duration, 
        str_replace(text, "^\\d{2}:\\d{2}:\\d{2}", "\\0 #"), 
        text)
    )
  
  ## 3.2 detect the numbers of white spaces within the text
  ## and we can use the number of white spaces to split the text
  ## we expect each row of the text has 5 white spaces, so we can split the text by 5 white spaces
  result <- result %>%
    mutate( n_white = str_count(text, "\\s") ) %>%
    #filter(n_white != 5) %>%
    # replace the incorrect "5" to "s" according to the number of white spaces `n_white`
    mutate(
      text = ifelse(
        n_white ==6 , 
        str_replace(text, "# (\\d{1,2})5", "\\1s"), 
        text)
      ) %>%
    # calculate the number of white spaces again
    mutate( n_white = str_count(text, "\\s") ) %>%
    # filter rows of incorrect number of white spaces
    filter(n_white == 5)
  
  # 4. split text, extract information, and make assure the correct column format
  names_new <- c("time", "duration", "price", "funds", "hands", "direction")
  result <- result %>%
    # split text
    separate(col =text, into = names_new, sep = " ", remove =FALSE ) %>%
    # extract duration to "hh", "mm", "ss" again
    mutate(
      hh = str_extract(duration, "\\d{1,2}(?=h)") %>% as.numeric(),
      mm = str_extract(duration, "\\d{1,2}(?=m)") %>% as.numeric(),
      ss = str_extract(duration, "\\d{1,3}(?=s)")  # may be wrong OCR "42s" to "425s"
    ) %>%
    # correct the "ss" has three digits and extract the first 2 digits
    mutate(
      ss = str_extract(ss, "\\d{1,2}") %>% as.numeric()
    ) %>%
    # replace NA to 0 for "hh", "mm", "ss"
    mutate_at(c("hh", "mm", "ss"), replace_na, replace=0) %>%
    # convert columns to numeric
    ## column of price, and remove not OCR price correctly 
    ## which may contains strings cause  NA when convert to numeric
    mutate(price = str_trim(price) %>% as.numeric() ) %>%
    filter(!is.na(price)) %>%
    ## columns of funds and hands 
    mutate(
      funds = str_replace(funds, "万", "") %>% str_trim( ) %>%as.numeric(),
      hands = str_trim(hands) %>% as.numeric()
    ) %>%
    ## check funds, and remove rows with unexpected funds,
    ## the sources of unexpected funds may due to 
    ## wrong price or wrong hands, or both
    mutate(
      funds_calc = price*hands/100,
      diff_calc = funds - funds_calc,
      is_expect_funds = ifelse(diff_calc < funds*0.101, TRUE, FALSE)
      ) %>%
    filter(is_expect_funds) %>%
    select(text, hh, mm, ss, price, funds, hands, direction)
  
  return(result)
    
}

# tidy and write tables ====
## tidy all raw csv tables
tbl_tidy <- tbl_read %>%
  ## batch tidy tables
  mutate(dt = map(data, tidy_bid)) 
## write out all tidy tables
(out_dir <- here("data/bid-tidy"))
tbl_tidy %>%
  mutate(path_out = glue::glue("{out_dir}/{files}")) %>%
  mutate(write =map2(.x =dt, .y= path_out, function(x, y) write.csv(x, file = y)))


