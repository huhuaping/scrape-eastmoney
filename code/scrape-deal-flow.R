# scrape stock deal flow from webpage 
## by using docker + RSelenium + tvnviewer

# Load libraries====
require(openxlsx)
require("rvest")
#require("xml2")
require("httr")
require("stringr")
require("tidyverse")
require("tidyselect")
require("here")
library("RSelenium")
library(jsonlite)
library(glue)

# prepare the docker container====
#### you should run and start docker desktop first.
#### then run code in 'window power shell': docker run --name chrome -v /dev/shm:/dev/shm -d -p 4445:4444 -p 5901:5900 selenium/standalone-chrome-debug:latest
### for firefox: docker run --name firefox -v /dev/shm:/dev/shm -d -p 4445:4444 -p 5901:5900 selenium/standalone-firefox:latest

# start the tvnviewer====
##  set remote host: 127.0.0.1:5901
##  set password: secret

# start the RSelenium====
## run local driver
remDr <- RSelenium::remoteDriver(remoteServerAddr = "localhost",
                                 port = 4445L,
                                 browserName = "chrome")
remDr$open()
remDr$maxWindowSize()


# scrape website====
## navigate to the target website
tbl_stocks <- tribble(
  ~stock_id, ~stock_name,
  "603306", "华懋科技"
) 

q <- 1
stock_id <- tbl_stocks$stock_id[q]
stock_name <- tbl_stocks$stock_name[q]
url_tar <- glue("https://quote.eastmoney.com/f1.html?newcode=1.{stock_id}")
remDr$navigate(url_tar)

## get the page source
page_source <- remDr$getPageSource()[[1]]
# tbl_read <- read_html(page_source) %>%
#   html_table() 
# n_tbl <- length(tbl_read)
## combind the last four tables
# tbl_read <- bind_rows(
#   tbl_read[[n_tbl-3]] %>% mutate_all(., as.character), 
#   tbl_read[[n_tbl-2]] %>% mutate_all(., as.character), 
#   tbl_read[[n_tbl-1]] %>% mutate_all(., as.character), 
#   tbl_read[[n_tbl]] %>% mutate_all(., as.character),
#   ) 


## find the total page number
### css selector pattern to find the total page number
css_tar <- "#app > div > div > div.stockf1 > div.f1page2 > div > span:nth-child(5)"
elm <- remDr$findElement(using = "css selector", value = css_tar)
page_total <- elm$getElementText() %>%
  # extract max page number
  str_extract(., "(?<=/)\\d+(?=页)") %>%
  as.numeric()
cat(page_total)
## find the page fill box and input the page number
# css_tar <- "input.gotoinput"
# elm <- remDr$findElement(using = "css selector", value = css_tar)
# ## clean the input box
# elm$clearElement()
# ## input the page number of 1
# elm$sendKeysToElement(list(as.character(1)))
# ## click the submit button
# css_tar <- "form > input[type=submit]:nth-child(2)"
# elm <- remDr$findElement(using = "css selector", value = css_tar)
# elm$clickElement()

## loop through the pages====
tbl_tar <- tibble()
i <- 1
for (i in 1:page_total) {
  ## get the page source
  page_source <- remDr$getPageSource()[[1]]
  tbl_read <- read_html(page_source) %>%
    html_table()
  
  ## combind the last four tables
  tbl_read <- bind_rows(
    tbl_read[[n_tbl-3]] %>% mutate_all(., as.character), 
    tbl_read[[n_tbl-2]] %>% mutate_all(., as.character), 
    tbl_read[[n_tbl-1]] %>% mutate_all(., as.character), 
    tbl_read[[n_tbl]] %>% mutate_all(., as.character),
  )
  
  ## append the table to the target table
  tbl_tar <- bind_rows(tbl_tar, tbl_read) 
                       
  ## find the page fill box and input the page number
  css_tar <- "input.gotoinput"
  elm <- remDr$findElement(using = "css selector", value = css_tar)
  ## clean the input box
  elm$clearElement()
  ## input the page number
  elm$sendKeysToElement(list(as.character(i + 1)))
  ## click the submit button
  css_tar <- "form > input[type=submit]:nth-child(2)"
  elm <- remDr$findElement(using = "css selector", value = css_tar)
  elm$clickElement()
  Sys.sleep(0.5)
  
  ## print out
  cat("Page ", i, "of", page_total, " is done.\n")
}

## close the driver====
remDr$closeServer()
remDr$close()
rm(remDr)
gc()


## add date column
## you can specify a custom date
#date_tar <- as.Date("2024-12-24")
## or default as today's date
( date_tar <- Sys.Date())

tbl_tar <- tbl_tar %>%
  # remove NA rows or empty
  filter(!is.na(`时间`)) %>%
  filter(`时间`!="") %>%
  # add a column of the target date
  add_column(date = date_tar, .before = 1) %>%
  # add the stock name and id
  add_column(stock_name = stock_name, .before = 1) %>%
  add_column(stock_id = stock_id, .before = 1) 
  

## save csv file to local directory `data/`====
## create the file name appending with the date of today
file_name <- glue("{stock_id}-{date_tar}.csv")
cat(file_name)
write_csv(tbl_tar, here::here("data","deal-flow", file_name))
