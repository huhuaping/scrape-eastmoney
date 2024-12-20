## R包准备----
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

## 启动Rselenum----

#part 01 start docker + RSelenium
# 1. run docker service and container
#### you should run and start docker desktop first.
#### then run code in 'window power shell': docker run --name chrome -v /dev/shm:/dev/shm -d -p 4445:4444 -p 5901:5900 selenium/standalone-chrome-debug:latest

### for firefox: docker run --name firefox -v /dev/shm:/dev/shm -d -p 4445:4444 -p 5901:5900 selenium/standalone-firefox:latest


# 2. create the remote driver
### 'window power shell': ipconfig
### home  ip  : 192.168.1.154
### office ip : 192.168.32.108

#### for chrome
remDr <- remoteDriver(remoteServerAddr = "10.129.244.205", port = 4445L, browserName = "chrome")

#### for firefox
remDr <- remoteDriver(remoteServerAddr = "192.168.199.119", port = 4445L, browserName = "firefox")

# 
remDr <- RSelenium::remoteDriver(remoteServerAddr = "localhost",
                                 port = 4445L,
                                 browserName = "chrome")
remDr$open()

driver <- rsDriver(browser=c("firefox"), port = 4445L)

remDr <- driver[["client"]]

remDr$maxWindowSize()

# quit and release process
remDr$closeServer()
remDr$close()
rm(remDr)
rm(driver)
gc()


# scrape website====
## navigate to the target website
url_tar <- "https://data.eastmoney.com/zjlx/detail.html"
remDr$navigate(url_tar)

## get the page source
page_source <- remDr$getPageSource()[[1]]
tbl_read <- read_html(page_source) %>%
  html_table()

## create unique column names
h1 <- names(tbl_read[[1]])
h2 <- unname(unlist(tbl_read[[1]][1,]))
col_names <-tibble(h1 = h1, h2=h2) %>%
  mutate(cols = ifelse(h1 == h2, h1, paste(h1, h2, sep = "_"))) %>%
  mutate(cols = str_replace_all(cols, "今日", "")) %>%
  pull(cols)

## extract the target table
tbl_tar <- tbl_read[[2]] %>%
  # remove the first row
  .[-1,] %>%
  # set the column names
  setNames(col_names) %>%
  as_tibble()

## find the total page number
### css selector pattern of the second last <a> descent of <div class="pagebox">

css_tar <- "#dataview > div.dataview-pagination.tablepager > div.pagerbox > a:nth-last-of-type(2)"
elm <- remDr$findElement(using = "css selector", value = css_tar)
page_total <- elm$getElementText() %>%
  as.numeric()

## find the page fill box and input the page number
css_tar <- "#gotopageindex"
elm <- remDr$findElement(using = "css selector", value = css_tar)
## clean the input box
elm$clearElement()
## input the page number of 1
elm$sendKeysToElement(list(as.character(1)))
## click the submit button
css_tar <- "#dataview > div.dataview-pagination.tablepager > div.gotopage > form > input.btn"
elm <- remDr$findElement(using = "css selector", value = css_tar)
elm$clickElement()

## loop through the pages====
tbl_tar <- tibble()
for (i in 1:page_total) {
  ## get the page source
  page_source <- remDr$getPageSource()[[1]]
  tbl_read <- read_html(page_source) %>%
    html_table()
  
  ## extract the target table
  tbl_tar <- bind_rows(tbl_tar, tbl_read[[2]] %>%
                         # remove the first row
                         .[-1,] %>%
                         # set the column names
                         setNames(col_names) %>%
                         as_tibble())
  
  ## find the page fill box and input the page number
  css_tar <- "#gotopageindex"
  elm <- remDr$findElement(using = "css selector", value = css_tar)
  ## clean the input box
  elm$clearElement()
  ## input the page number
  elm$sendKeysToElement(list(as.character(i + 1)))
  ## click the submit button
  css_tar <- "#dataview > div.dataview-pagination.tablepager > div.gotopage > form > input.btn"
  elm <- remDr$findElement(using = "css selector", value = css_tar)
  elm$clickElement()
  Sys.sleep(0.5)
  cat("Page ", i, " is done.\n")
}

## close the driver====
remDr$closeServer()
remDr$close()
rm(remDr)
gc()


## add date column
## you can specify a custom date
date_tar <- as.Date("2024-12-19")
## or default as today's date
date_tar <- Sys.Date()

tbl_tar <- tbl_tar %>%
  # add a column of the target date
  add_column(date = date_tar, .before = 1)

## save csv file to local directory `data/`====
## create the file name appending with the date of today
file_name <- glue("funds-flow-{date_tar}.csv")
write_csv(tbl_tar, here::here("data", file_name))

