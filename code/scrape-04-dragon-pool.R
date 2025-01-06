# 使用RSelenium+docker抓取东方财富网站的龙虎榜股票池数据
# 数据来源：东方财富
# 数据集：日度龙虎榜股票池数据集`dragon-pool`
# 抓取流程：抓取每日龙虎榜个股名单。此为常规操作，可以根据需要每周抓取一次。
#  - 进入龙虎榜数据中心<https://data.eastmoney.com/stock/tradedetail.html>
#  - 设定日期。点击**自定义区间**
#  - 按日期抓取表格
#  - 保存到`data/dragon-pool/`目录下

# 1. 加载必要的包
library(RSelenium)
library(tidyverse)
library(rvest)
library(dplyr)
library(stringr)
library(lubridate)
library(here)
library(glue)
library(jsonlite)

# 2. 启动docker
## run docker service and container
## you should run and start docker desktop first.
## then run code in 'window power shell': docker run --name chrome -v /dev/shm:/dev/shm -d -p 4445:4444 -p 5901:5900 selenium/standalone-chrome-debug:latest

### for firefox: docker run --name firefox -v /dev/shm:/dev/shm -d -p 4445:4444 -p 5901:5900 selenium/standalone-firefox:latest


# 3. 启动tvnViewer。非必须，仅用于查看抓取过程
## 输入端口密码


# 2. 连接selenium====
remDr <- remoteDriver(remoteServerAddr = "localhost",
                       port = 4445L,
                       browserName = "chrome")

remDr$open()
remDr$maxWindowSize()

# 3. 读取法定节假日json数据====
## 3.1 读取json数据
file_json <- here("data", "holiday-new.json")
# Read the JSON file
json_data <- fromJSON(file_json)

## step3: Convert to tibble and rename columns
names_new <- c("date","stock_id", "market", "stock_name")
tbl_holiday <- json_data %>%
  imap_dfr(~ tibble(
    year = .y,
    holiday = .x
  )
  ) %>%
  unnest(holiday) %>%
  unnest(date) %>%
  rename("holiday" = "name") 

# 4. 设定目标日期====
date_start <- ymd("2024-01-01")
date_end <- ymd("2024-09-30")
#list_holiday <- NA
tbl_date <- tibble(date = as.Date(date_start:date_end)) %>%
  mutate(
    wday = wday(date, week_start = 1),
    wday = factor(wday, levels = 1:7, labels = c("周一", "周二", "周三", "周四", "周五", "周六", "周日"))
  ) %>%
  mutate(
    is.weekend = wday %in% c("周六", "周日"),
    is.holiday = date %in% as.Date(tbl_holiday$date)
  ) %>%
  filter(!is.weekend, !is.holiday)
#target_date <- "2023-12-14"

# 5. 抓取龙虎榜日度榜单数据====

## loop all dates
k <- 29
for (k in 124:nrow(tbl_date)){
  cat(glue("begin {k} of total {nrow(tbl_date)} \n"))
  
  ## 5.0 设定目标日期====
  target_date <- as.character(tbl_date$date[k])
  ## 5.1 进入龙虎榜数据中心====
  url_tar <- "https://data.eastmoney.com/stock/tradedetail.html"
  remDr$navigate(url_tar)
  Sys.sleep(2)
  
  ## 5.2 设定日期====
  ## 点击自定义区间
  #css_tar <- "li.tagRange"
  css_tar <- "body > div:nth-child(2) > div.main-content > div.content > div:nth-child(3) > ul > li.tagRange"
  elm <- remDr$findElement(using = "css", value = css_tar)
  Sys.sleep(2)
  elm$clickElement()
  
  
  ## 5.3 开始日期选择器====
  ## we have to select the date range in the calendar with the date picker.
  ## the start date picker has css `input.date-search-start`， and has "onclick=WdatePicker()" attribute.
  css_tar <- "input.date-search-start"
  elm <- remDr$findElement(using = "css", value = css_tar)
  ## send target date to the date picker
  elm$clickElement()
  Sys.sleep(1)
  # clear the date picker
  remDr$executeScript("arguments[0].value = '';", list(elm))
  Sys.sleep(1) # larger if the network is slow
  # send new date to the date picker
  elm$sendKeysToElement(list(target_date))
  ## get the date
  date1 <- elm$getElementAttribute("value") %>% .[[1]]
  
  ## 5.4 结束日期选择器====
  ## the end date picker has css `input.date-search-end`， and has "onclick=WdatePicker()" attribute.
  css_tar <- "input.date-search-end"
  elm <- remDr$findElement(using = "css", value = css_tar)
  ## send target date to the date picker
  elm$clickElement()
  Sys.sleep(2)
  # clear the date picker
  remDr$executeScript("arguments[0].value = '';", list(elm))
  Sys.sleep(2) # larger if the network is slow
  # send new date to the date picker
  elm$sendKeysToElement(list(target_date))
  ## get the date
  date2 <- elm$getElementAttribute("value") %>% .[[1]]
  
  ## stop if the date is not consistent
  if (date1 != target_date | date2 != target_date) {
    cat(glue("Date start {date1} or data end {date2} is not correct.\n"))
    break
  }
  
  ## 5.5 点击查询====
  css_tar <- "div.search_btn"
  elm <- remDr$findElement(using = "css", value = css_tar)
  elm$clickElement()
  Sys.sleep(1)
  
  ## 5.6 抓取表格====
  ## source page
  page <- remDr$getPageSource()[[1]]
  Sys.sleep(1)
  page <- read_html(page)
  
  ## 5.7 获取总页码数====
  ## the html source is: 
  css_tar <- "div.pagerbox >a"
  ## check element exist
  if (length(html_nodes(page, css_tar)) == 0) {
    cat("No data for ", target_date, "\n")
    warning("No paganation for ", target_date, "\n")
    page_total <- 999
  } else {
    page_total <- html_nodes(page, css_tar) %>% 
      html_text() %>% # may be NA for "下一页"
      as.numeric() %>% 
      na.omit() %>% 
      max()
    cat("Total pages: ", page_total, "\n")
  }
  
  # 6. 循环抓取所有页数据====
  ## 6.1 循环抓取所有页数据
  ## initialize
  tbl_all <- tibble()
  
  ## reset total pages
  if (page_total == 999) {
    total_pages <- 1
  } else {
    total_pages <- page_total
  }
  
  #i <- 3
  for (i in 1:total_pages) {
    # case of multiple page
    if (page_total != 999) {
      ## fill the page index
      css_tar <- "#gotopageindex"
      elm <- remDr$findElement(using = "css", value = css_tar)
      ## clear the page index
      elm$clearElement()
      ## send new page index
      elm$sendKeysToElement(list(as.character(i)))
      Sys.sleep(1)
      ## click the ok button
      css_tar <- "input.btn"
      elm <- remDr$findElement(using = "css", value = css_tar)
      elm$clickElement()
      Sys.sleep(1)
    }
    
    ## read page
    page <- remDr$getPageSource()[[1]]
    Sys.sleep(1)
    page <- read_html(page)
    
    ## read table
    tbl_read <- page %>% 
      html_table() %>%
      .[[2]]
    ## append to all
    tbl_all <- rbind(tbl_all, tbl_read)
    cat("Page ", i, " is done.\n")
  }
  
  # 7. 保存数据====
  ## save csv files to "data/dragon-pool/"
  dir_out <- here("data", "dragon-pool")
  if (!dir.exists(dir_out)) {
    dir.create(dir_out, recursive = TRUE)
  }
  ## files name
  (file_out <- glue("{dir_out}/{target_date}.csv"))
  write_csv(tbl_all, file_out)
  Sys.sleep(1)
  cat("File ", file_out, " is saved.\n")
  
  ## date scrape is finished
  cat("Date ", target_date, " is done.\n")
  
  ## Force garbage collection (optional, but good to do)
  #gc()
  # close browser and to the blank page
  remDr$navigate("about:blank")
}

## close the driver====
# remDr$closeServer()
# remDr$close()
# rm(remDr)
# gc()


