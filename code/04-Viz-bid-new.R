# analyze the bid list data set
# and visualize the results

# I want to analyze stock bid list data set and visualize withdraw buy or sell hands on the horizontal time.
## can you show me template plot can visualize this data set?

# prepare packages====
library("here")
library("tidyverse")
library(ggplot2)
library(lubridate)
library(plotly)
library(glue)
library(rvest)
library(jsonlite)
library(janitor)

# load basic json file====
##read the json file which contains the stock id, name and date
source(here("code/00-prepare-json.R"))
id_tar <- "600693"
market_tar <- tbl_json %>%
  filter(`stock_id` == id_tar) %>%
  select(market) %>%
  unique() %>%
  pull()

# prepare stock daily data use ====
library(usethis)
library("quantmod")
## R包`quantmod` 接口数据
### tiingo api acount and api key
### 进行环境变量设置
#usethis::edit_r_environ(scope = "project")
### 制订默认的key
setDefaults(getSymbols.tiingo, api.key = Sys.getenv("TIINGO_API_KEY"))

### 使用`tiingo`源，`yahooo finance`国内不可用


# obtain the limit up or limit down price of the target date====
## Custom function to calculate limit up and limit down prices
## also get the stock data within the date range.
## Example usage of the function


date_target <- "2024-12-27"
date_start <- "2024-12-01"
tbl_query <- query_stock(ticker = id_tar, date_tar = date_target, date_from = date_start)
price_range <- c(tbl_query$Limit_Down_Price, tbl_query$Limit_Up_Price)


query_stock <- function(
    ticker ="600693", date_tar = "2024-12-27",
    date_from ="2024-12-01", date_to = Sys.Date(),
    limit_up_pct = 0.10, limit_down_pct = 0.10) {
  # check the package `quantmod`, `lubridate`, `dplyr`, `janitor`
  if (!requireNamespace(c("quantmod","janitor"), quietly = TRUE)) {
    stop("The 'quantmod', 'janitor' package is required.")
  }
  
  # Get historical stock data
  stock_data <- getSymbols(ticker, src = "tiingo", from = date_from, to = date_to, auto.assign = FALSE)
  
  # Convert date to Date object
  date_tar <- as.Date(date_tar)
  
  # Check if data exists for the specified date
  if (!(date_tar) %in% index(stock_data)) {
    stop("No trading data available for the specified date.")
  }
  
  # Get previous day's closing price
  previous_day <- index(stock_data[index(stock_data) < date_tar])[length(index(stock_data[index(stock_data) < date_tar]))]
  
  if (is.na(previous_day)) {
    stop("No previous trading day found.")
  }
  
  previous_close <- stock_data[previous_day, paste0(ticker,".Close")]
  
  # Calculate limit up and limit down prices
  limit_up_price <- previous_close * (1 + limit_up_pct)
  limit_down_price <- previous_close * (1 - limit_down_pct)
  
  # obtain the OHLC price of the target date 
  ## open, high, low, close, volume
  ## create a new data frame
  names_new <- c("date", "open", "high", "low", "close", "volume", "adj_close")
  tbl_stock <- stock_data %>%
    as.data.frame() %>%
    rownames_to_column(var = "date") %>%
    #mutate(date= as.character(date)) %>%
    rename_all(., ~names_new) 
  ## latest close price
  close_latest <- tbl_stock %>%
    filter(date == max(date)) %>%
    select(c("date","close")) %>%
    t() %>%
    as_tibble() %>%
    janitor::row_to_names(row_number = 1)  %>%
    as.list()
    
  tbl_tar <- tbl_stock %>%
    dplyr::filter(date == as.character(date_tar))
  open <- tbl_tar$open
  high <- tbl_tar$high
  low <-  tbl_tar$low
  close <-  tbl_tar$close
  volume <-  tbl_tar$volume
  
  cat("Target date:", as.character(date_tar),"星期",lubridate::wday(ymd(date_tar),label=TRUE, week_start = 1), "\n")
  cat("maxium date:", max(tbl_stock$date),"星期",lubridate::wday(ymd(max(tbl_stock$date)),label=TRUE, week_start = 1), "\n")
  
  
  # Return results as a list
  return(
    list(
      previous_day = previous_day,
      Previous_Close = as.vector(previous_close),
      now_date = date,
      open = open,
      high = high,
      low = low,
      close = close,
      volume = volume,
      Limit_Up_Price = as.vector(limit_up_price),
      Limit_Down_Price = as.vector(limit_down_price),
      close_latest = close_latest,
      tbl_stock = tbl_stock
    )
  )
}



# obtain latest stock information ====
## from sohu finance website

## try remote server and browser====
library(rvest)
library(RSelenium)


## you should run docker container first
## use RSelenium to get the html content
# local server
## should download the chrome driver first at 
## url: https://developer.chrome.com/docs/chromedriver/downloads?hl=zh-cn
## and pay attention to the version of chrome and chromedriver
## download to local path and click to run, it will open a local server 
## and expose the port `9515`


## try local server and browser====

# my pc is windows 11, and chrome version 131.0.6778.205
# Specify the path to ChromeDriver
chrome_driver_path <- "D:/mysoft/chromedriver.exe"
rD <- rsDriver(
  browser="chrome",
  chromever = "114.0.5735.90",
  port = 9515L) 
# Create a remote driver specifying the executable path
remDr <- remoteDriver(
  remoteServerAddr = "localhost",
  port = 9515L#, 
  #browserName = "chrome"
)


# Start the remote driver
remDr$open()

# get the stock information from sohu finance website
(url_tar <- glue::glue("https://q.stock.sohu.com/cn/600693/index.shtml"))
remDr$navigate(url_tar)
Sys.sleep(1)
html_tar <- remDr$getPageSource()[[1]]

# Start RSelenium with specified ChromeDriver path
rD <- rsDriver(browser = "firefox", port = 9515L)
remDr <- rD[["client"]]

remDr <- remoteDriver(
  remoteServerAddr = "localhost", 
  port = 9515L, browserName = "firefox")

remDr$open()

## try local chrome driver
## break point of Selenium server
## see url: <https://www.selenium.dev/blog/2023/headless-is-going-away/>
chrome_driver_path <- "D:/mysoft/chromedriver.exe"
driver <- rsDriver(browser = "chrome", port = 9515L, 
                   chromever = NULL, # Set to NULL if specifying path directly
                   extraCapabilities = list("chromeOptions" = list(
                     binary = chrome_driver_path,
                     args = c('--headless=new') # Optional: run in `headless=new` mode
                   )))

## try firefox driver ====
driver <- rsDriver(browser = "firefox", port = 4444L)

# Extract the client from the driver
remDr <- driver[["client"]]

tbl_info <- html_tar %>%
  read_html() %>%
  html_nodes(xpath = xpath_tar) %>%
  html_table() %>%
  .[[1]] %>%
  as_tibble()


# Specify the path to the downloaded selenium-server-standalone jar
selenium_server_path <- "D:/mysoft/selenium-chromium-driver-4.27.0.jar"

# Start RSelenium with the specified server path
rD <- rsDriver(browser = "chrome", port = 4567L, 
               check = FALSE, 
               version = selenium_server_path)
## If you have already downloaded geckodriver.exe or chromedriver.exe, 
## ensure they are in your system's PATH 
## or specify their exact paths in your R script:
rD <- rsDriver(browser = "firefox", 
               port = 4567L, 
               geckover = "D:/mysoft/geckodriver.exe", 
               check = FALSE)

# Alternative Download Method:====
#   If you continue to face issues with downloading from Google APIs, 
### consider using an alternative method for managing WebDriver binaries, 
### such as using the wdman package.
#renv::install("wdman")
library(wdman)
# Start a standalone Selenium server with wdman
selServ <- selenium(port = 4567L)

# Then use RSelenium
rD <- remoteDriver(remoteServerAddr = "localhost", port = 4567L, browserName = "chrome")
rD$open()

