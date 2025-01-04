# scrape stocks which price hit limits up or limits down from webpage 
## by using docker + RSelenium + tvnviewer

# R packages----
require(openxlsx)
require("rvest")
#require("xml2")
require("httr")
require("stringr")
require("tidyverse")
require("tidyselect")
require("here")
library("RSelenium")
library(glue)

# start Rselenum----
#part 01 start docker + RSelenium
# 1. run docker service and container
#### you should run and start docker desktop first.
#### then run code in 'window power shell': docker run --name chrome -v /dev/shm:/dev/shm -d -p 4445:4444 -p 5901:5900 selenium/standalone-chrome-debug:latest

### for firefox: docker run --name firefox -v /dev/shm:/dev/shm -d -p 4445:4444 -p 5901:5900 selenium/standalone-firefox:latest


# 2. create the remote driver
### 'window power shell': ipconfig
### home  ip  : 192.168.1.154
### office ip : 192.168.32.108

# #### for chrome
# remDr <- remoteDriver(remoteServerAddr = "10.129.244.205", port = 4445L, browserName = "chrome")
# 
# #### for firefox
# remDr <- remoteDriver(remoteServerAddr = "192.168.199.119", port = 4445L, browserName = "firefox")

# local server
remDr <- RSelenium::remoteDriver(remoteServerAddr = "localhost",
                                 port = 4445L,
                                 browserName = "chrome")
remDr$open()

# driver <- rsDriver(browser=c("firefox"), port = 4445L)
# 
# remDr <- driver[["client"]]

remDr$maxWindowSize()

# scrape limits up stocks from webpage====
## create a list of stock type pools
type <- list(
  ztgc="ztgc", # 涨停股池
  zrzt="zrzt", # 昨日涨停股池
  zbgc="zbgc", # 炸板股池
  dtgc="dtgc" # 跌停股池
  )

## select the target date
#target_date <- ymd("2024-12-27")
(target_date <- Sys.Date())
# loop through the list of stock type pools
# i <- 4
for (i in 1:length(type)) {
  type_tar <- type[i]
  ## create a list of urls
  url_tar <- glue("https://quote.eastmoney.com/ztb/detail#type={type_tar}")
  Sys.sleep(1)
  ## navigate to the target website
  remDr$navigate(url_tar)
  
  css_tar <- "#beginDate" 
  ### select the date input box and input the date
  ### and the css selector of the date input box has html attribute: <input type="text" id="beginDate" class="t-input" readonly="" aria-label="Use the arrow keys to pick a date">
  
  # 3. Locate the date input box element
  date_input_element <- remDr$findElement(using = "id", value = "beginDate")
  
  # 4. Locate the date picker icon
  date_icon_element <- remDr$findElement(using = "css", value = ".dateicon.icon.icon_timearr")
  
  # 5.  Click the date picker icon to open it
  date_icon_element$clickElement()
  
  # Select the target day in the calendar.
  day_xpath <- paste0('//button[@data-pika-day="', day(target_date), '" and @data-pika-month="', month(target_date)-1, '" and @data-pika-year="', year(target_date), '"]')
  day_element <- remDr$findElement(using = 'xpath', value=day_xpath)
  day_element$clickElement() # Select day
  
  ## click the footer bar of "click to load more"
  css_tar <-"#zrzttable > tfoot > tr>td"
  xpath_tar <- "//*[@id='zrzttable']/tfoot/tr/td"
  elm <- remDr$findElement(using = "xpath", value = xpath_tar)
  # elm$getElementAttribute("text")[1] 
  elm$clickElement()
  Sys.sleep(1)
  ## get the page source
  page_source <- remDr$getPageSource()[[1]]
  tbl_read <- read_html(page_source) %>%
    html_table() %>%
    .[[1]] %>%
    # remove firt row
    .[-1,] %>%
    # remove the last row
    .[-nrow(.),] %>%
    as_tibble() %>%
    ## add target date
    add_column(date = target_date, .before = 1)
  
  ## save csv to local directory `data/limits-pool/
  (file_out <- here("data", "limits-pool",type_tar, glue("{target_date}.csv")))
  write_csv(tbl_read, file_out)
  
  cat("The file ", file_out, " is saved.\n")
}

## close the driver====
remDr$closeServer()
remDr$close()
rm(remDr)
gc()
