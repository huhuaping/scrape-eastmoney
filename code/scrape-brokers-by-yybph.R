# 爬取券商营业部基本信息(营业部排行)
## 特别说明：本方法爬取的营业部信息更全！而`scrape-brokers-by-yybcx.R`爬取的营业部信息相对要少很多。
## 目标网站：https://data.eastmoney.com/stock/yybph.html
## 目标数据集：券商营业部基本信息`brokers`
## 目标字段：
# 1. 营业部名称(name)
# 2. 营业部代码(id)
## 爬取策略：
# 1. 打开目标网站
# 2. 获取所有营业部信息
# 3. 提取目标字段
# 4. 保存为brokers-yybph.rds文件
## 爬取技术：
# 1.采用Selenium+docker方式进行爬取



# 1. 加载必要的包====
library(RSelenium)
library(tidyverse)
library(rvest)
library(dplyr)
library(stringr)
library(lubridate)
library(here)
library(glue)
library(jsonlite)

# 2. 启动docker====
## run docker service and container
## you should run and start docker desktop first.
## then run code in 'window power shell': docker run --name chrome2 -v /dev/shm:/dev/shm -d -p 4445:4444 -p 5901:5900 selenium/standalone-chrome-debug:latest

### for firefox: docker run --name firefox -v /dev/shm:/dev/shm -d -p 4445:4444 -p 5901:5900 selenium/standalone-firefox:latest


# 3. 启动tvnViewer。非必须，仅用于查看抓取过程
## 输入端口密码


# 4. 连接selenium====
remDr <- remoteDriver(remoteServerAddr = "localhost",
                       port = 5555L,
                       browserName = "chrome")

remDr$open()
remDr$maxWindowSize()




# 5. 爬取券商公司的网址信息====

## 获取券商公司列表
url_firms <- "https://data.eastmoney.com/stock/yybph.html"
remDr$navigate(url_firms)
## 可能弹出广告，需要手动关闭


## 点击“近一年”按钮

css_target <- "#filter_yybph > li:nth-child(4)"
node_target <- remDr$findElement(using = "css", css_target)
node_target$clickElement()


# 6. 抓取券商营业部基本信息====
## 获得最大分页页码 ====
css_target <- "#dataview > div.dataview-pagination.tablepager > div.pagerbox > a"
nodes_target <- remDr$findElements(using = "css", css_target)
max_page <- nodes_target %>%
    lapply(function(x) x$getElementText()) %>%
    unlist() %>%
    str_extract_all("\\d+") %>%
    as.numeric() %>%
    max(., na.rm = TRUE)

## 循环抓取所有页面的数据 ====
## 初始化数据框
df_brokers_all <- data.frame()

page <- 1
for (page in 1:max_page) {
    ## 如果page大于1，则点击分页按钮
    if (page > 1) {
        css_target <- "#gotopageindex"
        node_target <- remDr$findElement(using = "css", css_target)
        ## 清除输入框内容
        node_target$clearElement()
        ## 输入页码
        node_target$sendKeysToElement(list(as.character(page)))
        ## 等待页面加载
        Sys.sleep(0.5)
        ## 点击确定按钮
        css_target <- "#dataview > div.dataview-pagination.tablepager > div.gotopage > form > input.btn"
        node_target <- remDr$findElement(using = "css", css_target)
        node_target$clickElement()
        ## 等待页面加载
        Sys.sleep(1)
    }

    ## 保存doc
    doc <- remDr$getPageSource()[[1]]    
    css_target <- "#dataview > div.dataview-center > div.dataview-body > table > tbody > tr > td > a"
    ## 获取href，提取id
    id <- doc %>%
        read_html() %>%
        html_nodes(css_target) %>%
        html_attr("href") %>%
        str_extract("\\d+")
    ## 获取name    
    name <- doc %>%
        read_html() %>%
        html_nodes(css_target) %>%
        html_text()
    ## 创建数据框
    df_brokers <- data.frame(
        id = id,
        name = name
    )
    ## 合并数据框
    df_brokers_all <- bind_rows(df_brokers_all, df_brokers)
    cat(paste0("已爬取第", page, "页数据，共", max_page, "页\n"))
}

# View(df_brokers_all)

# 7. 保存数据====

df_output <- df_brokers_all %>%
  arrange(name) %>%
  add_column(
    index = 1:nrow(df_brokers_all),
    .before = "id"
  )
saveRDS(df_output, here("data", "brokers-yybph.rds"))
write_csv(df_output, here("data", "brokers-yybph.csv"))


# 8. 关闭driver====
remDr$closeServer()
remDr$close()
rm(remDr)
gc()
















