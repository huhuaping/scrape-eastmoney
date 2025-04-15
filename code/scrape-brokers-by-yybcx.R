# 爬取券商营业部基本信息(营业部查询)
## 特别说明：本方法爬取的营业部信息相对更少！而`scrape-brokers-by-yybph.R`爬取的营业部信息要更全更多。
## 目标网站：https://data.eastmoney.com/stock/yybcx.html
## 目标数据集：券商营业部基本信息`brokers`
## 目标字段：
# 1. 营业部名称(name)
# 2. 营业部代码(id)
# 3. 营业部所属地域(region)
## 爬取策略：
# 1. 打开目标网站
# 2. 获取所有营业部信息
# 3. 提取目标字段
# 4. 保存为brokers.rds文件
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
url_firms <- "https://data.eastmoney.com/stock/yybcx.html"
remDr$navigate(url_firms)
## 可能弹出广告，需要手动关闭

## 目标节点，不是表格要素，而是列表要素
css_target <- "body > div:nth-child(2) > div.main-content > div.content > div.company-container.yyb-box > ul >li > a"
## 获取目标节点
nodes <- remDr$findElements(using = "css", css_target)
## 获取目标节点文本
text <- sapply(nodes, function(x) x$getElementText())
## 获取目标节点href
href <- sapply(nodes, function(x) x$getElementAttribute("href"))

## 将文本和href合并为数据框
df <- data.frame(text = unlist(text), href = unlist(href))
# View(df)

# 6. 爬取券商公司的具体营业部信息 =====

## 遍历df的每一行，获取营业部信息=====
i <- 23
i <- 1
# 设置一个空数据框
df_broker_all <- data.frame()
for (i in 1:nrow(df)) {
  cat(paste0("开始爬取第", i, "行，共", nrow(df), "行\n"))
  ## 获取当前行的href
  firm_href <- df$href[i]
  ## 获取当前行的文本
  firm_name <- df$text[i]
  ## 打开券商公司网址
  remDr$navigate(firm_href)
  ## 可能弹出广告，需要手动关闭
  Sys.sleep(0.5)

  ## 部分公司营业部数量较多，需要分页爬取
  ## 获得最大页码数
  ## 获得最大页码数的节点
  ## 注意：如果页面没有分页，则没有该节点
  css_target <- "div.pagerbox > a"
  ## 获取最大页码数的节点
  nodes <- remDr$findElements(using = "css", css_target)
  ## 获取最大页码数的节点文本
  text_page <- sapply(nodes, function(x) x$getElementText()) %>%
    unlist() %>%
    str_extract("\\d+") 
  ## 如果最大页码数为空，则没有分页
  if (length(text_page) == 0) {
    total_page <- 1
  } else {
    total_page <- max(as.numeric(text_page), na.rm = TRUE)
  }
  # 打印最大页码数
  cat(paste0(firm_name, "的最大页码数为：", total_page, "\n"))

  ## 初始化一个空数据框
  df_broker <- data.frame()
  ## 遍历最大页码数 
  page <- 1
  for (page in 1:total_page) {
    ## 如果page为1，则不需要点击分页按钮
      if (page >1) {
        ## 页码输入框
        css_target <- "#gotopageindex"
        nodes <- remDr$findElements(using = "css", css_target)
        ## 清除页码输入框内容
        nodes[[1]]$clearElement()
        ## 输入页码
        nodes[[1]]$sendKeysToElement(list(as.character(page)))
        ## 等待页面加载
      Sys.sleep(0.5)
      ## 点击确定按钮
      css_target <- "#dataview > div.dataview-pagination.tablepager > div.gotopage > form > input.btn"
        nodes <- remDr$findElements(using = "css", css_target)
        nodes[[1]]$clickElement()
        ## 等待页面加载
        Sys.sleep(1)
      }

      ## 保存doc
      doc <- remDr$getPageSource()[[1]]
      ## 将doc转换为html
      html <- read_html(doc)
      ## 目标节点，是table要素
      ## 获取营业部代码
      css_target <- "#dataview > div.dataview-center > div.dataview-body > table > tbody > tr > td > a"
      id <- html %>% html_nodes(css_target) %>% html_attr("href") %>% str_extract("\\d+")
      ## 获取营业部表格内容
      css_target <- "#dataview > div.dataview-center > div.dataview-body > table"
      table <- html %>% html_nodes(css_target) %>% html_table() %>%
        .[[1]]
      
      ## 表格可能为空
      if (table$`序号`[1] == "暂无数据") {
        table <- data.frame(
            index = 1,
            name = firm_name,
            region = NA,
            id = NA
        )
      } else {
        ## 清理表格
        table <- table %>%
          # 只保留前三列
          select(1, 2, 3)  %>%
          rename(index = 1, name = 2, region = 3) %>%
          # 添加营业部代码列
          mutate(id = id)
      }

      ## 将表格内容转换为数据框
      df_broker <- rbind(df_broker, table)
    
    cat(paste0("爬取完成第", page, "页，共", total_page, "页\n"))
  }
  ## 将当前行的数据框与总数据框合并
  df_broker_all <- rbind(df_broker_all, df_broker)
  ## 打印当前行的数据框
  cat(paste0("爬取完成第", i, "行，共", nrow(df), "行\n"))
  ## 随机等待1-2秒
  Sys.sleep(runif(1, 1, 2))
}

# View(df_broker_all)

## 保存数据====
saveRDS(df_broker_all, here("data", "brokers.rds"))
write_csv(df_broker_all, here("data", "brokers.csv"))


# 7. 关闭driver====
remDr$closeServer()
remDr$close()
rm(remDr)
gc()


