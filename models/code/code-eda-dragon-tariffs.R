#使用东方财富网站的龙虎榜每日活跃营业部数据（网站爬取数据），
##   分析机构或游资的龙虎榜资金是否发生了结构性变化。我们将主要采用可视化方法，
##  绘制龙虎榜每日活跃营业部资金的变化趋势。

# 准备R环境 ----
library(tidyverse)
library(magrittr)
library(lubridate)
library(ggplot2)
library(plotly)
library(scales)
library(here)
library(glue)
library(jsonlite)

# 读取游资数据 ----
## 游资列表以json格式存放
json_file <- here("data", "large-actor.json")

## 读取json文件，并转换为标准tibble格式
## 读取json文件，并转换为标准tibble格式
df_actor <- jsonlite::fromJSON(json_file) %>%
  # 将嵌套的JSON结构展开为tibble
  map_df(function(x) {
    tibble(
      portrait = x$portrait,
      # 展开actors列表
      bind_rows(x$actors) %>%
        # 将department列表转为字符串
        mutate(department = map(department, paste, collapse = ";"))
    )
  }, .id = "category") %>%
  # 展开department为多行
  separate_rows(department, sep = ";")

## 查看数据
# df_actor %>%
#   head() %>%
#   View()

# 龙虎榜每日活跃营业部数据 ----

## 读取csv数据----
## csv文件名的目标范围
date_start <- lubridate::ymd("2025-01-01")
date_end <- lubridate::ymd("2025-04-13")
date_tar <- seq(date_start, date_end, by = "day")
date_ptn <- paste0(date_tar, collapse = "|")

## 数据文件夹
dir_tar <- here("data", "dragon-dpt")

## 获得目标范围内的csv文件
files_tar <- dir_tar    %>%
  list.files(pattern = "^.*csv$") %>%
  str_subset(date_ptn)

## 读取csv文件
### 同时保留文件名变量
df_tar <- files_tar %>%
  map_df(~read_csv(here("data", "dragon-dpt", .x)))

## 读取csv文件
### 同时保留文件名变量
df_tar <- files_tar %>%
  map(function(x) {
    read_csv(here("data", "dragon-dpt", x)) %>%
      mutate(file_name = x,
             date = str_extract(x, "\\d{4}-\\d{2}-\\d{2}"))
  }) %>%
  bind_rows()

## 查看数据
# df_check <- df_tar %>%
#   head() %>%
#   View()


## 数据清洗 ----
### 变量命名----
names(df_tar)
# "序号,营业部名称,相关,上榜日,买入个股数,卖出个股数,买入总金额(万),卖出总金额(万),总买卖净额(万),买入股票,href_dpt,href_stock,file_name,date"
# 对应创建英文变量名
names_eng <- c(
    "id", "name", "related", "date_raw", 
    "buy_num", "sell_num", "buy_amount",
     "sell_amount", "net_amount", 
     "stocks", "href_dpt", "href_stock", 
     "file_name", "date_file"
     )

## 变量重命名
df_tar <- df_tar %>%
  set_names(names_eng)

### 选择变量----
names_tar <- c(
    "date_file", "name", "buy_amount", "sell_amount", "net_amount",
    "buy_num", "sell_num", "stocks",
    "href_dpt", "href_stock"
    )

df_reduce <- df_tar %>%
  select(all_of(names_tar))

## 查看数据
# df_reduce %>%
#   head() %>%
#   View()
