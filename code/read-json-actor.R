# 准备R环境----
library(here)
library(tidyverse)
library(magrittr)
library(jsonlite)

# 人工添加游资json信息----
## （1）通过同花顺桌面版游资席位识别。
##     可以人工手动整理得到的游资席位，并按照操作特征进行了归类。
## （2）通过东方财富龙虎榜营业部排行。
##     可以获取到游资的营业部信息（营业部名称、营业部代码）。
## （3）以东方财富的营业部代码为基准，将游资营业部信息与游资席位信息进行匹配。
##     匹配成功后，将游资席位信息与游资营业部信息进行合并，整理为json格式。


# 读取游资json列表----
## 游资列表以json格式存放
json_file <- here("data", "large-actor.json")
## 读取json文件，并转换为标准tibble格式
df_actor <- jsonlite::fromJSON(json_file) %>%
  # 将嵌套的JSON结构展开为tibble
  map_df(function(x) {
    tibble(
      portrait = x$portrait,
      # 展开actors列表
      bind_rows(x$actors) %>%
        # 展开department列表为name和id
        mutate(
          # 将department中的name和id分别提取出来
          department_info = map(department, bind_rows)
        ) %>%
        unnest(department_info)
    )
  }, .id = "category") %>%
  select(-department)


# 导出数据----
## 导出为rds格式
write_rds(df_actor, here("data", "large-actor.rds"))
