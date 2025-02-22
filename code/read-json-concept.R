## load stocks list from json file====

## step1: write the new coming stocks to the json file by hand

## step2: read the json file which contains the stock id, name and date
library(jsonlite)
library(tidyverse)
library(here)
file_json <- here("data/concept-hot-new.json")
# Read the JSON file
json_data <- fromJSON(file_json)

## step3: Convert to tibble and rename columns
### 使用 imap_dfr 将嵌套的 JSON 数据扁平化
tbl_json <- json_data %>%
  imap_dfr(~ {
    # 提取 c1 层级
    c1_name <- .y
    .x %>%
      imap_dfr(~ {
        # 提取 c2 层级
        c2_name <- .y
        .x %>%
          imap_dfr(~ {
            # 提取 c3 层级
            c3_name <- .y
            # 提取 stocks 列表
            stocks <- .x$stocks
            if (!is.null(stocks)) {
              stocks %>%
                as_tibble() %>%
                mutate(
                  c1 = c1_name,
                  c2 = c2_name,
                  c3 = c3_name
                )
            } else {
              tibble(
                id = NA_character_,
                market = NA_character_,
                name = NA_character_,
                c1 = c1_name,
                c2 = c2_name,
                c3 = c3_name
              )
            }
          })
      })
  }) %>%
  select(c1, c2, c3, id, market, name)

# 查看转换后的数据
head(tbl_json)
