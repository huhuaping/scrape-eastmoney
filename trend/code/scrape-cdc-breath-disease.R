# 近期全国急性呼吸道传染病在中国、印度、东南亚等地区爆发，我想知道呼吸道样本病原体核酸检测阳性率（百分数）监测指标的每周发展情况。
## 中国疾病预防控制中心 <https://www.chinacdc.cn/jksj/jksj04_14275/> 提供了全国急性呼吸道传染病哨点监测情况数据 (周报告数据)。
## 请帮我整理这些每周报告信息表并保存为csv文件。
## 然后使用R编程语言绘制出2024年以来每周的新型冠状病毒和流感病毒在这个监测指标上的每周趋势图。

# Load necessary libraries----
library(tidyverse)
library(lubridate)
library(openxlsx)
library(rvest)
library(stringr)
library(here)
library(glue)
library(mgsub)

# 获得每周报告网页的url列表----
## Define the URL of the CDC Pathogen page
url_index <- list(
    url_pg1 = "https://www.chinacdc.cn/jksj/jksj04_14275/index.html",
    url_pg2 = "https://www.chinacdc.cn/jksj/jksj04_14275/index_1.html"
)

## 循环获取每个url page下的href
links_all <- tibble(
    url = character(),
    link = character(),
    title = character()
)
for (url in url_index) {
    ## 获得全部周报告的页面href链接
    page <- read_html(url)
    ### 获得周报告的节点
    nodes <- page %>%
        html_nodes("ul.xw_list > li")

    ### 提取全部href链接
    links <- nodes %>%
        html_nodes("a") %>%
        html_attr("href")
    ### 提取周报告的标题
    ### 正常标题格式如：2025年第12周
    ### 异常标题格式如：2025年4月，第14周—18周
    titles <- nodes %>%
        html_nodes("a") %>%
        html_text()

    links_all <- links_all %>%
        bind_rows(tibble(url = url, link = links, title = titles))


    cat("url: ", url, "\n")
}

links_href <- links_all %>%
    # 可能存在不一致的标题
    ## 例如：2025年4月，第14周—18周
    mutate(
        link = str_replace_all(link, "^\\.", "https://www.chinacdc.cn/jksj/jksj04_14275"),
        year_week = str_extract(title, "(?<=（)(.+)(?=）)"),
        year = str_extract(title, "(\\d{4})(?=年)"),
        week = str_extract_all(title, "(\\d{1,2})(?=周)"),
        pub_date = str_extract(link, "(?<=t)(\\d{4}\\d{1,2}\\d{1,2})")
    ) %>%
    mutate(
        week = map(week, ~ seq(min(.x), max(.x)))
    )

## 保存links_href
## 备份links_href，用于log记录
date_scrape <- Sys.Date()
file_name <- str_c("backup_", "tbl_links_href_", date_scrape, ".xlsx")
path_target <- here("trend", "data", "cdc-breath-disease", file_name)

# write.xlsx(links_href, file = path_target)


# 循环获取每个周报告的网页内容----
i <- 21
for (i in 1:nrow(links_href)) {
    ## 获取该page包括的年份、发布日期和周数
    href_target <- links_href$link[i]
    year <- links_href$year[i]
    pub_date <- links_href$pub_date[i]
    weeks <- unlist(links_href$week[i])
    num_week <- length(weeks)
    week_range <- ifelse(
        num_week > 1,
        str_c(min(weeks), "-", max(weeks)),
        min(weeks)
    )

    cat(glue("正在爬取第{i}个页面，{year}年第{week_range}周报告"), sep = "\n")


    ## 设施文件和保存路径
    file_name <- str_c(year, "_week", week_range, "_pub", pub_date, ".xlsx")
    dir_target <- here("trend", "data", "cdc-breath-disease")
    file_path <- here("trend", "data", "cdc-breath-disease", file_name)

    ## 如果文件存在，则跳过
    if (file.exists(file_path)) {
        cat(glue("文件{file_name}已存在，跳过爬取"), sep = "\n")
    } else {
        ## 只有目标文件不存在才进行数据爬取
        ## 获取网页pageUTF-8
        page <- read_html(href_target, encoding = "UTF-8")
        ## 获取网页page中的表1
        tbl_raw <- page %>%
            html_table() %>%
            .[[1]] %>%
            as.data.frame()

        ## 清洗数据----
        ### 去除全部为NA的数据列
        tbl_clean <- tbl_raw %>%
            select(where(~ !all(is.na(.))))

        ### 把第一行和第二行进行合并，获得列变量名
        week_raw <- unlist(str_extract_all(tbl_clean[2, ], "(\\d{1,2})(?=周)"))
        names_raw <- rep(c("emergency", "hospital"), num_week)
        vars_raw <- str_c(names_raw, week_raw, sep = "_")
        vars_all <- c("desease", vars_raw)

        ### 数据集只保留第二行包含“病原体”和“第xx周”的列
        tbl_clean <- tbl_clean[, str_detect(tbl_clean[2, ], "病原体|第\\d{1,2}周")]
        # View(tbl_clean)
        ### 进一步清洗数据并进行变形
        tbl_tidy <- tbl_clean %>%
            ## 删除第一行和第二行
            slice(-c(1, 2)) %>%
            ## 设置列变量名
            setNames(vars_all) %>%
            ## 变形为long dataset
            pivot_longer(
                cols = -desease,
                names_to = c("type_week"),
                values_to = "value"
            ) %>%
            ## 拆分type_week列
            separate(
                type_week,
                into = c("type", "week"),
                sep = "_"
            ) %>%
            ## 添加年份和发布日期
            mutate(
                year = year,
                pub_date = pub_date
            ) %>%
            select(
                year,
                pub_date,
                desease,
                type,
                week,
                value
            )

        ## 保存数据
        write.xlsx(tbl_tidy, file = file.path(dir_target, file_name))
        cat(glue("数据集保存成功"), sep = "\n")
    } # 结束if
    cat(glue("第{i}个页面，{year}年第{week_range}周报告爬取完成"), sep = "\n")
} # 结束for循环
