# scrape loan surplus data from pbc.gov.cn
# 金融机构人民币信贷收支表
# Summary of Sources And Uses of Credit Funds of Financial Institutions（in RMB）
# 单位：亿元
# Unit:100 Million Yuan

# R packages----
require(openxlsx)
require("rvest")
# require("xml2")
require("httr")
require("stringr")
require("tidyverse")
require("tidyselect")
require("here")
require(glue)

# download xlsx file from pbc.gov.cn----

## 下载历年数据----

url_page_list <- list(
    "2025" = "http://www.pbc.gov.cn/diaochatongjisi/116219/116319/5570903/5570888/index.html",
    "2024" = "http://www.pbc.gov.cn/diaochatongjisi/116219/116319/5225358/5225362/index.html",
    "2023" = "http://www.pbc.gov.cn/diaochatongjisi/116219/116319/4780803/4780807/index.html",
    "2022" = "http://www.pbc.gov.cn/diaochatongjisi/116219/116319/4458449/4458453/index.html",
    "2021" = "http://www.pbc.gov.cn/diaochatongjisi/116219/116319/4184109/4184113/index.html",
    "2020" = "http://www.pbc.gov.cn/diaochatongjisi/116219/116319/3959050/3959053/index.html",
    "2019" = "http://www.pbc.gov.cn/diaochatongjisi/116219/116319/3750274/3750286/index.html",
    "2018" = "http://www.pbc.gov.cn/diaochatongjisi/116219/116319/3471721/3471763/index.html"
)

## 循环语句开始读取html table 并保存为xlsx文件
year <- names(url_page_list)[1]
year_now <- year(Sys.Date())
for (year in names(url_page_list)) {
    url_page <- url_page_list[[year]]
    # 获取html href
    html_url <- read_html(url_page) %>%
        html_node("#con > table.border_nr > tbody > tr > td > table:nth-child(5) > tbody > tr > td:nth-child(2) > a") %>%
        html_attr("href")
    url_tar <- glue("http://www.pbc.gov.cn{html_url}")
    date_pub <- str_extract(html_url, "(?<=/\\d{4}/\\d{2}/)(\\d{4}\\d{2}\\d{2})") %>%
        as.Date(format = "%Y%m%d")
    year_pub <- year(date_pub)

    # 抓取html table
    table_raw <- read_html(url_tar) %>%
        html_table() %>%
        .[[1]] %>%
        as_tibble()


    # 清洗数据表
    table_clean <- table_raw %>%
        # 去除空白行
        filter(X1 != "") %>%
        # 识别表头
        mutate(is_table_head = if_else(str_detect(X1, "^Summary of Sources"), "yes", NA)) %>%
        tidyr::fill(is_table_head, .direction = "up") %>%
        # # 去除表头
        # filter(is_table_head != "yes") %>%
        # select(-is_table_head) %>%
        # 识别表尾
        mutate(is_table_end = if_else(str_detect(X1, "^注："), "yes", NA)) %>%
        tidyr::fill(is_table_end, .direction = "down") %>%
        mutate(is_table_head = if_else(is.na(is_table_head), "no", is_table_head)) %>%
        mutate(is_table_end = if_else(is.na(is_table_end), "no", is_table_end)) %>%
        dplyr::filter(is_table_head == "no" & is_table_end == "no") %>%
        select(-is_table_head, -is_table_end) %>%
        # 去除全部为NA的列
        select(where(~ !all(is.na(.))))

    # 重命名列名
    names_var <- c("item", paste0("month_", str_pad(1:12, 2, "left", "0")))
    table_clean <- table_clean %>%
        rename_all(~names_var) %>%
        # 识别“资金来源总计”
        mutate(type = if_else(str_detect(item, "^资金来源总计"), "来源方项目", NA)) %>%
        tidyr::fill(type, .direction = "up") %>%
        # 识别两个类别：“来源方项目”和“运用方项目”
        mutate(type = if_else(is.na(type), "运用方项目", type)) %>%
        filter(!str_detect(item, "^来源方项目|运用方项目")) %>%
        # 去掉第一行
        slice(-1) %>%
        # 添加年份列
        mutate(year = year) %>%
        select(year, type, item, everything())

    # View(table_clean)

    # 将数据保存为xlsx文件
    file_tar <- glue("raw-credit-funds-source-use-{year}.xlsx")
    path_tar <- here("trend", "data", file_tar)
    # 如果年份等于当前年份，则可以多次保存并覆盖同名xlsx文件
    # 如果年份小于当前年份，则只保存一次
    if (year == year_now) {
        write.xlsx(table_clean, path_tar)
        cat(
            glue("文件发布日期 {date_pub}"),
            glue("文件已保存到 {path_tar}"),
            sep = "\n"
        )
    } else {
        if (!file.exists(path_tar)) {
            write.xlsx(table_clean, path_tar)
            cat(
                glue("文件发布日期 {date_pub}"),
                glue("文件已保存到 {path_tar}"),
                sep = "\n"
            )
        }
    }
    cat(
        glue("年度 {year} 数据已抓取并完成本地保存"),
        sep = "\n"
    )
}
