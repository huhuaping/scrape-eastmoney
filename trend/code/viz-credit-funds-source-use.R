# 金融机构人民币信贷收支表数据分析及可视化
# 准备R包----

require(tidyverse)
require(openxlsx)
require(here)
require(glue)
require(lubridate)
require(ggplot2)
require(fs)
library(zoo)
library(mgsub)

# 读取历年数据----

dir_data <- here("trend", "data", "credit-funds-source-use")

list_data <- fs::dir_ls(dir_data, pattern = "raw-\\d{4}\\.xlsx")

# 采用循环语法读取历年数据

data_combined <- tibble()
for (file_data in list_data) {
    # 读取数据
    data_read <- openxlsx::read.xlsx(file_data, sheet = 1, colNames = TRUE)
    data_combined <- bind_rows(data_combined, data_read)
}

# 数据清洗----

level_1 <- c("一、", "二、", "三、", "四、", "五、", "六、", "七、", "八、", "九、", "十、", "总计")
rpl_level_1 <- c("l1.1", "l1.2", "l1.3", "l1.4", "l1.5", "l1.6", "l1.7", "l1.8", "l1.9", "l1.10", "l1.0")
level_2 <- c("（一）", "（二）", "（三）", "（四）", "（五）", "（六）", "（七）", "（八）", "（九）", "（十）")
rpl_level_2 <- c("l2.1", "l2.2", "l2.3", "l2.4", "l2.5", "l2.6", "l2.7", "l2.8", "l2.9", "l2.10")
level_3 <- c("1\\.", "2\\.", "3\\.", "4\\.", "5\\.", "6\\.", "7\\.", "8\\.", "9\\.", "10\\.")
rpl_level_3 <- c("l3.1", "l3.2", "l3.3", "l3.4", "l3.5", "l3.6", "l3.7", "l3.8", "l3.9", "l3.10")
level_4 <- c("（1）", "（2）", "（3）", "（4）", "（5）", "（6）", "（7）", "（8）", "（9）", "（10）")
rpl_level_4 <- c("l4.1", "l4.2", "l4.3", "l4.4", "l4.5", "l4.6", "l4.7", "l4.8", "l4.9", "l4.10")
level_5 <- c("a", "b", "c", "d", "e")
rpl_level_5 <- c("l5.1", "l5.2", "l5.3", "l5.4", "l5.5")
rpl_level_all <- paste0(rpl_level_1, rpl_level_2, rpl_level_3, rpl_level_4, rpl_level_5, sep = "|")

ptn_level_1 <- paste(level_1, collapse = "|")
ptn_level_2 <- paste(level_2, collapse = "|")
ptn_level_3 <- paste(level_3, collapse = "|")
ptn_level_4 <- paste(level_4, collapse = "|")
ptn_level_5 <- paste(level_5, collapse = "|")
ptn_level_all <- paste(ptn_level_1, ptn_level_2, ptn_level_3, ptn_level_4, ptn_level_5, sep = "|")

data_clean <- data_combined %>%
    # filter(year == 2018) %>%
    # item列中的子类别识别符，利用识别符创建四个子级别列
    # 各级别子类别依次是：
    # 一级类别识别符：一、二、三、四、五、六、七、八、九、十、总计
    # 二级类别识别符：（一）、（二）、（三）、（四）、（五）、（六）、（七）、（八）、（九）、（十）
    # 三级类别识别符：1. 2. 3. 4. 5. 6. 7. 8. 9. 10.
    # 四级类别识别符：(1) (2) (3) (4) (5) (6) (7) (8) (9) (10)
    # 将item中的子类别符号全部提取出来
    # 将所有类别信息存放在item_all列中
    mutate(item_all = sapply(str_extract_all(item, ptn_level_all), function(x) if (length(x) > 0) x[1] else NA)) %>%
    # item中存在"总计"字样，将其类别替换为"总计"
    mutate(item_all = ifelse(str_detect(item, "总计"), "总计", item_all)) %>%
    # 对于item_all列中，如果连续多行单元格出现空白值，则将其依次向下填充为a b c d
    # 最终获得五级类别识别符：a b c d e
    group_by(grp = cumsum(!is.na(item_all))) %>%
    mutate(item_all = ifelse(is.na(item_all),
        letters[1:n()],
        item_all
    )) %>%
    ungroup() %>%
    select(-grp) %>%
    # 整合为item_level列
    mutate(
        item_level = mgsub::mgsub(
            item_all,
            c(level_1, level_2, level_3, level_4, level_5),
            c(rpl_level_1, rpl_level_2, rpl_level_3, rpl_level_4, rpl_level_5)
        )
    ) %>%
    # 根据item_level列，创捷item_level1、item_level2、item_level3、item_level4、level5列，
    mutate(
        item_level1 = ifelse(str_detect(item_level, "l1"), str_extract(item_level, "\\d{1}$"), NA),
        item_level2 = ifelse(str_detect(item_level, "l2"), str_extract(item_level, "\\d{1}$"), NA),
        item_level3 = ifelse(str_detect(item_level, "l3"), str_extract(item_level, "\\d{1}$"), NA),
        item_level4 = ifelse(str_detect(item_level, "l4"), str_extract(item_level, "\\d{1}$"), NA),
        item_level5 = ifelse(str_detect(item_level, "l5"), str_extract(item_level, "\\d{1}$"), NA)
    ) %>%
    # 向下填充item_level1列
    fill(item_level1, .direction = "down") %>%
    # 创建item_combined列，创建1和二级类别识别符的组合
    mutate(
        item_combined = case_when(
            is.na(item_level2) & is.na(item_level3) & is.na(item_level4) & is.na(item_level5) ~ item_level1,
            is.na(item_level3) & is.na(item_level4) & is.na(item_level5) ~ str_c(item_level1, item_level2, sep = "_"),
            is.na(item_level4) & is.na(item_level5) ~ str_c(item_level1, item_level2, item_level3, sep = "_"),
            is.na(item_level5) ~ str_c(item_level1, item_level2, item_level3, item_level4, sep = "_"),
            # other case as NA
            TRUE ~ NA_character_
        )
    ) %>%
    # 向下填充item_combined列
    fill(item_combined, .direction = "down") %>%
    # 创建item_combined列，创建1、2和三级类别识别符的组合
    mutate(
        item_combined = case_when(
            is.na(item_level3) & is.na(item_level4) & is.na(item_level5) ~ item_combined,
            is.na(item_level4) & is.na(item_level5) ~ str_c(item_combined, item_level3, sep = "_"),
            is.na(item_level5) ~ str_c(item_combined, item_level3, item_level4, sep = "_"),
            TRUE ~ NA_character_
        )
    ) %>%
    # 向下填充item_combined列
    fill(item_combined, .direction = "down") %>%
    # 创建item_combined列，创建1、2、3和四级类别识别符的组合
    mutate(
        item_combined = case_when(
            is.na(item_level4) & is.na(item_level5) ~ item_combined,
            is.na(item_level5) ~ str_c(item_combined, item_level4, sep = "_"),
            TRUE ~ NA_character_
        )
    ) %>%
    # 向下填充item_combined列
    fill(item_combined, .direction = "down") %>%
    # 创建item_combined列，创建1、2、3、4和五级类别识别符的组合
    mutate(
        item_combined = ifelse(is.na(item_level5), item_combined, str_c(item_combined, item_level5, sep = "_"))
    ) %>%
    select(-item_level1, -item_level2, -item_level3, -item_level4, -item_level5) %>%
    # 获取item标签，创建列item_label
    mutate(item_label = str_extract(item_level, "\\d{1}$")) %>%
    # 根据type类，创建类别英文列
    mutate(type_eng = case_when(
        type == "来源方项目" ~ "source",
        type == "运用方项目" ~ "use",
        TRUE ~ NA_character_
    )) %>%
    # 清理item列，创建item_name列，删除全部子类别识别符
    mutate(item_name = str_remove_all(item, ptn_level_all)) %>%
    # 清理item_name列，清除前后空格
    mutate(item_name = str_trim(item_name)) %>%
    # 清理item_name列，
    # 注意item_name列中可能存在其他汉字字符例如"（"、"）"等
    mutate(item_name = str_replace_all(item_name, "（|）", "")) %>%
    # 按item_name列字符串中的任意一个汉字后的第一个空格进行拆分，
    # 只拆分为两列，并分别命名为item_name_chn和 item_name_eng列
    mutate(
        item_name_chn = str_extract(item_name, "^[\\u4e00-\\u9fa5]+"),
        item_name_eng = str_remove(item_name, "^[\\u4e00-\\u9fa5]+\\s*")
    ) %>%
    select(-item_name, -item, -item_level) %>%
    select(year, type, type_eng, item_combined, item_label, item_all, item_name_chn, item_name_eng, everything())


# View(data_clean)

# 分析运用方项目，按主体进行分析----
## 主体包括：住户贷款、非金融企业及机关团体贷款
## 贷款周期分为：短期贷款、中长期贷款
## 变量编码对应关系为：
## - 住户贷款：1_1_1
## - 住户贷款-短期贷款：1_1_1_1_1
## - 住户贷款-短期贷款-消费贷款：1_1_1_1_1
## - 住户贷款-短期贷款-经营贷款：1_1_1_1_5
## - 住户贷款-中长期贷款：1_1_1_2
## - 住户贷款-中长期贷款-消费贷款：1_1_1_2_1
## - 住户贷款-中长期贷款-经营贷款：1_1_1_2_5

vars_list <- list(
    "1_1_1" = "住户贷款",
    "1_1_1_1" = "住户贷款-短期贷款",
    "1_1_1_1_1" = "住户贷款-短期贷款-消费贷款",
    "1_1_1_1_5" = "住户贷款-短期贷款-经营贷款",
    "1_1_1_2" = "住户贷款-中长期贷款",
    "1_1_1_2_1" = "住户贷款-中长期贷款-消费贷款",
    "1_1_1_2_5" = "住户贷款-中长期贷款-经营贷款"
)

vars_tar <- names(vars_list)

df_household <- data_clean %>%
    filter(type == "运用方项目") %>%
    filter(item_combined %in% vars_tar) # %>%

View(df_household)

# 按year分组，并计算每组数据中，item_combined列中每个值的占比
df_check <- df_household %>%
    group_by(year) %>%
    summarise(
        n = n()
    ) %>%
    ungroup()

View(df_check)

# 家庭住户贷款月度数据可视化----

## ggplot2制图数据准备----

df_household_long <- df_household %>%
    select(year, item_combined, item_name_chn, item_name_eng, month_01:month_12) %>%
    pivot_longer(
        cols = month_01:month_12,
        names_to = "month",
        values_to = "value"
    ) %>%
    select(year, month, everything()) %>%
    mutate(
        month = str_replace(month, "month_", ""),
        year = as.numeric(year),
        # month = as.numeric(month),
        ymd = as.Date(str_c(year, "-", month, "-01"))
    )

View(df_household_long)

## 短期贷款----

### 短期贷款数据集
vars_short <- c("1_1_1_1", "1_1_1_1_1", "1_1_1_1_5")
df_household_short <- df_household_long %>%
    filter(item_combined %in% vars_short)

### 绘制area graph图，对三个变量在ymd列上进行绘制
### 横轴是ymd列，纵轴是value列
### 绘制为一张图, 并设置area颜色，体现透视和遮挡关系
### 横轴主刻度线为年份，主刻度标签为年份，线形为实线；次刻度线为月份，次刻度标签为月份，线形为虚线
p_short <- df_household_short %>%
    # 确保value列为数值类型，并移除NA值
    mutate(value = as.numeric(as.character(value))) %>%
    filter(!is.na(value)) %>%
    ggplot(aes(x = ymd, y = value, fill = item_name_chn)) +
    geom_area(position = "stack", alpha = 0.8) +
    scale_fill_brewer(palette = "Set2") +
    # 设置横轴主刻度（年份）和次刻度（月份）
    scale_x_date(
        date_breaks = "1 year",
        date_labels = "%Y年",
        minor_breaks = "1 month",
        expand = c(0.01, 0)
    ) +
    scale_y_continuous(
        labels = scales::comma_format(scale = 1 / 10000, suffix = "万亿元"),
        expand = c(0, 0)
    ) +
    labs(
        title = "住户短期贷款余额变化趋势",
        subtitle = "按贷款用途分类",
        x = "年份",
        y = "贷款余额",
        fill = "贷款类型"
    ) +
    theme_minimal() +
    theme(
        plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
        plot.subtitle = element_text(size = 12, hjust = 0.5),
        axis.title = element_text(size = 12),
        # 主刻度标签样式
        axis.text.x = element_text(
            angle = 45,
            hjust = 1,
            size = 10,
            face = "bold"
        ),
        # 次刻度标签样式
        axis.text.x.top = element_text(
            angle = 45,
            hjust = 0,
            size = 8,
            color = "gray50"
        ),
        # 图例位置和样式
        legend.position = "right",
        legend.title = element_text(size = 10, face = "bold"),
        legend.text = element_text(size = 9),
        legend.margin = margin(l = 10),
        # 主网格线样式
        panel.grid.major = element_line(
            color = "#1922cb",
            linetype = "solid",
            linewidth = 0.75
        ),
        # 次网格线样式
        panel.grid.minor = element_line(
            color = "#e841be",
            linetype = "dashed",
            linewidth = 0.5
        ),
        plot.margin = margin(t = 20, r = 20, b = 40, l = 20) # 增加底部边距以容纳月份标签
    ) +
    # 添加季度末月标签
    annotate(
        "text",
        x = seq(min(df_household_short$ymd), max(df_household_short$ymd), by = "3 months"),
        y = -Inf, # 将标签位置固定在图表底部
        label = format(seq(min(df_household_short$ymd), max(df_household_short$ymd), by = "3 months"), "%m月"),
        angle = 45,
        hjust = 1,
        vjust = 2,
        size = 3,
        color = "#e841be" # 使用与次网格线相同的颜色
    )

# 保存图表
ggsave(
    filename = here("trend", "images", "household_short_term_loans.png"),
    plot = p_short,
    width = 12,
    height = 8,
    dpi = 300
)

## 中长期贷款----
vars_long <- c("1_1_1_2", "1_1_1_2_1", "1_1_1_2_5")
df_household_long_term <- df_household_long %>%
    filter(item_combined %in% vars_long)

### 绘制area graph图，对三个变量在ymd列上进行绘制
p_long <- df_household_long_term %>%
    # 确保value列为数值类型
    mutate(value = as.numeric(value)) %>%
    ggplot(aes(x = ymd, y = value, fill = item_name_chn)) +
    geom_area(position = "stack", alpha = 0.8) +
    scale_fill_brewer(palette = "Set2") +
    scale_x_date(date_breaks = "1 year", date_labels = "%Y年") +
    scale_y_continuous(
        labels = scales::comma_format(scale = 1 / 10000, suffix = "万亿元"),
        expand = c(0, 0)
    ) +
    labs(
        title = "住户中长期贷款余额变化趋势",
        subtitle = "按贷款用途分类",
        x = "年份",
        y = "贷款余额",
        fill = "贷款类型"
    ) +
    theme_minimal() +
    theme(
        plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
        plot.subtitle = element_text(size = 12, hjust = 0.5),
        axis.title = element_text(size = 12),
        axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "bottom",
        legend.title = element_text(size = 10),
        legend.text = element_text(size = 9),
        panel.grid.major = element_line(color = "gray90"),
        panel.grid.minor = element_blank(),
        plot.margin = margin(t = 20, r = 20, b = 20, l = 20)
    )

# 保存图表
ggsave(
    filename = here("trend", "output", "household_long_term_loans.png"),
    plot = p_long,
    width = 12,
    height = 8,
    dpi = 300
)
