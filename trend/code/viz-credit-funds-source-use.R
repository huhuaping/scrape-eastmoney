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
library(plotly)
library(scales)


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

# View(df_household)

# 按year分组，并计算每组数据中，item_combined列中每个值的占比
df_check <- df_household %>%
    group_by(year) %>%
    summarise(
        n = n()
    ) %>%
    ungroup()

# View(df_check)

# 家庭住户贷款月度数据可视化----

## 制图long数据准备----

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
    ) %>%
    mutate(value = as.numeric(value))

# View(df_household_long)

## 短期贷款----

### 短期贷款数据集
vars_short <- c("1_1_1_1", "1_1_1_1_1", "1_1_1_1_5")
df_household_short <- df_household_long %>%
    filter(item_combined %in% vars_short)

### 绘制area graph图----
### 对三个变量在ymd列上进行绘制
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
    # 设置横轴标签为年月格式
    scale_x_date(
        date_breaks = "2 months", # 每2个月显示一个刻度
        date_labels = "%Y%m", # 设置为年月格式，如"202503"
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
            size = 9,
            face = "bold"
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
        # 禁用次网格线
        panel.grid.minor = element_blank(),
        plot.margin = margin(t = 20, r = 20, b = 40, l = 20) # 增加底部边距
    )

# 保存图表
file_path <- here("trend", "images", "household_short_term_loans.png")
## 如果文件已经存在，则跳过保存
if (!file.exists(file_path)) {
    ggsave(
        filename = file_path,
        plot = p_short,
        width = 12,
        height = 8,
        dpi = 300
    )
    cat("图表已保存到", file_path, "\n")
}

### 使用plotly包绘制交互式图表----
p_short_plotly <- plotly::plot_ly(
    df_household_short,
    x = ~ymd,
    y = ~ value / 10000,
    color = ~item_name_chn,
    type = "scatter", # 改为scatter类型
    mode = "none", # 不显示点和线
    fill = "tonexty", # 填充到下一个y值
    stackgroup = "one", # 设置堆叠模式
    hoverinfo = "text",
    text = ~ paste(
        format(ymd, "%Y年%m月"),
        "<br>",
        item_name_chn,
        "<br>",
        "金额: ", scales::comma(value / 10000, accuracy = 0.01), "万亿元"
    )
) %>%
    plotly::layout(
        title = list(
            text = "住户短期贷款余额变化趋势<br><sup>按贷款用途分类</sup>",
            font = list(size = 16, family = "sans-serif")
        ),
        height = 700, # 设置图表高度
        xaxis = list(
            # 移除轴标题
            title = "",
            type = "date",
            # 设置日期格式为年-月
            tickformat = "%Y-%m",
            tickangle = 45,
            tickmode = "auto",
            nticks = 12, # 控制显示标签数量，避免过于拥挤
            tickfont = list(size = 9, color = "black"),
            # 移除次标签相关设置
            minor = list(
                showgrid = FALSE,
                showticklabels = FALSE
            ),
            gridcolor = "#1922cb",
            gridwidth = 1,
            showline = TRUE,
            linecolor = "#1922cb",
            linewidth = 1,
            # 添加年度范围选择器
            rangeselector = list(
                buttons = list(
                    list(
                        count = 1,
                        label = "最近1年",
                        step = "year",
                        stepmode = "backward"
                    ),
                    list(
                        count = 2,
                        label = "最近2年",
                        step = "year",
                        stepmode = "backward"
                    ),
                    list(
                        count = 3,
                        label = "最近3年",
                        step = "year",
                        stepmode = "backward"
                    ),
                    list(
                        count = 5,
                        label = "最近5年",
                        step = "year",
                        stepmode = "backward"
                    ),
                    list(step = "all", label = "全部")
                ),
                # 将选择器放在滑块上方
                x = 0.1, # 靠左放置
                y = -0.2, # 放在滑块下方
                font = list(size = 9),
                bgcolor = "#FCFCFC",
                bordercolor = "#1922cb",
                borderwidth = 1,
                # 水平排列按钮
                direction = "right",
                yanchor = "top"
            ),
            # 添加年度范围滑块
            rangeslider = list(
                type = "date",
                thickness = 0.05
            )
        ),
        yaxis = list(
            title = "贷款余额(万亿元)",
            tickformat = ",", # 使用逗号分隔数字
            hoverformat = ".2f", # 悬停时显示两位小数
            tickprefix = "", # 前缀
            ticksuffix = "万亿元", # 后缀
            gridcolor = "#1922cb",
            gridwidth = 0.75
        ),
        hoverlabel = list(
            bgcolor = "white",
            font = list(size = 12)
        ),
        # 设置颜色与ggplot配色类似
        colorway = RColorBrewer::brewer.pal(3, "Set2"),
        legend = list(
            title = list(text = "贷款类型"),
            # 将图例放置在右侧
            orientation = "v", # 垂直方向排列
            xanchor = "left",
            yanchor = "top",
            x = 1.05, # 放在图表右侧
            y = 1
        ),
        margin = list(t = 80, r = 150, b = 100, l = 80) # 增加右侧边距以容纳图例和选择器
    ) %>%
    plotly::config(
        displayModeBar = TRUE,
        modeBarButtonsToRemove = c(
            "zoomIn2d", "zoomOut2d", "autoScale2d", "resetScale2d",
            "hoverClosestCartesian", "hoverCompareCartesian"
        ),
        displaylogo = FALSE,
        locale = "zh-CN"
    )

### 展示plotly图表
# p_short_plotly

## 中长期贷款----
### 中长期贷款数据集----
### 变量编码对应关系为：
### - 住户贷款-中长期贷款：1_1_1_2
### - 住户贷款-中长期贷款-消费贷款：1_1_1_2_1
### - 住户贷款-中长期贷款-经营贷款：1_1_1_2_5
vars_long <- c("1_1_1_2", "1_1_1_2_1", "1_1_1_2_5")
df_household_long_term <- df_household_long %>%
    filter(item_combined %in% vars_long)

### 绘制area graph图，对三个变量在ymd列上进行绘制
p_long <- df_household_long_term %>%
    # 确保value列为数值类型，并移除NA值
    mutate(value = as.numeric(as.character(value))) %>%
    filter(!is.na(value)) %>%
    ggplot(aes(x = ymd, y = value, fill = item_name_chn)) +
    geom_area(position = "stack", alpha = 0.8) +
    scale_fill_brewer(palette = "Set2") +
    # 设置横轴标签为年月格式
    scale_x_date(
        date_breaks = "2 months", # 每2个月显示一个刻度
        date_labels = "%Y%m", # 设置为年月格式，如"202503"
        expand = c(0.01, 0)
    ) +
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
        # 主刻度标签样式
        axis.text.x = element_text(
            angle = 45,
            hjust = 1,
            size = 9,
            face = "bold"
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
        # 禁用次网格线
        panel.grid.minor = element_blank(),
        plot.margin = margin(t = 20, r = 20, b = 40, l = 20) # 增加底部边距
    )

# 保存图表
file_path <- here("trend", "images", "household_long_term_loans.png")
## 如果文件已经存在，则跳过保存
if (!file.exists(file_path)) {
    ggsave(
        filename = file_path,
        plot = p_long,
        width = 12,
        height = 8,
        dpi = 300
    )
    cat("图表已保存到", file_path, "\n")
}


### 使用plotly包绘制交互式图表----

p_long_plotly <- plotly::plot_ly(
    df_household_long_term,
    x = ~ymd,
    y = ~ value / 10000,
    color = ~item_name_chn,
    type = "scatter", # 改为scatter类型
    mode = "none", # 不显示点和线
    fill = "tonexty", # 填充到下一个y值
    stackgroup = "one", # 设置堆叠模式
    hoverinfo = "text",
    text = ~ paste(
        format(ymd, "%Y年%m月"),
        "<br>",
        item_name_chn,
        "<br>",
        "金额: ", scales::comma(value / 10000, accuracy = 0.01), "万亿元"
    )
) %>%
    plotly::layout(
        title = list(
            text = "住户中长期贷款余额变化趋势<br><sup>按贷款用途分类</sup>",
            font = list(size = 16, family = "sans-serif")
        ),
        height = 700, # 设置图表高度
        xaxis = list(
            # 移除轴标题
            title = "",
            type = "date",
            # 设置日期格式为年-月
            tickformat = "%Y-%m",
            tickangle = 45,
            tickmode = "auto",
            nticks = 12, # 控制显示标签数量，避免过于拥挤
            tickfont = list(size = 9, color = "black"),
            # 移除次标签相关设置
            minor = list(
                showgrid = FALSE,
                showticklabels = FALSE
            ),
            gridcolor = "#1922cb",
            gridwidth = 1,
            showline = TRUE,
            linecolor = "#1922cb",
            linewidth = 1,
            # 添加年度范围选择器
            rangeselector = list(
                buttons = list(
                    list(
                        count = 1,
                        label = "最近1年",
                        step = "year",
                        stepmode = "backward"
                    ),
                    list(
                        count = 2,
                        label = "最近2年",
                        step = "year",
                        stepmode = "backward"
                    ),
                    list(
                        count = 3,
                        label = "最近3年",
                        step = "year",
                        stepmode = "backward"
                    ),
                    list(
                        count = 5,
                        label = "最近5年",
                        step = "year",
                        stepmode = "backward"
                    ),
                    list(step = "all", label = "全部")
                ),
                # 将选择器放在滑块上方
                x = 0.1, # 靠左放置
                y = -0.2, # 放在滑块下方
                font = list(size = 9),
                bgcolor = "#FCFCFC",
                bordercolor = "#1922cb",
                borderwidth = 1,
                # 水平排列按钮
                direction = "right",
                yanchor = "top"
            ),
            # 添加年度范围滑块
            rangeslider = list(
                type = "date",
                thickness = 0.05
            )
        ),
        yaxis = list(
            title = "贷款余额(万亿元)",
            tickformat = ",", # 使用逗号分隔数字
            hoverformat = ".2f", # 悬停时显示两位小数
            tickprefix = "", # 前缀
            ticksuffix = "万亿元", # 后缀
            gridcolor = "#1922cb",
            gridwidth = 0.75
        ),
        hoverlabel = list(
            bgcolor = "white",
            font = list(size = 12)
        ),
        # 设置颜色与ggplot配色类似
        colorway = RColorBrewer::brewer.pal(3, "Set2"),
        legend = list(
            title = list(text = "贷款类型"),
            # 将图例放置在右侧
            orientation = "v", # 垂直方向排列
            xanchor = "left",
            yanchor = "top",
            x = 1.05, # 放在图表右侧
            y = 1
        ),
        margin = list(t = 80, r = 150, b = 100, l = 80) # 增加右侧边距以容纳图例和选择器
    ) %>%
    plotly::config(
        displayModeBar = TRUE,
        modeBarButtonsToRemove = c(
            "zoomIn2d", "zoomOut2d", "autoScale2d", "resetScale2d",
            "hoverClosestCartesian", "hoverCompareCartesian"
        ),
        displaylogo = FALSE,
        locale = "zh-CN"
    )

### 展示plotly图表
# p_long_plotly


## 住户短期和中长期贷款变化趋势----

### 准备数据集----
### 变量编码对应关系为：
### - 住户贷款-短期贷款：1_1_1_1
### - 住户贷款-中长期贷款：1_1_1_2

df_household_short_long <- df_household_long %>%
    filter(item_combined %in% c("1_1_1_1", "1_1_1_2"))


### 绘制堆叠柱状图----
### 横轴是ymd列，纵轴是value列
### 在堆叠柱形上分别添加短期贷款和中长期贷款的金额（保留整数），注意value除以10000，表示万亿
p_short_long <- df_household_short_long %>%
    # 确保value列为数值类型，并移除NA值
    mutate(value = as.numeric(as.character(value))) %>%
    filter(!is.na(value)) %>%
    # 按年月分组，确保每个年月只有两种贷款类型
    group_by(year, month, item_name_chn) %>%
    summarise(value = sum(value, na.rm = TRUE)) %>%
    ungroup() %>%
    # 创建年月日期
    mutate(ymd = as.Date(paste0(year, "-", month, "-01"))) %>%
    # 按月筛选，每季度末月(3,6,9,12)
    filter(as.numeric(month) %in% c(3, 6, 9, 12)) %>%
    # 按年月排序
    arrange(ymd) %>%
    # 绘制堆叠柱状图
    ggplot(aes(x = ymd, y = value / 10000, fill = item_name_chn)) +
    geom_col(position = "stack", width = 60) + # 调整柱宽
    geom_text(
        aes(
            label = round(value / 10000, 1), # 保留整数
            group = item_name_chn
        ),
        position = position_stack(vjust = 0.5), # 文本位于每个部分的中间
        color = "white",
        fontface = "bold",
        size = 3
    ) +
    scale_fill_brewer(palette = "Set2") +
    # 设置横轴为年月格式
    scale_x_date(
        date_breaks = "1 months", # 每月一个刻度
        date_labels = "%Y%m",
        expand = c(0.01, 0.01)
    ) +
    scale_y_continuous(
        labels = scales::comma_format(suffix = "万亿元"),
        expand = c(0, 0.1) # 上方留出空间显示标签
    ) +
    labs(
        title = "住户短期和中长期贷款余额变化趋势",
        subtitle = "按月统计",
        x = "",
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
            size = 9,
            face = "bold"
        ),
        # 图例位置和样式
        legend.position = "right",
        legend.title = element_text(size = 10, face = "bold"),
        legend.text = element_text(size = 9),
        legend.margin = margin(l = 10),
        # 主网格线样式
        panel.grid.major = element_line(
            color = "#e2e3e9",
            linetype = "solid",
            linewidth = 0.75
        ),
        # 禁用次网格线
        panel.grid.minor = element_blank(),
        plot.margin = margin(t = 20, r = 20, b = 40, l = 20) # 增加底部边距
    )

# 保存图表
file_path <- here("trend", "images", "household_short_long_term_loans.png")
## 如果文件已经存在，则跳过保存
if (!file.exists(file_path)) {
    ggsave(
        filename = file_path,
        plot = p_short_long,
        width = 12,
        height = 8,
        dpi = 300
    )
    cat("图表已保存到", file_path, "\n")
}



### 使用plotly包绘制交互式图表----

p_short_long_plotly <- df_household_short_long %>%
    # 确保value列为数值类型，并移除NA值
    mutate(value = as.numeric(as.character(value))) %>%
    filter(!is.na(value)) %>%
    # 按年月分组，确保每个年月只有两种贷款类型
    group_by(year, month, item_name_chn) %>%
    summarise(value = sum(value, na.rm = TRUE)) %>%
    ungroup() %>%
    # 创建年月日期
    mutate(ymd = as.Date(paste0(year, "-", month, "-01"))) %>%
    # 按年月排序
    arrange(ymd) %>%
    # 创建交互式柱状图
    plotly::plot_ly(
        x = ~ymd,
        y = ~ value / 10000, # 转换为万亿单位
        color = ~item_name_chn,
        type = "bar", # 柱状图类型
        text = ~ paste(
            format(ymd, "%Y年%m月"),
            "<br>",
            item_name_chn,
            "<br>",
            "金额: ", scales::comma(value / 10000, accuracy = 0.1), "万亿元"
        ),
        hoverinfo = "text"
    ) %>%
    plotly::layout(
        title = list(
            text = "住户短期和中长期贷款余额变化趋势<br><sup>按月统计</sup>",
            font = list(size = 16, family = "sans-serif")
        ),
        height = 700, # 设置图表高度
        xaxis = list(
            # 移除轴标题
            title = "",
            type = "date",
            # 设置日期格式为年-月
            tickformat = "%Y%m",
            tickangle = 45,
            tickmode = "auto",
            nticks = 12, # 控制显示标签数量，避免过于拥挤
            tickfont = list(size = 9, color = "black"),
            gridcolor = "#e2e3e9",
            gridwidth = 1,
            showline = TRUE,
            linecolor = "#e2e3e9",
            linewidth = 1,
            # 添加年度范围选择器
            rangeselector = list(
                buttons = list(
                    list(
                        count = 1,
                        label = "最近1年",
                        step = "year",
                        stepmode = "backward"
                    ),
                    list(
                        count = 2,
                        label = "最近2年",
                        step = "year",
                        stepmode = "backward"
                    ),
                    list(
                        count = 3,
                        label = "最近3年",
                        step = "year",
                        stepmode = "backward"
                    ),
                    list(
                        count = 5,
                        label = "最近5年",
                        step = "year",
                        stepmode = "backward"
                    ),
                    list(step = "all", label = "全部")
                ),
                # 将选择器放在滑块上方
                x = 0.1, # 靠左放置
                y = -0.2, # 放在滑块下方
                font = list(size = 9),
                bgcolor = "#FCFCFC",
                bordercolor = "#e2e3e9",
                borderwidth = 1,
                # 水平排列按钮
                direction = "right",
                yanchor = "top"
            ),
            # 添加年度范围滑块
            rangeslider = list(
                type = "date",
                thickness = 0.05
            )
        ),
        yaxis = list(
            title = "贷款余额(万亿元)",
            tickformat = ",", # 使用逗号分隔数字
            hoverformat = ".1f", # 悬停时显示一位小数
            tickprefix = "", # 前缀
            ticksuffix = "万亿元", # 后缀
            gridcolor = "#e2e3e9",
            gridwidth = 0.75
        ),
        barmode = "stack", # 设置为堆叠柱状图
        hoverlabel = list(
            bgcolor = "white",
            font = list(size = 12)
        ),
        # 设置颜色与ggplot配色类似
        colorway = RColorBrewer::brewer.pal(3, "Set2"),
        legend = list(
            title = list(text = "贷款类型"),
            # 将图例放置在右侧
            orientation = "v", # 垂直方向排列
            xanchor = "left",
            yanchor = "top",
            x = 1.05, # 放在图表右侧
            y = 1
        ),
        margin = list(t = 80, r = 150, b = 100, l = 80) # 增加右侧边距以容纳图例和选择器
    ) %>%
    plotly::config(
        displayModeBar = TRUE,
        modeBarButtonsToRemove = c(
            "zoomIn2d", "zoomOut2d", "autoScale2d", "resetScale2d",
            "hoverClosestCartesian", "hoverCompareCartesian"
        ),
        displaylogo = FALSE,
        locale = "zh-CN"
    )

### 展示plotly图表
# p_short_long_plotly
