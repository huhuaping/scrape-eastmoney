# 代码说明：
## 对cdc-breath-disease数据进行可视化
## 重点关注两类呼吸道疾病：
## 1. 新型冠状病毒
## 2. 流感病毒
## 主要变量为病原体核酸检测阳性率（%），具体包括两类：
## 1. 门诊检测阳性率（%）， emergency ratio
## 2. 住院检测阳性率（%）， hospital ratio


# Load necessary libraries ----
library(tidyverse)
library(lubridate)
library(scales) # For number formatting
library(here)
library(ggplot2)
library(glue)
library(here)
library(fs)
library(openxlsx)

# 读取xlsx数据 ----
## 文件名样式：2025_week9_pub20250306.xlsx
dir_target <- here("trend", "data", "cdc-breath-disease")
## 获得文件名起始字符为"\\d{4}_week"的xlsx文件
files_target <- fs::dir_ls(dir_target)
files_filter <- str_subset(files_target, "\\d{4}_week")

## 批量读取xlsx文件
tbl_raw <- map_df(files_filter, read.xlsx) %>%
    select(year, pub_date, week, type, desease, value) %>%
    arrange(year, pub_date, week, type)


# 分析数据并进行可视化绘图----
## 过滤数据，只保留流感病毒和新型冠状病毒
tbl_breath <- tbl_raw %>%
    filter(desease %in% c("流感病毒", "新型冠状病毒")) %>%
    mutate(
        week = as.numeric(week), # 确保week为数值型
        value = as.numeric(value) # 确保value为数值型
    ) %>%
    mutate(
        year_week = paste0(year, "-", str_pad(week, 2, "left", "0"))
    )

## 核对数据量 group by year, week, type，
tbl_check <- tbl_breath %>%
    group_by(year, week, type) %>%
    summarise(n = n())

## 绘制facet类型的折线图
## 横轴为年份-周次（year_week），纵轴为阳性率（value）
## 分面变量为病原体（desease）：流感病毒和新型冠状病毒
## 每个分面都有两条折线，分别表示门诊检测阳性率（emergency）和住院检测阳性率（hospital）

p_breath <- tbl_breath %>%
    ggplot(aes(x = year_week, y = value, color = type, group = type)) +
    geom_line(linewidth = 1.2) +
    geom_point(size = 2) +
    facet_wrap(~desease, ncol = 1, scales = "free_y") +
    scale_color_manual(
        values = c("emergency" = "#E41A1C", "hospital" = "#377EB8"),
        labels = c("emergency" = "门诊检测", "hospital" = "住院检测"),
        name = "检测类型"
    ) +
    scale_x_discrete(
        guide = guide_axis(angle = 45)
    ) +
    scale_y_continuous(
        labels = function(x) paste0(x, "%"),
        expand = expansion(mult = c(0, 0.1))
    ) +
    labs(
        title = "2024-2025年呼吸道病原体核酸检测阳性率趋势",
        x = "年份-周次",
        y = "阳性率"
    ) +
    theme_minimal() +
    theme(
        legend.position = "right",
        legend.title = element_text(face = "bold", size = 11),
        legend.text = element_text(size = 10),
        legend.key.size = unit(1.2, "cm"),
        legend.box.background = element_rect(color = "grey80", fill = "white", linewidth = 0.5),
        legend.box.margin = margin(6, 6, 6, 6),
        panel.grid.minor = element_blank(),
        strip.text = element_text(size = 12, face = "bold"),
        axis.title = element_text(face = "bold"),
        plot.title = element_text(hjust = 0.5, face = "bold"),
        axis.text.x = element_text(angle = 45, hjust = 5)
    ) +
    guides(color = guide_legend(override.aes = list(linewidth = 2)))

# 显示图表
# print(p_breath)

# 保存图表
file_path <- here("trend", "images", "cdc_breath_disease_trend.png")
## 如果文件已经存在，则跳过保存
if (!file.exists(file_path)) {
    ggsave(
        file_path,
        p_breath,
        width = 10, height = 8, dpi = 300
    )
}

# 绘制交互式plotly图表
library(plotly)
p_breath_plotly <-
    ggplotly(p_breath) %>%
    layout(
        title = list(
            text = "2024-2025年呼吸道病原体核酸检测阳性率趋势",
            font = list(size = 18)
        ),
        xaxis = list(
            # title = "年份-周次",
            tickangle = 45
        ),
        # yaxis = list(title = "阳性率"),
        hoverlabel = list(
            bgcolor = "white",
            font = list(size = 12)
        ),
        hovermode = "closest",
        legend = list(
            orientation = "v",
            x = 1.05,
            xanchor = "left",
            y = 0.5,
            yanchor = "middle"
        )
    ) %>%
    config(
        displayModeBar = TRUE,
        modeBarButtonsToRemove = list(
            "sendDataToCloud", "autoScale2d", "resetScale2d",
            "hoverClosestCartesian", "hoverCompareCartesian"
        ),
        displaylogo = FALSE,
        locale = "zh-CN"
    )

# 显示交互式图表
# p_breath_plotly
