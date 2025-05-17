# Load necessary libraries----
library(tidyverse)
library(lubridate)
library(scales) # For number formatting
library(here)
library(ggplot2)
library(glue)
library(here)

# Read the cleaned CSV data with explicit column types
covid_data <- read_csv(
  here("trend", "data", "covid-trend", "cleaned_covid_data.csv"),
  col_types = cols(
    Year = col_integer(),
    Month = col_integer(),
    ConfirmedCases = col_double(),
    CriticalCases = col_double(),
    Date = col_date()
  )
)

# Print data structure and first few rows
# print("Data structure:")
# str(covid_data)
# print("\nFirst few rows:")
# print(head(covid_data))
# View(covid_data)

# Create a subset of data with non-NA ConfirmedCases
confirmed_data <- covid_data %>%
  filter(!is.na(ConfirmedCases))

# print("\nConfirmed cases data:")
# print(confirmed_data)

# Create the plot
p_covid <- ggplot(covid_data, aes(x = Date)) +
  # Critical Cases
  geom_line(aes(y = CriticalCases, colour = "Critical Cases"), linewidth = 1) +
  geom_point(aes(y = CriticalCases, colour = "Critical Cases"), size = 2) +
  # # Confirmed Cases (only where available)
  # geom_text(
  #   data = confirmed_data, # 使用预先过滤的数据
  #   aes(
  #     y = CriticalCases + 2000, # 增加偏移量以避免重叠
  #     colour = "Confirmed Cases (where available)",
  #     label = format(round(ConfirmedCases), big.mark = ",") # 将label移到aes内部
  #   ),
  #   size = 3.5, # 稍微增加字体大小
  #   angle = 45,
  #   hjust = 0,
  #   vjust = -0.5
  # ) +
  scale_y_continuous(
    name = "重症病例",
    labels = scales::comma,
    limits = c(0, max(covid_data$CriticalCases) * 1.1) # 增加y轴上限以容纳文本
    # sec.axis = sec_axis(~., name = "Confirmed Cases (where available)", labels = scales::comma)
  ) +
  scale_x_date(date_breaks = "1 month", date_labels = "%Y-%m") +
  labs(
    title = "中国新冠重症病例月度趋势 (2024年1月-2025年3月)",
    x = "月份",
    y = "重症病例",
    colour = ""
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    # legend.position = "right",
    legend = element_blank(),
    plot.title = element_text(hjust = 0.5)
  ) +
  scale_colour_manual(
    values = c("Critical Cases" = "red"),
    labels = c("Critical Cases" = "重症病例")
  )

# Save the plot
file_path <- here("trend", "images", "covid_trend.png")
## 如果文件已经存在，则跳过保存
if (!file.exists(file_path)) {
  ggsave(
    file_path,
    plot = p_covid,
    width = 12,
    height = 6
  )
  print("Plot has been created and saved successfully.")
}



# plotly交互制图----
library(plotly)

p_covid_plotly <- ggplotly(p_covid, tooltip = c("x", "y", "colour")) %>%
  layout(
    title = list(
      text = "中国新冠重症病例月度趋势 (2024年1月-2025年3月)",
      font = list(size = 18)
    ),
    # legend = list(
    #   orientation = "v",
    #   x = 1.02,
    #   xanchor = "left",
    #   y = 0.5,
    #   yanchor = "middle"
    # ),
    showlegend = FALSE,
    hoverlabel = list(
      bgcolor = "white",
      font = list(size = 12)
    ),
    hovermode = "closest"
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
# p_covid_plotly
