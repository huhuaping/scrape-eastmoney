# analyze the bid list data set
# and visualize the results

# I want to analyze stock bid list data set and visualize withdraw buy or sell hands on the horizontal time.
## can you show me template plot can visualize this data set?

# prepare packages====
library("here")
library("tidyverse")
library(ggplot2)
library(lubridate)
library(plotly)
library(glue)

# prepare stock daily data use ====
library(usethis)
library("quantmod")
## R包`quantmod` 接口数据
### tiingo api acount and api key
### 进行环境变量设置
#usethis::edit_r_environ(scope = "project")
### 制订默认的key
setDefaults(getSymbols.tiingo, api.key = Sys.getenv("TIINGO_API_KEY"))

### 使用`tiingo`源，`yahooo finance`国内不可用
stock_id <- "603306"
stock_abbr <- "stock_603306"
date_start <- "2024-12-20"
date_end <- "2024-12-24"
df <- getSymbols(stock_id , src='tiingo', from=date_start, to= date_end, auto.assign = FALSE)

# create data.frame and rename columns
names_new <- c("date", "open", "high", "low", "close", "volume", "adj_close")
tbl_daily <- df %>%
  as.data.frame() %>%
  rownames_to_column(var = "date") %>%
  rename_all(., ~names_new)

# obtain the OHLC price of the target date 
date_tar <- "2024-12-23"
tbl_target <- tbl_daily %>%
  filter(date == date_tar)
open <- tbl_target$open
high <- tbl_target$high
low <- tbl_target$low
close <- tbl_target$close

## obtain the limit up or limit down price of the target date====
# Custom function to calculate limit up and limit down prices


calculate_limit_prices <- function(
    ticker, date,
    date_from ="2024-01-01", date_to = Sys.Date(),
    limit_up_pct = 0.10, limit_down_pct = 0.10) {
  # Get historical stock data
  stock_data <-getSymbols(ticker, src = "tiingo", from = date_from, to = date_to, auto.assign = FALSE)
  
  # Convert date to Date object
  date <- as.Date(date)
  
  # Check if data exists for the specified date
  if (!date %in% index(stock_data)) {
    stop("No trading data available for the specified date.")
  }
  
  # Get previous day's closing price
  previous_day <- index(stock_data[index(stock_data) < date])[length(index(stock_data[index(stock_data) < date]))]
  
  if (is.na(previous_day)) {
    stop("No previous trading day found.")
  }
  
  previous_close <- stock_data[previous_day, paste0(ticker,".Close")]
  
  # Calculate limit up and limit down prices
  limit_up_price <- previous_close * (1 + limit_up_pct)
  limit_down_price <- previous_close * (1 - limit_down_pct)
  
  # Return results as a list
  return(
    list(
      previous_day = previous_day,
      Previous_Close = as.vector(previous_close),
      now_date = date,
      Limit_Up_Price = as.vector(limit_up_price),
      Limit_Down_Price = as.vector(limit_down_price)
   )
  )
}

# Example usage of the function
limit_query <- calculate_limit_prices(
  ticker = stock_id, date = date_tar, date_from = "2024-12-01",
  limit_up_pct = 0.10, limit_down_pct = 0.10
  )

price_range <- c(limit_query$Limit_Down_Price, limit_query$Limit_Up_Price)

# load the deal flow data====
file_tar <- here(glue("data/deal-flow/603306-{date_tar}.csv"))
names_new  <- c("stock_id","stock_name", "date", "time", "price", "hands")

tbl_deal <- read_csv(file_tar) %>%
  rename_all(., ~names_new) %>%
  # create a new column for time
  mutate(
    time = as.POSIXct(paste(date, time), format="%Y-%m-%d %H:%M:%S")
  ) %>%
  # clean price column. remove symbols "↓" and "↓", and convert to numeric.
  # convert hands to numeric.
  mutate(
    price = as.numeric(gsub("↓|↑", "", price)),
    hands = as.numeric(hands)
  ) 


# load the bid flow data====
file_tar <- here(glue("code/bid/603306-hwkj-{date_tar}-s60h100-clean.rds"))
tbl_clean <- readRDS(file_tar)

# reduce data set====
tbl_reduce <- tbl_clean %>%
  select(date, time, hour, minute, second,
         price, volume, hands, direction, volume_check) %>%
  # filter price  at the range of limit up and limit down price
  filter(price >= price_range[1] & price <= price_range[2])

# save as txt file for Google ai studio====
# file_out <- here("code/bid/603306-hwkj-2024-12-20-s60h100-reduce.txt")
# write.table(tbl_reduce, file_out, sep = "\t", row.names = FALSE)



# use plotly to interactivity====
# what I want is that the visualization should show all variables including time, price, hands, volume and direction.

# 1. Data Loading and Preparation
# Assuming your data is in a data frame called `df` after reading it into R
# If not, use read.delim to load into the df
# e.g., df <- read.delim("603306-hwkj-2024-12-20-s60h100-reduce.txt", sep="\t", header=TRUE)

# Convert time to POSIXct (for easier time handling)
df <- tbl_reduce %>%
  mutate(
    time = as.POSIXct(paste(date, time), format="%Y-%m-%d %H:%M:%S"),
    minute = case_when(
      !is.na(minute) ~ as.numeric(gsub("m", "", minute)),
      TRUE ~ 0),
    second = case_when(
      !is.na(second) ~ as.numeric(gsub("s", "", second)),
      TRUE ~ 0)
  )

df <- df %>%
  mutate(
    time_adj = time + minutes(minute) + seconds(second)
  ) %>%
  select(-minute,-second,-time) %>%
  rename(time = time_adj)


# Filter for withdraw buy and sell events
withdraw_events <- df %>%
  filter(direction %in% c("撤买", "撤卖")) %>%
  mutate(
    price_change = scales::percent(price/limit_query$Previous_Close-1, 0.01),
    time_group = floor_date(time, "15 minutes")
    )


# 2. Interactive Plot with plotly
# how can I add another layer of distribution of  hands data in this chart? and the added layer chart can change along time axis.
library(plotly)
#library(ggridges)
p <- ggplot(withdraw_events, aes(x = time, y = price)) + # Use price for the y axis
  geom_point(aes(color = direction,
                 size = volume, # Use size for volume
                 text = paste(  # Tooltip info
                   "Time: ", format(time, "%H:%M:%S"),
                   "<br>Direction: ", direction,
                   "<br>Price: ", price,
                   "<br>Pct_change: ", price_change,
                   "<br>Hands: ", hands,
                   "<br>Volume: ", volume
                 )
  )) +
  # Add stock price line with another data set "tbl_deal"
  geom_line(data = tbl_deal, aes(x = time, y = price), color = "blue") +
  labs(
    title = "Interactive View of Withdraw Buy and Sell Events",
    x = "Time",
    y = "Price",
    color = "Event Type",
    size = "Volume"
  ) +
  # adding a boxplot
  # geom_boxplot(
  #   aes(x=time_group, y = hands, group = time, fill = direction), 
  #   width = 0.003, alpha = 0.3, color = 'gray'
  #   ) +
  # add a secondary y axis showing the price change rate
  # geom_line(aes(y=price_change), color = "purple") +
  # scale_y_continuous(
  #   name = "Price",
  #   labels = scales::label_number(accuracy = 0.01),
  #   # Major breaks every 10 units
  #   breaks = seq(price_range[1], price_range[2], by = (price_range[2] - price_range[1])/20),
  #   # Set limits for Y-axis
  #   limits = c(price_range[1], price_range[2]),
  #   sec.axis = sec_axis(~.,
  #                       name = "Price Change (%)",
  #                       breaks = seq(-0.1, 0.1, by = 0.01),
  #                       limits = c(-.1, 0.1),
  #                       labels = scales::percent_format(accuracy = 1))
  #) +
  # set time axis which main break is 1 hour, and second grid is 15 minutes
  scale_x_datetime(
    date_breaks = "1 hour",         # Major breaks at every hour
    date_minor_breaks = "15 min",    # Minor breaks at every 15 minutes
    date_labels = "%H:%M"            # Format for labels
  ) +
  # set the Intercept line with previous close price
  geom_hline(yintercept = limit_query$Previous_Close, linetype = "solid", color = "black", size = 0.75) +  
  # set the horizontal line with today open price
  geom_hline(yintercept = open, linetype = "dashed", color = "yellow", size = 0.5) +
  scale_y_continuous(
    # Major breaks every 10 units
    breaks = seq(price_range[1], price_range[2], by = (price_range[2] - price_range[1])/20),  
    # Set limits for Y-axis
    limits = c(price_range[1], price_range[2]) ,  
    # format y ticker label
    labels = scales::label_number(accuracy = 0.01),
    sec.axis = sec_axis(~.x/limit_query$Previous_Close-1,
                        name = "Price Change (%)",
                        breaks = seq(-0.1, 0.1, by = 0.01),
                        #limits = c(-0.1, 0.1),
                        labels = scales::percent_format(accuracy = 1))
  ) +
  theme_minimal()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


ggplotly(p, tooltip = "text")
