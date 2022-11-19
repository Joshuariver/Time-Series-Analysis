# Modeltime 을 이용해서 환율예측 모델링 해 보기

# 환율 데이터 다운로드 방법을 실시간으로 전환
# 실시간 전환 가능성은 PRED, Yahoo Finance api 사용

# 원 코드 위치 Asia Against Dollar: Forecasting with Modeltime
# https://www.r-bloggers.com/2022/11/asia-against-dollar-forecasting-with-modeltime/


library(tidymodels)
library(modeltime)
library(tidyverse)
library(timetk)
library(plotly)
library(tidyquant)
library(glue)


# 다운로드 받아 가공되어 있는 데이터 다운로드

# df <- 
#  read_csv("https://raw.githubusercontent.com/mesdi/blog/main/fx.csv") %>% 
#  mutate(date = parse_date(date, "%m/%d/%Y"))


# Yahoofinance 에서 실시간 데이터 다운받기

library(yahoofinancer)
library(purrr)

df <- currency_converter('USD', 'KRW', '2020-01-01', '2022-11-19')
df <- df %>% mutate(date = parse_date(date, "%m/%d/%Y"))
df <- mutate(df, symbol = "KRW")
df <- df[,c(8,1,7)]
colnames(df) <- c("symbol","date","value")
df$date <- AS_DATE(df$date)
# currency_converter('USD', 'KRW', period = '3mo', interval = '1d')



#Comparing with the daily line plots
df %>%
  group_by(symbol) %>% #for facetting
  plot_time_series(date, 
                   value, 
                   .facet_ncol = 2, 
                   .interactive = FALSE,
                   .title = "") + 
  theme(strip.background = element_rect(fill="#215a4d", color ="#215a4d"))


#Low to high frequency
df %>% 
  group_by(symbol) %>%
  pad_by_time(date, .by = "hour") %>%
  mutate(across(value, .fns = ts_impute_vec, period = 1)) %>%
  filter_by_time(date, "2022-10", LAST(date)) %>% 
  plot_time_series(date, 
                   value, 
                   .facet_ncol = 2, 
                   .interactive = FALSE, 
                   .line_size = 1,
                   .title = "") +
  theme(strip.background = element_rect(fill="#9da832", color ="#9da832"))



# train 데이터와 test 데이터 분리

# Split Data 80/20
splits <- initial_time_split(df, prop = 0.9)



#Preprocessing
df_rec <- 
  recipe(value ~ date, training(splits)) %>% 
  step_timeseries_signature(date) %>%   #adding time series signatures
  step_fourier(date, period = 365, K = 5) %>%
  step_rm(date) %>%
  step_rm(contains("iso"), 
          contains("minute"), 
          contains("hour"),
          contains("am.pm"), 
          contains("xts")) %>%
  step_normalize(contains("index.num"), date_year) %>%
  step_dummy(contains("lbl"), one_hot = TRUE) 


#Modeling (with random forest) and fitting
df_spec <- 
  rand_forest(trees = 1000) %>% 
  set_engine("ranger") %>% 
  set_mode("regression")

workflow_df <- 
  workflow() %>%
  add_recipe(df_rec) %>%
  add_model(df_spec)

set.seed(12345)
workflow_df_fit <- 
  workflow_df %>% 
  fit(training(splits))




#Model and calibration table
model_table <- modeltime_table(workflow_df_fit) 

df_calibration <- 
  model_table %>% 
  modeltime_calibrate(testing(splits))

#Accuracy 
df_calibration %>% 
  modeltime_accuracy() %>% 
  select(rsq)

# A tibble: 1 x 1
#    rsq
#  <dbl>
#1 0.654


#Forecasting the next 3 months
df_calibration %>% 
  modeltime_refit(df %>% filter(symbol == "KRW")) %>% 
  modeltime_forecast(h = "3 months", #forecast horizon
                     actual_data = df %>% filter(symbol == "KRW")) %>% 
  plot_modeltime_forecast(.interactive = TRUE, 
                          .legend_show = FALSE,
                          .title = "향후 3개월간 원달러 환율 추세 예측") %>% 
  #customizing the hoverinfo texts of the traces(lines)
  style(text = glue("{.$x$data[[1]]$x}\n{.$x$data[[1]]$y}"), traces = 1) %>% 
  style(text = glue("{.$x$data[[2]]$x}\n{round(.$x$data[[2]]$y, 4)}"), traces = 2) %>% 
  style(text = glue("{.$x$data[[3]]$x}\n{round(.$x$data[[3]]$y, 4)}"), traces = 3) %>% 
  style(text = glue("{.$x$data[[4]]$x}\n{round(.$x$data[[4]]$y, 4)}"), traces = 4)
