# Modeltime Origial Tutorial
# https://business-science.github.io/modeltime/articles/getting-started-with-modeltime.html

rm(list=ls())

# Getting started with Modeltime


# Modeltime 의 진행 절차

# 1. Collect data and split into training and test sets
# 2. Create & Fit Multiple Models
# 3. Add fitted models to a Model Table
# 4. Calibrate the models to a testing set.
# 5. Perform Testing Set Forecast & Accuracy Evaluation
# 6. Refit the models to Full Dataset & Forecast Forward


library(xgboost)
library(tidymodels)
library(modeltime)
library(tidyverse)
library(lubridate)
library(timetk)

# This toggles plots from plotly (interactive) to ggplot (static)
interactive <- FALSE


# Step 1 - Collect data and split into training and test sets.

# Data
m750 <- m4_monthly %>% filter(id == "M750")

m750 %>%
  plot_time_series(date, value, .interactive = interactive)

# Split Data 80/20
splits <- initial_time_split(m750, prop = 0.9)


# Step 2 - Create & Fit Multiple Models


# Timeseries 에서 test 해 볼 4 가지 예측 알고리즘

# ARIMA
# Exponential Smoothing
# Linear Regression
# MARS (Multivariate Adaptive Regression Splines)


# Model 1: auto_arima ----
model_fit_arima_no_boost <- arima_reg() %>%
  set_engine(engine = "auto_arima") %>%
  fit(value ~ date, data = training(splits))


# Model 2: arima_boost ----
model_fit_arima_boosted <- arima_boost(
  min_n = 2,
  learn_rate = 0.015
) %>%
  set_engine(engine = "auto_arima_xgboost") %>%
  fit(value ~ date + as.numeric(date) + factor(month(date, label = TRUE), ordered = F),
      data = training(splits))


# Model 3: ets ----
model_fit_ets <- exp_smoothing() %>%
  set_engine(engine = "ets") %>%
  fit(value ~ date, data = training(splits))


# Model 4: prophet ----
model_fit_prophet <- prophet_reg() %>%
  set_engine(engine = "prophet") %>%
  fit(value ~ date, data = training(splits))


# Model 5: lm ----
model_fit_lm <- linear_reg() %>%
  set_engine("lm") %>%
  fit(value ~ as.numeric(date) + factor(month(date, label = TRUE), ordered = FALSE),
      data = training(splits))


# Model 6: earth ----
model_spec_mars <- mars(mode = "regression") %>%
  set_engine("earth") 

recipe_spec <- recipe(value ~ date, data = training(splits)) %>%
  step_date(date, features = "month", ordinal = FALSE) %>%
  step_mutate(date_num = as.numeric(date)) %>%
  step_normalize(date_num) %>%
  step_rm(date)

wflw_fit_mars <- workflow() %>%
  add_recipe(recipe_spec) %>%
  add_model(model_spec_mars) %>%
  fit(training(splits))



# Step 3 - Add fitted models to a Model Table

models_tbl <- modeltime_table(
  model_fit_arima_no_boost,
  model_fit_arima_boosted,
  model_fit_ets,
  model_fit_prophet,
  model_fit_lm,
  wflw_fit_mars
)

models_tbl


# Step 4 - Calibrate the model to a testing set

calibration_tbl <- models_tbl %>%
  modeltime_calibrate(new_data = testing(splits))

calibration_tbl


# Step 5 - Testing Set Forecast & Accuracy Evaluation

# 5A - Visualizing the Forecast Test

calibration_tbl %>%
  modeltime_forecast(
    new_data    = testing(splits),
    actual_data = m750
  ) %>%
  plot_modeltime_forecast(
    .legend_max_width = 25, # For mobile screens
    .interactive      = interactive
  )


# 5B - Accuracy Metrics

calibration_tbl %>%
  modeltime_accuracy() %>%
  table_modeltime_accuracy(
    .interactive = interactive
  )


# Step 6 - Refit to Full Dataset & Forecast Forward

refit_tbl <- calibration_tbl %>%
  modeltime_refit(data = m750)

refit_tbl %>%
  modeltime_forecast(h = "3 years", actual_data = m750) %>%
  plot_modeltime_forecast(
    .legend_max_width = 25, # For mobile screens
    .interactive      = interactive
  )


# Refitting - What happened?
# The models have all changed! (Yes - this is the point of refitting)

# The LM model looks much better now because the linear trend line has now been fit to new data that follows the longer term trend.
# The EARTH model has a trend that is more representative of the near-term trend.
# The PROPHET model has a trend that is very similar to the EARTH model (this is because both modeling algorithms use changepoints to model trend, and prophet’s auto algorithm seems to be doing a better job at adapting).
# The ETS model has changed from (M,A,A) to (A,A,A).
# The ARIMA model have been updated and better capture the upswing.
# This is the (potential) benefit of refitting.

# More often than not refitting is a good idea. Refitting:
  
#  Retrieves your model and preprocessing steps
# Refits the model to the new data
# Recalculates any automations. This includes:
#  Recalculating the long-term trend for Linear Model
# Recalculating the changepoints for the Earth Model
# Recalculating the ARIMA and ETS parameters
# Preserves any parameter selections. This includes:
#  XGBoost Parameters in the Boosted ARIMA min_n = 2, learn_rate = 0.015.
# Any other defaults that are not automatic calculations are used.

# Summary
# We just showcased the Modeltime Workflow. But this is a simple problem. And, there’s a lot more to learning time series.

# Many more algorithms
# Ensembling
# Machine Learning
# Deep Learning
# Scalable Modeling: 10,000+ time series


