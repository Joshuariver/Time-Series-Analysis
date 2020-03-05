# Trend Forecasting Models and Seasonality with Time Series
# https://www.r-bloggers.com/trend-forecasting-models-and-seasonality-with-time-series/
# 


rm(list=ls())
setwd("~/R/Time Series Analysis/Time Series Analysis")

library(readxl)

gasoline_df <- read_xlsx("gasoline_df.xlsx")

head(gasoline_df)


#exponential trend model
exp.model <- lm(log(gasoline)~date,data = gasoline_df) 
exp.model.df <- data.frame(x=gasoline_df$date,
                           y=exp(fitted(exp.model)))


#Trend lines 
library(tidyverse)

ggplot(gasoline_df, aes(x = date, y = gasoline)) + geom_point() +
  stat_smooth(method = 'lm', aes(colour = 'linear'), se = FALSE) +
  stat_smooth(method = 'lm', formula = y ~ poly(x,2), aes(colour = 'quadratic'), se= FALSE) +
  stat_smooth(method = 'lm', formula = y ~ poly(x,3), aes(colour = 'cubic'), se = FALSE)+
  stat_smooth(data=exp.model.df, method = 'loess',aes(x,y,colour = 'exponential'), se = FALSE) 

adj_r_squared_linear <- summary(model_linear) %>% 
  .$adj.r.squared %>% 
  round(4)


summary(model_exponential)


library(purrr)

y_predicted <- model_exponential$fitted.values %>% 
  map_dbl(~exp(.+0.09939^2/2))

# And then, we execute the formula shown above to get adjusted R^2

r_squared_exponential <- (cor(y_predicted,gasoline_df$gasoline))^2

adj_r_squared_exponential <- 1-(1-r_squared_exponential)*
  ((nrow(gasoline_df)-1)/(nrow(gasoline_df)-1-1))

#Model variables
model_quadratic <- lm(data = df,gasoline~poly(date,2))
model_cubic <- lm(data = df,gasoline~poly(date,3))

#adjusted coefficients of determination
adj_r_squared_quadratic <- summary(model_quadratic) %>% 
  .$adj.r.squared

adj_r_squared_cubic <- summary(model_cubic) %>% 
  .$adj.r.squared

adj_r_squared_all


#we create a time series variable for the plot
gasoline_ts <- ts(gasoline_df$gasoline,start = c(2013,1),end = c(2019,12),frequency = 12)
#The plot have all the components of the series(T, S, I)
plot(gasoline_ts,lwd=2,ylab="Gasoline")


library(forecast)

gasoline_trend <- forecast::ma(gasoline_ts,12)

gasoline_detrend <- gasoline_ts/gasoline_trend


unadjusted_seasonality <- sapply(1:12, function(x) mean(window(gasoline_detrend,c(2013,x),c(2019,12),deltat=1),na.rm = TRUE)) %>% round(4)

adjusted_seasonality <- (unadjusted_seasonality*(12/sum(unadjusted_seasonality))) %>% 
  round(4)

#building a data frame to plot in ggplot
adjusted_seasonality_df <- data_frame(
  months=month.abb,
  index=adjusted_seasonality)

#converting char month names to factor sequentially
adjusted_seasonality_df$months <- factor(adjusted_seasonality_df$months,levels = month.abb)

ggplot(data = adjusted_seasonality_df,mapping = aes(x=months,y=index))+
  geom_point(aes(colour=months))+
  geom_hline(yintercept=1,linetype="dashed")+
  theme(legend.position = "none")+
  ggtitle("Seasonality of Unleaded Gasoline Prices for 95 Octane")+
  theme(plot.title = element_text(h=0.5))

#Trend is shown by red line
plot(gasoline_ts,lwd=2,ylab="Gasoline")+
  lines(gasoline_trend,col="red",lwd=3)

plot(gasoline_trend,lwd=3,col="red",ylab="Trend")


plot(as.ts(rep(unadjusted_seasonality,12)),lwd=2,ylab="Seasonality",xlab="")

randomness <- gasoline_ts/(gasoline_trend*unadjusted_seasonality)
plot(randomness,ylab="Randomness",lwd=2)

