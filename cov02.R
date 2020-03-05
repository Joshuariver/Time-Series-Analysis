# https://www.ecdc.europa.eu/en/geographical-distribution-2019-ncov-cases
# ???????????? ????????? ????????? ???????????? ?????? ??????


rm(list=ls())
setwd("~/R/Time Series Analysis/Time Series Analysis/coronavirus")

library(readxl)
library(dplyr)

covid19 <- read_xls("COVID-19.xls")

covid19$EU <- as.factor(covid19$EU)
covid19$CountryExp <- as.factor(covid19$CountryExp)
covid19$GeoId <- as.factor(covid19$GeoId)

covid19kor <- filter(covid19, GeoId == "KR")
covid19kor_con <- covid19kor[,c(3)]

covid19korts <- ts(covid19kor_con, frequency = 7, start = c(2019-12-31))
covid19korts

plot.ts(covid19korts)


library(zoo)

x <- zoo(covid19kor_con, seq(from = as.Date("2019-12-31"), to = as.Date("2020-03-04"), by = 1))
str(x)

plot(x)
