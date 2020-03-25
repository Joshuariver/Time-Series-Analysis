# Daily increase rate by country


# Coronavirus analysis

rm(list=ls())
setwd("~/R/coronavirus")


# 1.1 Data Source
# The data source used for this analysis is the 2019 Novel Coronavirus COVID-19 (2019-nCoV) 
# Data Repository1
# built the Center for Systems Science and Engineering, Johns Hopkins University.


# 1.2 R Packages

library(magrittr)
library(lubridate)
library(tidyverse)
library(gridExtra)
library(knitr)
library(kableExtra)
library(ggplot2)
library(ggrepel)


# 2 Loading Data

## source data files
filenames <- c('time_series_19-covid-Confirmed.csv',
               'time_series_19-covid-Deaths.csv',
               'time_series_19-covid-Recovered.csv')
url.path <- 'https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/'

## download files to local
download <- function(filenames) {
  url <- file.path(url.path, filenames)
  dest <- file.path('./data/', filenames)
  download.file(url, dest)
}
bin <- lapply(filenames, download)

## load data into R
data.confirmed <- read.csv("./data/time_series_19-covid-Confirmed.csv")
data.deaths <- read.csv("./data/time_series_19-covid-Deaths.csv")
data.recovered <- read.csv("./data/time_series_19-covid-Recovered.csv")
dim(data.confirmed)

data.confirmed[, 1:10] %>% sample_n(10) %>%
  kable("latex", booktabs=T, caption="Raw Data (Confirmed, First 10 Columns only)") %>%
  kable_styling(font_size=6, latex_options = c("striped", "hold_position", "repeat_header"))


# below we check the time frame of the data
n.col <- ncol(data.confirmed)
## get dates from column names
dates <- names(data.confirmed)[5:n.col] %>% substr(2,8) %>% mdy()
range(dates)

max.date <- max(dates)


# 3 Data Preparation
# 3.1 Data Cleaning

## data cleaning and transformation
cleanData <- function(data) {
  ## remove some columns
  data %<>% select(-c(Province.State, Lat, Long)) %>% rename(country=Country.Region)
  ## convert from wide to long format
  data %<>% gather(key=date, value=count, -country)
  ## convert from character to date
  data %<>% mutate(date = date %>% substr(2,8) %>% mdy())
  ## aggregate by country
  data %<>% group_by(country, date) %>% summarise(count=sum(count)) %>% as.data.frame()
  return(data)
}

## clean the three datasets
data.confirmed %<>% cleanData() %>% rename(confirmed=count)
data.deaths %<>% cleanData() %>% rename(deaths=count)
data.recovered %<>% cleanData() %>% rename(recovered=count)

## merge above 3 datasets into one, by country and date
data <- data.confirmed %>% merge(data.deaths) %>% merge(data.recovered)



# Write a data to specific form

# write(cfd, file = "data.confirmed.2020032x.csv")

# Here we need to edit data to have a number of daily confirmed case increase




# cfd <- read.csv("data.confirmed.20200322.csv")

cfd <- data
cfd1 <- data
cfd1$date <- cfd1$date + 1
cfdt <- merge(cfd,cfd1,by=c("country","date"))
str(cfdt)
cfdt <- mutate(cfdt, daily_confirmed = confirmed.x - confirmed.y)
cfdt <- mutate(cfdt, daily_dead = deaths.x - deaths.y) 
cfdt <- mutate(cfdt, daily_recovered = recovered.x - recovered.y)

colnames(cfdt)

scountries <- c("Korea, South","US","Iran","Italy","France","Spain","South Africa","Brazil","Argentina","United Kingdom","Japan")

cfd.i.countries <- filter(cfdt, cfdt$country == scountries)

ggplot(cfd.i.countries, aes(x=date, y=daily_confirmed, color=country)) + geom_point() +  geom_label_repel(aes(label = country), box.padding   = 0.35, point.padding = 0.5, segment.color = 'grey50') +
theme_classic() + geom_smooth() + labs(title="주요국가 확진자수 추세", x="날짜", y="인원수")


ggplot(cfd.i.countries, aes(x=date, y=confirmed.x, color=country)) + geom_point() + geom_smooth() + labs(title="주요국가 확진자수 추세", x="날짜", y="인원수") + geom_label(label=rownames(cfd.i.countries$country), 
  nudge_x = 0.25, nudge_y = 0.25, 
  check_overlap = T
)


cfd.korea <- filter(cfdt, cfdt$country == "Korea, South")
# plot(cfd.korea$confirmed)
ggplot(cfd.korea, aes(x=date, y=daily_confirmed)) + geom_point() + geom_smooth() + labs(title="COVID-19 한국 확진자수 추세", x="날짜", y="인원수")

cfd.usa <- filter(cfdt, cfdt$country == "US")
ggplot(cfd.usa, aes(x=date, y=daily_confirmed)) + geom_point() + geom_smooth() + labs(title="COVID-19 미국 확진자수 추세", x="날짜", y="인원수")

cfd.iran <- filter(cfdt, cfdt$country == "Iran")
ggplot(cfd.iran, aes(x=date, y=daily_confirmed)) + geom_point() + geom_smooth() + labs(title="COVID-19 이란 확진자 수 추세", x="날짜", y="인원수" )

cfd.italy <- filter(cfdt, cfdt$country == "Italy")
ggplot(cfd.iran, aes(x=date, y=daily_confirmed)) + geom_point() + geom_smooth() + labs(title="COVID-19 이태리 확진자 수 추세", x="날짜", y="인원수" )

cfd.france <- filter(cfdt, cfdt$country == "France")
ggplot(cfd.france, aes(x=date, y=daily_confirmed)) + geom_point() + geom_smooth() + labs(title="COVID-19 프랑스 확진자 수 추세", x="날짜", y="인원수" )

cfd.china <- filter(cfdt, cfdt$country == "China")
ggplot(cfd.china, aes(x=date, y=daily_confirmed)) + geom_point() + geom_smooth() + labs(title="COVID-19 중국 확진자 수 추세", x="날짜", y="인원수" )

cfd.spain <- filter(cfdt, cfdt$country == "Spain")
ggplot(cfd.spain, aes(x=date, y=daily_confirmed)) + geom_point() + geom_smooth() + labs(title="COVID-19 스페인인 확진자 수 추세", x="날짜", y="인원수" )

cfd.safrica <- filter(cfdt, cfdt$country == "South Africa")
ggplot(cfd.safrica, aes(x=date, y=daily_confirmed)) + geom_point() + geom_smooth() + labs(title="COVID-19 남아공 확진자 수 추세", x="날짜", y="인원수" )

cfd.brazil <- filter(cfdt, cfdt$country == "Brazil")
ggplot(cfd.brazil, aes(x=date, y=daily_confirmed)) + geom_point() + geom_smooth() + labs(title="COVID-19 브라질 확진자 수 추세", x="날짜", y="인원수" )

cfd.argentina <- filter(cfdt, cfdt$country == "Argentina")
ggplot(cfd.argentina, aes(x=date, y=daily_confirmed)) + geom_point() + geom_smooth() + labs(title="COVID-19 아르헨티나 확진자 수 추세", x="날짜", y="인원수" )

cfd.uk <- filter(cfdt, cfdt$country == "United Kingdom")
ggplot(cfd.uk, aes(x=date, y=daily_confirmed)) + geom_point() + geom_smooth() + labs(title="COVID-19 영국 확진자 수 추세", x="날짜", y="인원수" )

cfd.japan <- filter(cfdt, cfdt$country == "Japan")
ggplot(cfd.japan, aes(x=date, y=daily_confirmed)) + geom_point() + geom_smooth() + labs(title="COVID-19 일본 확진자 수 추세", x="날짜", y="인원수" )
