# 존스홉킨스대학에서 제공하는 표준 코로나 확산데이터를 활용하여 추가적인 지표 개발하기
# 2020년 3월 29일    하재선 (joshuariver@gmail.com)
# Create additional measure by using corona virus spread data from Johns Hopkins University Github repository by Jaesun HA


# 데이터 초기화 및 폴더 지정

rm(list=ls())
setwd("~/R/coronavirus")


# 1.1 데이터 소스
# 이 데이터는 COVID-19 확산 데이터임 (2019-nCoV) 
# Data Repository1
# Johns Hopkins 대학 Center for Systems Science and Engineering 연구소에서 제공


# 1.2 R Packages 설치

library(magrittr)
library(lubridate)
library(tidyverse)
library(gridExtra)
library(knitr)
library(kableExtra)
library(ggplot2)
library(ggrepel)


# 2 데이터 가져오기 

## 원본 데이터 파일 위치 지정
filenames <- c('time_series_covid19_confirmed_global.csv',
               'time_series_covid19_deaths_global.csv',
               'time_series_covid19_recovered_global.csv')
url.path <- 'https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/'

## 로컬 컴퓨터로 데이터 다운로드
download <- function(filenames) {
  url <- file.path(url.path, filenames)
  dest <- file.path('./data/', filenames)
  download.file(url, dest)
}
bin <- lapply(filenames, download)

## 다운받은 데이터를 R로 읽어오기
data.confirmed <- read.csv("./data/time_series_covid19_confirmed_global.csv")
data.deaths <- read.csv("./data/time_series_covid19_deaths_global.csv")
data.recovered <- read.csv("./data/time_series_covid19_recovered_global.csv")
dim(data.confirmed)

data.confirmed[, 1:10] %>% sample_n(10) %>%
  kable("latex", booktabs=T, caption="Raw Data (Confirmed, First 10 Columns only)") %>%
  kable_styling(font_size=6, latex_options = c("striped", "hold_position", "repeat_header"))


# 아래에서 데이터 구조를 체크
n.col <- ncol(data.confirmed)
## get dates from column names
dates <- names(data.confirmed)[5:n.col] %>% substr(2,8) %>% mdy()
range(dates)

max.date <- max(dates)


# 3 데이터 준비
# 3.1 데이터 클린징

## 데이터 클린징과 변환
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

## 세 가지 데이터 셋을 모두 클린징
data.confirmed %<>% cleanData() %>% rename(confirmed=count)
data.deaths %<>% cleanData() %>% rename(deaths=count)
data.recovered %<>% cleanData() %>% rename(recovered=count)

## 상기 세 가지 데이터를 모두 합치기
data <- data.confirmed %>% merge(data.deaths) %>% merge(data.recovered)

# 3.2 국가 코드 데이터 합치기

ccode <- read.csv("./data/country_code.csv")
colnames(ccode) <- c("country_jh","country","iso2","iso3","numeric","iso_3166")
str(ccode)

data.c <- merge(x=data, y=ccode, by.x = "country", by.y = "country_jh", all.x = TRUE)


# 3.3 국가별 인구수 데이터와 합치기

library(wpp2019)
data(pop)
wp2020 <- pop[,c(1:2,17)]
colnames(wp2020) <- c("numeric","country_name","population")
wp2020 <- mutate(wp2020, pop = population*1000)  #세계은행의 인구데이터가 K명 단위로 구분되어 있음.

data.d <- merge(x=data.c, y=wp2020, by = "numeric", all.x = TRUE)
data.e <- data.d[,c(2:6,13)]


# 4. 신규 유의한 컬럼 생성하기
# 4.1 전일 대비 순증인원 (신규 확진자(new.confirm), 신규 사망자(new.death), 신규 회복자(new.recov))

data.f <- mutate(data.e, temp.diff = date + 1)
data.g <- data.f[,c(1,7,3:5)]
colnames(data.g) <- c("country","date","confirmed","deaths","recovered")
data.h <- merge(x=data.e, y=data.g, by = c("country","date"), all.x = TRUE)
data.i <- mutate(data.h, new.confirm = (confirmed.x - confirmed.y), new.death = (deaths.x - deaths.x), new.recov = (recovered.x-recovered.y))
data.j <- data.i[,c(1:5,10:12,6)]
colnames(data.j) <- c("country","date","confirmed","deaths","recovered","new.confirm","new.death","new.recov","pop")

# 4.2 인구 백만명명 당 확진자수(confM)
# 누적 확진자 수 당 사망자수 비율 (criticality)
# 누적 확진자 수 당 회복자수 비율 (a.rec.per.conf)
# 일간 추가 확진자 수 당 일단 회복자 수 비율 (d.rec.per.conf)

data.k <- mutate(data.j, confM = confirmed/pop*1000000, criticality = deaths/confirmed, a.rec.per.conf = recovered/confirmed
                 , d.rec.per.conf = recovered/(confirmed - deaths - recovered))


# 5 데이터 시각화
# 5.1 한국의 각종 지수 시각화


cfd.korea <- filter(data.k, data.k$country == "Korea, South")
# plot(cfd.korea$confirmed)
ggplot(cfd.korea, aes(x=date, y=confirmed)) + geom_point() + geom_smooth() + labs(title="COVID-19 한국 확진자수 추세", x="날짜", y="인원수")
ggplot(cfd.korea, aes(x=date, y=confM)) + geom_point() + geom_smooth() + labs(title="COVID-19 한국 100만명당 확진자 추세", x="날짜", y="인원수")
ggplot(cfd.korea, aes(x=date, y=new.confirm)) + geom_point() + geom_smooth() + labs(title="COVID-19 한국 신규 확진자수 추세", x="날짜", y="인원수")
ggplot(cfd.korea, aes(x=date, y=deaths)) + geom_point() + geom_smooth() + labs(title="COVID-19 한국 사망자 추세", x="날짜", y="인원수")
ggplot(cfd.korea, aes(x=date, y=new.death)) + geom_point() + geom_smooth() + labs(title="COVID-19 한국 신규 사망자 추세", x="날짜", y="인원수")
ggplot(cfd.korea, aes(x=date, y=recovered)) + geom_point() + geom_smooth() + labs(title="COVID-19 한국 회복자 추세", x="날짜", y="인원수")
ggplot(cfd.korea, aes(x=date, y=new.recov)) + geom_point() + geom_smooth() + labs(title="COVID-19 한국 신규 회복자 추세", x="날짜", y="인원수")
ggplot(cfd.korea, aes(x=date, y=criticality)) + geom_point() + geom_smooth() + labs(title="COVID-19 한국 사망율율 추세", x="날짜", y="인원수")
ggplot(cfd.korea, aes(x=date, y=a.rec.per.conf)) + geom_point() + geom_smooth() + labs(title="COVID-19 한국 누적 확진자 대 회복인원 비율 추세", x="날짜", y="인원수")
ggplot(cfd.korea, aes(x=date, y=d.rec.per.conf)) + geom_point() + geom_smooth() + labs(title="COVID-19 한국 일간 확진자 대 회복인원 비율 추세", x="날짜", y="인원수")



# Reference
