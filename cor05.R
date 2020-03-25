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

## first 10 records when it first broke out in China
data %>% filter(country=='Mainland China') %>% head(10) %>%
kable("latex", booktabs=T, caption="Raw Data (with first 10 Columns Only)",
format.args=list(big.mark=",")) %>%
kable_styling(latex_options = c("striped", "hold_position", "repeat_header"))



# 3.2 Cases for the Whole World


## counts for the whole world
data.world <- data %>% group_by(date) %>%
summarise(country='World',
confirmed = sum(confirmed),
deaths = sum(deaths),
recovered = sum(recovered))
data %<>% rbind(data.world)
## remaining confirmed cases
data %<>% mutate(remaining.confirmed = confirmed - deaths - recovered)


# 3.3 Daily Increases and Death Rates

## sort by country and date
data %<>% arrange(country, date)

## daily increases of deaths and cured cases
## set NA to the increases on day1
n <- nrow(data)
day1 <- min(data$date)
data %<>% mutate(confirmed.inc = ifelse(date == day1, NA, confirmed - lag(confirmed, n=1)),
deaths.inc = ifelse(date == day1, NA, deaths - lag(deaths, n=1)),
recovered.inc = ifelse(date == day1, NA, recovered - lag(recovered, n=1)))

## death rate based on total deaths and cured cases
data %<>% mutate(rate.upper = (100 * deaths / (deaths + recovered)) %>% round(1))

## lower bound: death rate based on total confirmed cases
data %<>% mutate(rate.lower = (100 * deaths / confirmed) %>% round(1))

## death rate based on the number of death/cured on every single day
data %<>% mutate(rate.daily = (100 * deaths.inc / (deaths.inc + recovered.inc)) %>% round(1))


# 4 Visualisation
# After tidying up the data, we visualise it with various charts.
# 4.1 Top 10 countries with most confirmed cases


## ranking by confirmed cases
data.latest <- data %>% filter(date == max(date)) %>%
select(country, date, confirmed, deaths, recovered, remaining.confirmed) %>%
mutate(ranking = dense_rank(desc(confirmed)))

## top 10 countries: 12 incl. 'World' and 'Others'
top.countries <- data.latest %>% filter(ranking <= 12) %>%
arrange(ranking) %>% pull(country) %>% as.character()

## move 'Others' to the end
top.countries %<>% setdiff('Others') %>% c('Others')
top.countries

# a <- data %>% group_by(country) %>% tally()

## put all others in a single group of 'Others'
df <- data.latest %>% filter(!is.na(country) & country!='World') %>%
mutate(country=ifelse(ranking <= 12, as.character(country), 'Others')) %>%
mutate(country=country %>% factor(levels=c(top.countries)))
df %<>% group_by(country) %>% summarise(confirmed=sum(confirmed))

## precentage and label
df %<>% mutate(per = (100*confirmed/sum(confirmed)) %>% round(1)) %>%
mutate(txt = paste0(country, ': ', confirmed, ' (', per, '%)'))

# pie(df$confirmed, labels=df$txt, cex=0.7)
df %>% ggplot(aes(fill=country)) +
geom_bar(aes(x='', y=per), stat='identity') +
coord_polar("y", start=0) +
xlab('') + ylab('Percentage (%)') +
labs(title=paste0('Top 10 Countries with Most Confirmed Cases (', max.date, ')')) +
scale_fill_discrete(name='Country', labels=df$txt)


## convert from wide to long format, for purpose of drawing a area plot
data.long <- data %>% select(c(country, date, confirmed, remaining.confirmed, recovered, deaths)) %>%
gather(key=type, value=count, -c(country, date))

## set factor levels to show them in a desirable order
data.long %<>% mutate(type = factor(type, c('confirmed', 'remaining.confirmed', 'recovered', 'deaths')))

## cases by type
df <- data.long %>% filter(country %in% top.countries) %<>%
mutate(country=country %>% factor(levels=c(top.countries)))
df %>% filter(country != 'World') %>%
ggplot(aes(x=date, y=count, fill=country)) +
geom_area() + xlab('Date') + ylab('Count') +
labs(title='Cases around the World') +
theme(legend.title=element_blank()) +
facet_wrap(~type, ncol=2, scales='free_y')


## excluding Mainland China
df %>% filter(!(country %in% c('World', 'China'))) %>%
ggplot(aes(x=date, y=count, fill=country)) +
geom_area() + xlab('Date') + ylab('Count') +
labs(title='Cases around the World (excl. China)') +
theme(legend.title=element_blank()) +
facet_wrap(~type, ncol=2, scales='free_y')


## if Australia in not in top 10, add it in and remove 'Others'
if(!('Australia' %in% top.countries)) {
top.countries %<>% setdiff('Others') %>% c('Australia')
df <- data.long %>% filter(country %in% top.countries) %<>%
mutate(country=country %>% factor(levels=c(top.countries)))
}

## cases by country
df %>% filter(type != 'confirmed') %>%
ggplot(aes(x=date, y=count, fill=type)) +
geom_area(alpha=0.5) + xlab('Date') + ylab('Count') +
labs(title=paste0('COVID-19 Cases by Country (', max.date, ')')) +
scale_fill_manual(values=c('red', 'green', 'black')) +
theme(legend.title=element_blank(), legend.position='bottom') +
facet_wrap(~country, ncol=3, scales='free_y')


# 4.2 Current (or Remaining) Confirmed Cases
# Now we focuse on all cases world wide.
# data %<>% filter(country=='Mainland China')
# data %<>% filter(country=='Australia')
data %<>% filter(country=='World')
n <- nrow(data)

## current confirmed and its increase
plot1 <- ggplot(data, aes(x=date, y=remaining.confirmed)) +
geom_point() + geom_smooth() +
xlab('Date') + ylab('Count') + labs(title='Current Confirmed Cases')
plot2 <- ggplot(data, aes(x=date, y=confirmed.inc)) +
geom_point() + geom_smooth() +
xlab('Date') + ylab('Count') + labs(title='Increase in Current Confirmed')

# + ylim(0, 4500)
grid.arrange(plot1, plot2, ncol=2)
## `geom_smooth()` using method = 'loess' and formula 'y ~ x'
## `geom_smooth()` using method = 'loess' and formula 'y ~ x'


# 4.3 Deaths and Cured Cases
# ## a scatter plot with a smoothed line and vertical x-axis labels
plot1 <- ggplot(data, aes(x=date, y=deaths)) +
geom_point() + geom_smooth() +
xlab('Date') + ylab('Count') + labs(title='Deaths')
plot2 <- ggplot(data, aes(x=date, y=recovered)) +
geom_point() + geom_smooth() +
xlab('Date') + ylab('Count') + labs(title='Recovered Cases')
plot3 <- ggplot(data, aes(x=date, y=deaths.inc)) +
geom_point() + geom_smooth() +
xlab('Date') + ylab('Count') + labs(title='Increase in Deaths')
plot4 <- ggplot(data, aes(x=date, y=recovered.inc)) +
geom_point() + geom_smooth() +
xlab('Date') + ylab('Count') + labs(title='Increase in Recovered Cases')


## show four plots together, with 2 plots in each row
grid.arrange(plot1, plot2, plot3, plot4, nrow=2)
## `geom_smooth()` using method = 'loess' and formula 'y ~ x'
## `geom_smooth()` using method = 'loess' and formula 'y ~ x'
## `geom_smooth()` using method = 'loess' and formula 'y ~ x'
## `geom_smooth()` using method = 'loess' and formula 'y ~ x'


## three death rates
plot1 <- ggplot(data, aes(x=date)) +
geom_line(aes(y=rate.upper, colour='Upper bound')) +
geom_line(aes(y=rate.lower, colour='Lower bound')) +
geom_line(aes(y=rate.daily, colour='Daily')) +
xlab('Date') + ylab('Death Rate (%)') + labs(title='Overall') +
theme(legend.position='bottom', legend.title=element_blank()) +
ylim(0, 90)


## focusing on last 2 weeks
plot2 <- ggplot(data[n-(14:0),], aes(x=date)) +
geom_line(aes(y=rate.upper, colour='Upper bound')) +
geom_line(aes(y=rate.lower, colour='Lower bound')) +
geom_line(aes(y=rate.daily, colour='Daily')) +
xlab('Date') + ylab('Death Rate (%)') + labs(title='Last two weeks') +
theme(legend.position='bottom', legend.title=element_blank()) +
ylim(0, 10)
grid.arrange(plot1, plot2, ncol=2)


# 5 Processed Data
# Blow is the processed data for this analysis and visualisation.
## re-order columns
# deadIncr, curedIncr,
data %<>% select(c(date, confirmed, deaths, recovered, remaining.confirmed,
confirmed.inc, deaths.inc, recovered.inc, rate.upper, rate.daily, rate.lower))

## to make column names shorter for output purpose only
names(data) %<>% gsub(pattern='Count', replacement='')

## output as a table
data %>% kable("latex", booktabs=T, longtable=T, caption="Cases in the Whole World",
format.args=list(big.mark=",")) %>%
kable_styling(font_size=5, latex_options = c("striped", "hold_position", "repeat_header"))


data.latest %>% arrange(desc(confirmed)) %>%
select(-ranking) %>% filter(country!='Others') %>%
kable("latex", booktabs=T, longtable=T, row.names=T, caption="Cases by Country",
format.args=list(big.mark=",")) %>%
kable_styling(font_size=7, latex_options = c("striped", "hold_position", "repeat_header"))


