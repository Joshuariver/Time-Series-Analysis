# Corona in Belgium
# https://www.r-bloggers.com/corona-in-belgium/

library(data.table)
library(lattice)


x <- list.files("csse_covid_19_data/csse_covid_19_daily_reports/", pattern = ".csv", full.names = TRUE)
x <- data.frame(file = x, date = substr(basename(x), 1, 10), stringsAsFactors = FALSE)
x <- split(x$file, x$date)
x <- lapply(x, fread)
x <- rbindlist(x, fill = TRUE, idcol = "date")
x$date <- as.Date(x$date, format = "%m-%d-%Y")
x <- setnames(x, 
              old = c("date", "Country/Region", "Province/State", "Confirmed", "Deaths", "Recovered"),
              new = c("date", "region", "subregion", "confirmed", "death", "recovered"))
x <- subset(x, subregion %in% "Hubei" | 
              region %in% c("Belgium", "France", "Netherlands", "Spain", "Singapore", "Germany", "Switzerland", "Italy"))
x$area <- ifelse(x$subregion %in% "Hubei", x$subregion, x$region)
x <- x[!duplicated(x, by = c("date", "area")), ]
x <- x[, c("date", "area", "confirmed", "death", "recovered")]
subset(x, area %in% "Belgium" & confirmed > 1)

trellis.par.set(strip.background = list(col = "lightgrey"))
xyplot(confirmed ~ date | area, data = x, type = "b", pch = 20, 
       scales = list(y = list(relation = "free", rot = 0), x = list(rot = 45, format = "%A %d/%m")), 
       layout = c(5, 2), main = sprintf("Confirmed cases of Corona\n(last date in this graph is %s)", max(x$date)))

x <- x[order(x$date, x$area, decreasing = TRUE), ]
x <- x[, days_since_case_onset := as.integer(date - min(date[confirmed > 75])), by = list(area)]
x <- x[, newly_confirmed := as.integer(confirmed - shift(confirmed, n = 1, type = "lead")), by = list(area)]
onset <- subset(x, days_since_case_onset == 0, select = c("date", "area", "confirmed"))
onset[order(onset$date), ]

xyplot(log(confirmed) ~ days_since_case_onset | "Log(confirmed cases) of Corona since onset of sick person nr 75", 
       groups = area,
       data = subset(x, days_since_case_onset >= 0 & 
                       area %in% c("Hubei", "France", "Belgium", "Singapore", "Netherlands", "Italy")), 
       xlab = "Days since Corona onset (confirmed case 75)", ylab = "Log of number of confirmed cases",
       auto.key = list(space = "right", lines = TRUE),
       type = "b", pch = 20, lwd = 2) 

xyplot(newly_confirmed ~ date | "Newly confirmed cases of Corona", groups = area,
       data = subset(x, area %in% c("Belgium", "Netherlands") & date > as.Date("2020-03-01")), 
       xlab = "Date", ylab = "Number of new Corona cases",
       scales = list(x = list(rot = 45, format = "%A %d/%m", at = seq(as.Date("2020-03-01"), Sys.Date(), by = "day"))), 
       auto.key = list(space = "right", lines = TRUE),
       type = "b", pch = 20, lwd = 2)
