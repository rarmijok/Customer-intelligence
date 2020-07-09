options("scipen" = 10)
options()$scipen
library(ggplot2)
library(RGoogleAnalytics)
library(scales)
library(lubridate)
library(prophet)
library(dplyr)


load(file="oauth_token")
ValidateToken(oauth_token)
query.list <- Init(start.date = as.Date("2014-01-01"),
                   end.date = as.Date(Sys.Date()),
                   dimensions = "ga:date",
                   metrics = "ga:sessions,ga:itemRevenue",
                   max.results = 1000,
                   sort = "ga:date",
                   filters ="ga:sourceMedium==mail",
                   table.id = "ga:19886580")
ga.query <- QueryBuilder(query.list)
ga.data <- GetReportData(ga.query, oauth_token, split_daywise = T)
ga.data$date <- as.Date(ga.data$date, "%Y%m%d")
ga.data$day <- wday(ga.data$date,label = TRUE)
stats <- subset(ga.data,select=c("date","itemRevenue"))
colnames(stats) <- c("ds", "y")

stats <- subset(ga.data,select=c("date","itemRevenue","sessions"))
colnames(stats) <- c("ds", "x","y")
head(stats)

View(summary(stats))
plot(y ~ ds, stats, type = "l")
m <- prophet(stats)
future <- make_future_dataframe(m, periods = 130)
forecast <- predict(m, future)
plot(m, forecast)
prophet_plot_components(m, forecast)



#####INCORPORAR EVENTOS
cyber <- data_frame(
  holiday = "Cyber",
  ds = as.Date(c("2017-11-06","2017-05-28","2016-11-07", "2016-05-28", "2015-11-16", "2015-05-24", "2014-11-16", "2014-07-28")),
  lower_window = 0,
  upper_window = 4
)
navidad <- data_frame(
 holiday = "Cyber",
 ds = as.Date(c("2017-12-01","2016-12-01","2015-12-01", "2014-12-01", "2015-11-16", "2015-05-24", "2014-11-16", "2014-07-28")),
 lower_window = 0,
 upper_window = 20
)

fechas <- bind_rows(cyber,navidad)
m <- prophet(stats, holidays = fechas,holidays.prior.scale = 15)

forecast <- predict(m, future)
plot(m, forecast)


#### SACAR OUTLIERS
outliers <- (as.Date(stats$ds) > as.Date(‘2014–07–09’)
             & as.Date(stats$ds) < as.Date(‘2014–07–12’))
stats$y[outliers] = NA

