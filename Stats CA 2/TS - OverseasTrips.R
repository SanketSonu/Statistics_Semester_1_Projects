rm(list=ls())
over_sea_trips_ts <- read.csv("C:\\Users\\Sanket\\OneDrive\\Semester 1\\Statistics for Data Analytics\\CA\\CA 2\\OverseasTrips.csv", header=T, na.strings=c(""), stringsAsFactors = T)
head(over_sea_trips_ts,10)
summary(over_sea_trips_ts)
#Creating time series object using ts function
dt<-ts(over_sea_trips_ts$Trips.Thousands.,start = c(2012,1), frequency = 4)
plot(dt)

seasonal_pattern <- ggseasonplot(dt, year.labels = TRUE, year.labels.left = TRUE) + ylab("Distance in KMS") + ggtitle("Seasonal Plot: Overseas Trips")
plot(seasonal_pattern)

seasonal_sub <- ggsubseriesplot(dt) + ylab("Distance in KMS") + ggtitle("Seasonal Subseries Plot: Overseas Trips")
plot(seasonal_sub)


plot(log(dt))
dec_dt <- decompose(dt)
plot(dec_dt)
library(fpp2)
library(tseries)
library(ggplot2)
dt_dec <- stl(log10(dt), s.window = 4)
plot(dt_dec)


train_dt <- window(dt, start = c(2012,1), end = c(2017,4), frequency = 4)
test_dt <- window(dt, start = c(2018,1), frequency = 4)
#decompose with stl(Seasonal time series decomposition using loess) function
train_dec <- stl(log10(dt), s.window = 4)
plot(train_dec)
library(forecast)
#Random walk
train_forecast <- forecast(train_dec, method = 'rwdrift', h = 8)
#PLot forecast
plot(train_forecast)
plot(train_dec)
res_df <- 10^(cbind(log10(test_dt),as.data.frame(forecast(train_dec, method = 'rwdrift', h = 8))[,1]))
res_df

# Seasonal naive
fcast.naive <- snaive(dt, h=8)
summary(fcast.naive)
plot(fcast.naive)

#Holt winters
#ets
holt_2 <- ets(dt, model = "ZZZ")
holt_2$model
plot(holt_2)
summary(holt_2)
plot(forecast(holt_2, h=8))


#Holt
#holt_model <- holt(dt, h=8)
#holt_model$model
#plot(holt_model)
#summary(holt_model)

#ets
#holt_2 <- ets(train_dt, model = "AAN")
#holt_2$model
#plot(holt_2)
#summary(holt_2)
#forecast(holt_2, h=8)

#Holt Winters model
#hw_model <- hw(dt,h = 8)
#summary(hw_model)
#test_dt

#Arima
ggtsdisplay(dt)
#Check number of differences required
ndiffs(dt)
nsdiffs(dt)
#The graphs suggest differencing of the data before applying ARIMA models.
dt_1 <- dt
dt_diif <- diff(dt_1)
plot(dt_diif)
kpss.test(dt_diif)

#dt_1 <- dt
#dt_diif <- diff(dt_diif)
#plot(dt_diif)
#adf.test(dt_diif)
#kpss.test(dt_diif)

#acf(dt_diif)
#pacf(dt_diif)
#ggtsdisplay(dt_diif)

#fit <- Arima(train_dt, c(2,1,0))
#checkresiduals(fit)

#fcast <- forecast(fit, h=8)
#fcast
#plot(fcast)
#fcast
#accuracy(fit)
#summary(fit)

#Auto arima
fit.overseas <- auto.arima(dt)
qqnorm(fit.overseas$residuals)
qqline(fit.overseas$residuals)
Box.test(fit.overseas$residuals, type = 'Ljung-Box')
checkresiduals(fit.overseas)
summary(fit.overseas)
acf(dt)
pacf(dt)
