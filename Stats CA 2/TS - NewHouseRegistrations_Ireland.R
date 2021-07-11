df = read.csv("C:\\Users\\Sanket\\OneDrive\\Semester 1\\Statistics for Data Analytics\\CA\\CA 2\\NewHouseRegistrations_Ireland.csv", header = TRUE)

library(fpp2)

summary(df)

library('lattice')
library('ggplot2') # visualization
library('ggthemes') # visualization
library('scales') # visualization
library('forecast')
library('TSA')
library('tseries')
library('caret')
df_ts <- ts(df$NewHouseRegistrations, frequency = 1, start = c(1978))
df_ts
summary(df_ts)
plot.ts(df_ts)

#SES model - It has flat forecast funciton
ses_house_reg <- ses(df_ts, h = 2)
plot(ses_house_reg)
round(accuracy(ses_house_reg),3)
autoplot(ses_house_reg)
autoplot(ses_house_reg)+autolayer(fitted(ses_house_reg), series = "Fitted TS")
summary(ses_house_reg)

#Holt
df_ts_holt <- window(df_ts, start = 1978 )
df_ts_holt
plot(df_ts_holt)
reg_house_holt <- holt(df_ts_holt, h = 5)
reg_house_holt
summary(reg_house_holt)
with_damo <- holt(df_ts_holt,damped = FALSE, PI=FALSE, h = 10)
without_damo <- holt(df_ts_holt,damped = TRUE, PI=FALSE, h = 10)
autoplot(df_ts_holt)+autolayer(with_damo, series = "With damp")+autolayer(without_damo, series = "Without damp")
#Holt with ets
ets_holt_damp <- ets(df_ts_holt, model="ZZZ", damped = TRUE)
summary(ets_holt_damp)
ets_holt_no_damp <- ets(df_ts_holt, model="ZZZ", damped = FALSE)
summary(ets_holt_no_damp)
autoplot(df_ts_holt)+autolayer(ets_holt_damp$fitted, series = "Damp = TRUE")+autolayer(ets_holt_no_damp$fitted, series = "Damp = FALSE")+autolayer(forecast(ets_holt_damp, h = 10))+autolayer(forecast(ets_holt_no_damp, h = 10))
forecast(ets_holt_damp, h = 10)
forecast(ets_holt_no_damp, h = 10)

#Holtwinters

plot.ts(df_ts, col = "blue", main = "house reg time series data between 1978 and 2019")
abline(reg=lm(df_ts ~ time(df_ts)),col = "lightgray")

Annual_df = aggregate(df_ts)
Annual_df

plot.ts(Annual_df, col = "blue", main = "Yearly house sales time series data between 1978 and 2019")

plot.ts(Annual_beerSales, col = "blue", main = "Yearly Beer sales time series data between 2000 and 2017")

df_p= periodogram(df)

freq_data <- df_p$freq
spec_data <- df_p$spec
df_frequency_data <- data.frame(spec = spec_data, freq = freq_data)
df_frequency_data[order(df_frequency_data$spec,decreasing = TRUE),]

#Stationarize the Series
#Smoothing the model

library("TTR")
smoothing_df_data <- SMA(df_ts,n=1)
plot.ts(smoothing_df_data)

smoothing_df_dataSMA3 <- SMA(df_ts,n=3)
plot.ts(smoothing_df_dataSMA3)

smoothing_df_dataSMA5 <- SMA(df_ts,n=5)
plot.ts(smoothing_df_dataSMA5)

smoothing_df_dataSMA10 <- SMA(df_ts,n=10)
plot.ts(smoothing_df_dataSMA10)

#Moving Avegrage
plot(df_ts, main= "Raw Time Series")
plot(ma(df_ts,5))

#Simple Exponential smoothing model orginal

housefit <- ses(df_ts, h=2)
housefit

round(accuracy(housefit),2)

autoplot(housefit)
autoplot(housefit) + autolayer(fitted(housefit), series = "Fitted")

#with Smoothing
housefit1 <- ses(smoothing_df_dataSMA5, h=2)
housefit1

#Holt
holt_model <- holt(df_ts, h=8)

plot(holt_model)
summary(holt_model)

round(accuracy(housefit1),2)

summary(housefit1)
autoplot(housefit1)
autoplot(housefit1) + autolayer(fitted(housefit1), series = "Fitted")

#Arima
ggtsdisplay(df_ts)
#The graphs suggest differencing of the data before applying ARMA models.
dftimeseries_1 <- df_ts
ndiffs(df_ts)

ggtsdisplay(df_ts)
acf(df_ts)
pacf(df_ts)
#AR I MA - p d q
fit <- Arima(df_ts, c(0,0,4))
fit

qqnorm(fit$residuals)
qqline(fit$residuals)
Box.test(fit$residuals, type = 'Ljung-Box')
checkresiduals(fit)
summary(fit)


fcast <- forecast(fit, h=8)
fcast
plot(fcast)
fcast

#Auto Arima
auto_arima_fit <- auto.arima(df_ts)
qqnorm(auto_arima_fit$residuals)
qqline(auto_arima_fit$residuals)
Box.test(auto_arima_fit$residuals, type = 'Ljung-Box')
checkresiduals(auto_arima_fit)
summary(auto_arima_fit)
