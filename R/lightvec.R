library(readr)
library(fpp2)
library(tseries)
library(forecast)

LWVS <- read_csv("LWVS.csv")
tsdata_LWVS <- ts(LWVS[,-1], start = c(1976,1), end = c(2022,9), frequency = 12)

autoplot(tsdata_LWVS, main = "Light Weight Vehicle Sales from Jan-1976 to Sep-2022", ylab = "Thousands of Unit, ('000)", xlab = "Time")
ggtsdisplay(tsdata_LWVS) 

ndiffs(tsdata_LWVS)
nsdiffs(tsdata_LWVS)

sdiff.LWVS <- diff(tsdata_LWVS, 12)
ggtsdisplay(sdiff.LWVS)

nsdiffs(sdiff.LWVS)
ndiffs(sdiff.LWVS)

LWVS_arima300310 <- Arima(tsdata_LWVS, order = c(3,0,0), seasonal = c(3,1,0))
LWVS_arima300310

LWVS_arima200310 <- Arima(tsdata_LWVS, order = c(2,0,0), seasonal = c(3,1,0))
LWVS_arima200310

LWVS_arima202313 <- Arima(tsdata_LWVS, order = c(2,0,2), seasonal = c(3,1,3))
LWVS_arima202313

LWVS_arima102313 <- Arima(tsdata_LWVS, order = c(1,0,2), seasonal = c(3,1,3))
LWVS_arima102313

LWVS_arima102312 <- Arima(tsdata_LWVS, order = c(1,0,2), seasonal = c(3,1,2))
LWVS_arima102312

LWVS_arima201212 <- Arima(tsdata_LWVS, order = c(2,0,1), seasonal = c(2,1,2))
LWVS_arima201212

checkresiduals(LWVS_arima102313)

forecast(LWVS_arima102313, h = 36)
autoplot(forecast(LWVS_arima102313, h = 36))
