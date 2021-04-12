install.packages("forecast")
install.packages("aTSA")
library(quantmod)
library(lubridate)
library(aTSA)
library(lmtest)
library(forecast)
library(stats)


#extract data from FRED, choose sample period
indpro <- new.env()
date.start <- "2010-01-01"
date.end <- "2019-01-01"
ticker <- ("INDPRO")
getSymbols(ticker,src="FRED",env = indpro)
indp <- ind[paste(date.start,date.end,sep="/")]



#plot time series, ACF and PACF of IP. Test stationary of IP
plot(indp, main = "IP Time Series", xlab = "Date", ylab = "Value", col = "green")

acf(indp, main = "IP ACF", lag.max = 50)
pacf(indp, main = "IP PACF", lag.max = 50)

stationary.test(indp, method = c("adf"), nlag = 1)
print("My null hypothesis is non-stationary.If p-value is less than 0.05, we are easy to reject null hypothesis. Our p-value is 0.99 which is significant than 0.05=> We do not reject null hypothesis")



#diff IP over one time lag
ip    = diff(indp,lag = 1)
newip = ip[2:109,]


plot(newip, main = "IP 1 Lag Time Series", xlab = "Date", ylab = "Change in Value", col = "blue")


acf(newip, main = "IP ACF", lag.max = 50)
pacf(newip, main = "IP PACF", lag.max = 50)

stationary.test(newip, method = c("adf"), nlag = 1)



#Trend Pattern for Time Series with no lag difference
print("In the time series plot of the industrial production index, the plot shows an obvious upward trend. The total output has increased in term of production in recent years.")

#Plot for difference of IP over one time lag
print("In the difference IP over one time lag, the plot shows no trend or no seasonality.")



#AIC or BIC to identify optimal ARIMA model
model <- auto.arima(indp, trace=TRUE, method = "ML")
print("The best model is ARIMA (0,1,0) with drift")



#Ljung-Box test to evaluate the serial correlation of residuals
resid = model$residuals
acf(resid,xlim=c(1,20))
Box.test(resid,type="Ljung-Box")
tsdiag(model)

#Forecast 12 months ahead of IP 
pred = forecast(model, h=12)
plot(pred)

