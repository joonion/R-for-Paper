# 시계열 데이터: Time-Series Data

data(co2)
str(co2)
class(co2)
co2
head(co2)
head(co2, 3*12)

plot(co2)
plot(co2, lwd=2, col="tomato")

start(co2)
end(co2)
frequency(co2)
deltat(co2)

time(co2)
cycle(co2)

window(co2, start=c(1990, 1), end=c(1995, 12))
window(co2, start=c(1990, 1), frequency = 1)
window(co2, start=c(1990, 1), frequency = 2)
window(co2, start=c(1990, 1), frequency = 4)

# 시계열 데이터 분해: Time Series Decomposition

data(nhtemp)
str(nhtemp)
nhtemp
?nhtemp

windows(width=8, height=4.5)
plot(nhtemp, type='l', lwd=3, col="blue",
     main="Average Yearly Temperatures in New Haven",
     xlab="year", ylab="temperature", las=1)

windows(9, 9)
library(forecast)
par(mfrow=c(4,1))
plot(ma(nhtemp, 1), lwd=2, col="black")
plot(ma(nhtemp, 4), lwd=2, col="green3")
plot(ma(nhtemp, 7), lwd=2, col="blue")
plot(ma(nhtemp, 10), lwd=2, col="red")
par(mfrow=c(1,1))

data(co2)
str(co2)
head(co2, n= 3*12)

ts <- window(co2, start=c(1985, 1), end=c(1996, 12))
head(ts, n=3*12)

stl(ts, s.window="periodic")

decomp <- stl(ts, s.window="periodic")
plot(decomp, lwd=2, col="darkcyan", col.range="skyblue",
     main="Decomposion of CO2 Concentration Time Series")

head(ts, n=3*12)
head(decomp$time.series)

co2.adj <- ts - decomp$time.series[, "seasonal"]
head(co2.adj, 3*12)

plot(co2.adj, lwd=2, col="tomato",
     main="CO2 Concentration Time Series without Sesonal Effect")

library(stats)
monthplot(ts, lwd=2, col="slateblue")

library(forecast)
seasonplot(ts, lwd=2, col="sienna", year.labels = T)


# 평활법 예측 모델

# ARIMA 예측 모델

data(Nile)
str(Nile)
head(Nile)

lag(Nile, 1)
lag(Nile, 2)

library(tseries)
adf.test(Nile)

library(forecast)
ndiffs(Nile)

diff.Nile <- diff(Nile)
adf.test(diff.Nile)

par(mfrow=c(2,1))
plot(Nile, lwd=2)
plot(diff.Nile, lwd=2)
par(mfrow=c(1, 1))

Acf(diff.Nile, lwd=2)
Pacf(diff.Nile, lwd=2)

Acf(diff.Nile, lwd=2, plot=F)
Pacf(diff.Nile, lwd=2, plot=F)

arima(Nile, order=c(0, 1, 1))
Nile.arima <- arima(Nile, order=c(0, 1, 1))
accuracy(Nile.arima)

residual <- Nile.arima$residuals
par(mfrow=c(1, 2))

hist(residual, prob=T)
xfit <- seq(min(residual), max(residual), length=40)
yfit <- dnorm(xfit, mean(residual), sd(residual))
lines(xfit, yfit, lwd=2, col="tomato")

qqnorm(residual, pch=21, col="black", bg="gold")
qqline(residual, col="royalblue", lwd=2)

par(mfrow=c(1, 1))

Box.test(Nile.arima$residuals, type="Ljung-Box")

library(forecast)
data(gas)
str(gas)
head(gas)
head(gas, 3*12)

auto.arima(gas)
gas.arima <- auto.arima(gas)
accuracy(gas.arima)

arima(gas, order=c(2, 1, 1), 
      seasonal = list(order=c(0, 1, 1), period=12))

forecast(gas.arima, h=5*12)
fc <- forecast(gas.arima, h=5*12)
plot(fc)
plot(fc, col="darkorange", lwd=2,
     flty=1, flwd=3, fcol="orangered",
     shadecols=c("lavender", "skyblue"),
     main="Austrian Monthly Gas Production",
     xlab="Year", ylab="Monthly Production")
