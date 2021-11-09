
# TODO: remove once package is available on CRAN
library(devtools)
install_github("gedeck/mlba/mlba", force=TRUE)

# Regression-Based Forecasting
## A Model with Trend
### Linear Trend

library(forecast)
Amtrak.data <- mlba::Amtrak

# create time series
ridership.ts <- ts(Amtrak.data$Ridership, start = c(1991,1),
                   end = c(2004,3), freq = 12)

# produce linear trend model
ridership.lm <- tslm(ridership.ts ~ trend)

# plot the series
plot(ridership.ts, xlab = "Time", ylab = "Ridership", ylim = c(1300,2300), bty = "l")
lines(ridership.lm$fitted, lwd = 2)


nValid <- 36
nTrain <- length(ridership.ts) - nValid

# partition the data
train.ts <- window(ridership.ts, start = c(1991, 1), end = c(1991, nTrain))
valid.ts <- window(ridership.ts, start = c(1991, nTrain + 1),
                                 end = c(1991, nTrain + nValid))


# fit linear trend model to training set and create forecasts
train.lm <- tslm(train.ts ~ trend)
train.lm.pred <- forecast(train.lm, h = nValid, level = 0)

par(mfrow = c(2, 1))
plot(train.lm.pred, ylim = c(1300, 2600),  ylab = "Ridership", xlab = "Time",
     bty = "l", xaxt = "n", xlim = c(1991,2006.25), main = "", flty = 2)
axis(1, at = seq(1991, 2006, 1), labels = format(seq(1991, 2006, 1)))
lines(train.lm.pred$fitted, lwd = 2, col = "blue")
lines(valid.ts)
plot(train.lm.pred$residuals, ylim = c(-420, 500),  ylab = "Forecast Errors",
    xlab = "Time", bty = "l", xaxt = "n", xlim = c(1991,2006.25), main = "")
axis(1, at = seq(1991, 2006, 1), labels = format(seq(1991, 2006, 1)))
lines(valid.ts - train.lm.pred$mean, lwd = 1)


summary(train.lm)

### Exponential Trend

# fit exponential trend using tslm() with argument lambda = 0
train.lm.expo.trend <- tslm(train.ts ~ trend, lambda = 0)
train.lm.expo.trend.pred <- forecast(train.lm.expo.trend, h = nValid, level = 0)

# fit linear trend using tslm() with argument lambda = 1 (no transform of y)
train.lm.linear.trend <- tslm(train.ts ~ trend, lambda = 1)
train.lm.linear.trend.pred <- forecast(train.lm.linear.trend, h = nValid, level = 0)

plot(train.lm.expo.trend.pred, ylim = c(1300, 2600),  ylab = "Ridership",
 xlab = "Time", bty = "l", xaxt = "n", xlim = c(1991,2006.25), main = "", flty = 2)
axis(1, at = seq(1991, 2006, 1), labels = format(seq(1991, 2006, 1)))
lines(train.lm.expo.trend.pred$fitted, lwd = 2, col = "blue")  # Added in 6-5
lines(train.lm.linear.trend.pred$fitted, lwd = 2, col = "black", lty = 3)
lines(train.lm.linear.trend.pred$mean, lwd = 2, col = "black", lty = 3)
lines(valid.ts)

### Polynomial Trend

# fit quadratic trend using function I(), which treats an object "as is".
train.lm.poly.trend <- tslm(train.ts ~ trend + I(trend^2))
summary(train.lm.poly.trend)
train.lm.poly.trend.pred <- forecast(train.lm.poly.trend, h = nValid, level = 0)

par(mfrow = c(2,1))
plot(train.lm.poly.trend.pred, ylim = c(1300, 2600),  ylab = "Ridership",
 xlab = "Time", bty = "l", xaxt = "n", xlim = c(1991,2006.25), main = "", flty = 2)
axis(1, at = seq(1991, 2006, 1), labels = format(seq(1991, 2006, 1)))
lines(train.lm.poly.trend$fitted, lwd = 2)
lines(valid.ts)

plot(train.lm.poly.trend$residuals, ylim = c(-400, 550),  ylab = "Forecast Errors",
 xlab = "Time", bty = "l", xaxt = "n", xlim = c(1991,2006.25), main = "")
axis(1, at = seq(1991, 2006, 1), labels = format(seq(1991, 2006, 1)))
lines(valid.ts - train.lm.poly.trend.pred$mean, lwd = 1)

## A Model with Seasonality

train.lm.season <- tslm(train.ts ~ season)
summary(train.lm.season)

## A Model with Trend and Seasonality

train.lm.trend.season <- tslm(train.ts ~ trend + I(trend^2) + season)
summary(train.lm.trend.season)

## Autocorrelation and ARIMA Models
### Computing Autocorrelation

ridership.24.ts <- window(train.ts, start = c(1991, 1), end = c(1991, 24))
Acf(ridership.24.ts, lag.max = 12, main = "")

### Improving Forecasts by Integrating Autocorrelation Information

# fit linear regression with quadratic trend and seasonality to Ridership
train.lm.trend.season <- tslm(train.ts ~ trend + I(trend^2) + season)

# fit AR(1) model to training residuals
# use Arima() in the forecast package to fit an ARIMA model
# (that includes AR models); order = c(1,0,0) gives an AR(1).
train.res.arima <- Arima(train.lm.trend.season$residuals, order = c(1,0,0))
valid.res.arima.pred <- forecast(train.res.arima, h = 1)


summary(train.res.arima)
valid.res.arima.pred


plot(train.lm.trend.season$residuals, ylim = c(-250, 250),  ylab = "Residuals",
    xlab = "Time", bty = "l", xaxt = "n", xlim = c(1991,2006.25), main = "")
axis(1, at = seq(1991, 2006, 1), labels = format(seq(1991, 2006, 1)))
lines(valid.res.arima.pred$fitted, lwd = 2, col = "blue")

### Evaluating Predictability

sp500.df <- mlba::SP500
sp500.ts <- ts(sp500.df$Close, start = c(1995, 5), end = c(2003, 8), freq = 12)
sp500.arima <- Arima(sp500.ts, order = c(1,0,0))
sp500.arima
