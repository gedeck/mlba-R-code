
# TODO: remove once package is available on CRAN
library(devtools)
install_github("gedeck/mlba/mlba", force=TRUE)

# Smoothing Methods
## Moving Average
### Centered Moving Average for Visualization

Amtrak.data <- mlba::Amtrak

# create time series
ridership.ts <- ts(Amtrak.data$Ridership, start = c(1991,1),
                   end = c(2004,3), freq = 12)


library(zoo)
library(forecast)

# centered moving average with window order = 12
ma.centered <- ma(ridership.ts, order = 12)

# trailing moving average with window k = 12
# in rollmean(), use argument align = right to calculate a trailing moving average.
ma.trailing <- rollmean(ridership.ts, k = 12, align = "right")

# generate a plot
plot(ridership.ts, ylim = c(1300, 2200),  ylab = "Ridership",
    xlab = "Time", bty = "l", xaxt = "n",
    xlim = c(1991,2004.25), main = "")
axis(1, at = seq(1991, 2004.25, 1), labels = format(seq(1991, 2004.25, 1)))
lines(ma.centered, lwd = 2)
lines(ma.trailing, lwd = 2, lty = 2)
legend(1994,2200, c("Ridership","Centered Moving Average", "Trailing Moving Average"),
    lty=c(1,1,2), lwd=c(1,2,2), bty = "n")

### Trailing Moving Average for Forecasting

# partition the data
nValid <- 36
nTrain <- length(ridership.ts) - nValid
train.ts <- window(ridership.ts, start = c(1991, 1), end = c(1991, nTrain))
valid.ts <- window(ridership.ts, start = c(1991, nTrain + 1),
    end = c(1991, nTrain + nValid))

# moving average on training
ma.trailing <- rollmean(train.ts, k = 12, align = "right")

# obtain the last moving average in the training period
last.ma <- tail(ma.trailing, 1)

# create forecast based on last MA
ma.trailing.pred <- ts(rep(last.ma, nValid), start = c(1991, nTrain + 1),
    end = c(1991, nTrain + nValid), freq = 12)

# plot the series
plot(train.ts, ylim = c(1300, 2600),  ylab = "Ridership", xlab = "Time", bty = "l",
    xaxt = "n", xlim = c(1991,2006.25), main = "")
axis(1, at = seq(1991, 2006, 1), labels = format(seq(1991, 2006, 1)))
lines(ma.trailing, lwd = 2, col = "blue")
lines(ma.trailing.pred, lwd = 2, col = "blue", lty = 2)
lines(valid.ts)


# fit regression model with trend and seasonality
train.lm.trend.season <- tslm(train.ts ~ trend + I(trend^2) + season)

# create single-point forecast
train.lm.trend.season.pred <- forecast(train.lm.trend.season, h = 1, level = 0)

# apply MA to residuals
ma.trailing <- rollmean(train.lm.trend.season$residuals, k = 12, align = "right")
last.ma <- tail(ma.trailing, 1)


train.lm.trend.season.pred
last.ma

## Simple Exponential Smoothing
### Choosing Smoothing Parameter $alpha$

# get residuals
residuals.ts <- train.lm.trend.season$residuals

# run simple exponential smoothing
# use ets() with model = "ANN" (additive error (A), no trend (N), no seasonality (N))
# and alpha = 0.2 to fit simple exponential smoothing.
ses <- ets(residuals.ts, model = "ANN", alpha = 0.2)
ses.pred <- forecast(ses, h = nValid, level = 0)

plot(ses.pred, ylim = c(-250, 300),  ylab = "Ridership", xlab = "Time",
     bty = "l", xaxt = "n", xlim = c(1991,2006.25), main = "", flty = 2)
lines(train.lm.trend.season.pred$fitted, lwd = 2, col = "blue")
axis(1, at = seq(1991, 2006, 1), labels = format(seq(1991, 2006, 1)))
lines(ses.pred$fitted, lwd = 2, col = "blue")
lines(valid.ts)

## Advanced Exponential Smoothing
### Series with Seasonality (No Trend)

# run Holt-Winters exponential smoothing
# use ets() with option model = "MAA" to fit Holt-Winter's exponential smoothing
# with multiplicative error, additive trend, and additive seasonality.
hwin <- ets(train.ts, model = "MAA")

# create predictions
hwin.pred <- forecast(hwin, h = nValid, level = 0)

# plot the series
plot(hwin.pred, ylim = c(1300, 2600),  ylab = "Ridership", xlab = "Time",
    bty = "l", xaxt = "n", xlim = c(1991,2006.25), main = "", flty = 2)
axis(1, at = seq(1991, 2006, 1), labels = format(seq(1991, 2006, 1)))
lines(hwin.pred$fitted, lwd = 2, col = "blue")
lines(valid.ts)


hwin
