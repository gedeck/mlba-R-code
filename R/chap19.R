
if (!require(mlba)) {
  library(devtools)
  install_github("gedeck/mlba/mlba", force=TRUE)
}

# Smoothing and Deep Learning Methods for Forecasting
## Moving Average
### Centered Moving Average for Visualization

Amtrak.data <- mlba::Amtrak

# create time series
ridership.ts <- ts(Amtrak.data$Ridership, start=c(1991,1),
                   end=c(2004,3), freq=12)


library(zoo)
library(forecast)

# centered moving average with window order = 12
ma.centered <- ma(ridership.ts, order = 12)

# trailing moving average with window k = 12
# in rollmean(), use argument align = right to calculate a trailing moving average.
ma.trailing <- rollmean(ridership.ts, k = 12, align = "right")

# generate a plot
colData <- "steelblue"; colModel <- "tomato"
autoplot(ridership.ts, ylab="Ridership", color=colData) +
  autolayer(ma.centered, series="Centered", size=1.1) +
  autolayer(ma.trailing, series="Trailing", size=1.1) +
  scale_color_manual(name="Moving average", values=c("forestgreen", colModel))


  g <- last_plot() +
    scale_x_continuous(n.breaks=10) +
    theme_bw() +
    theme(legend.position="bottom")

  ggsave(file=file.path("..", "figures", "chapter_19", "AmtrakMovingAverage.pdf"),
         g, width=6, height=3.2, units="in")

### Trailing Moving Average for Forecasting

colData <- "steelblue"; colModel <- "tomato"
plotForecast <- function(model, train.ts, test.ts) {
  model.pred <- forecast(model, h=length(test.ts), level=0)
  g <- autoplot(train.ts, xlab="Time", ylab="Ridership (in 000s)", color=colData) +
    autolayer(test.ts, color=colData, linetype=2) +
    autolayer(model$fitted.values, color=colModel, size=0.75) +
    autolayer(model.pred$mean, color=colModel, size=0.75)
  return (g)
}
plotResiduals <- function(model, test.ts) {
  model.pred <- forecast(model, h=length(test.ts), level=0)
  g <- autoplot(model$residuals, xlab="Time", ylab="Forecast Errors", color=colModel, size=0.75) +
    autolayer(test.ts - model.pred$mean, color=colModel, size=0.75) +
    geom_hline(yintercept=0, color="darkgrey") +
    coord_cartesian(ylim=c(-410, 410))
  return (g)
}
nTest <- 36
nTrain <- length(ridership.ts) - nTest

# partition the data
train.ts <- window(ridership.ts, start=c(1991, 1), end=c(1991, nTrain))
test.ts <- window(ridership.ts, start=c(1991, nTrain + 1),
                                   end=c(1991, nTrain + nTest))


# moving average on training with window of size 12
ma.trailing <- rollmean(train.ts, k=12, align="right")

# obtain the last moving average in the training period
last.ma <- tail(ma.trailing, 1)

# create forecast based on last MA
ma.trailing.pred <- ts(rep(last.ma, nTest), start=c(1991, nTrain + 1),
    end=c(1991, nTrain + nTest), freq=12)

# plot the series
autoplot(train.ts, ylab="Ridership", color=colData) +
  autolayer(test.ts, color=colData, linetype=2) +
  autolayer(ma.trailing, color=colModel, size=0.75) +
  autolayer(ma.trailing.pred, color=colModel, size=0.75)


  g <- last_plot() +
    scale_x_continuous(n.breaks=10) +
    theme_bw()
  ggsave(file=file.path("..", "figures", "chapter_19", "AmtrakMAPrediction.pdf"),
         g, width=6, height=3, units="in")


# fit regression model with trend and seasonality
train.lm.trend.season <- tslm(train.ts ~ trend + I(trend^2) + season)

# create single-point forecast
train.lm.trend.season.pred <- forecast(train.lm.trend.season, h = 1)

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
ses <- ets(residuals.ts, model="ANN", alpha=0.2)
ses.pred <- forecast(ses, h=nTest, level=0)

autoplot(residuals.ts, ylab="Residuals", color=colData) +
  autolayer(ses$fitted, color=colModel, size=0.75) +
  autolayer(ses.pred$mean, color=colModel, size=0.75)


  g <- last_plot() +
    scale_x_continuous(n.breaks=10) +
    theme_bw()
  ggsave(file=file.path("..", "figures", "chapter_19", "AmtrakExp.pdf"),
         g, width=6, height=3, units="in")

## Advanced Exponential Smoothing
### Series with a Trend and Seasonality

# run Holt-Winters exponential smoothing
# use ets() with option model = "MAA" to fit Holt-Winter's exponential smoothing
# with multiplicative error, additive trend, and additive seasonality.
hwin <- ets(train.ts, model="MAA")
hwin.pred <- forecast(hwin, h=nTest, level=0)

# plot the series
autoplot(train.ts, ylab="Ridership", color=colData) +
  autolayer(test.ts, color=colData, linetype=2) +
  autolayer(hwin$fitted, color=colModel, size=0.75) +
  autolayer(hwin.pred$mean, color=colModel, size=0.75)


  g <- last_plot() +
    scale_x_continuous(n.breaks=10) +
    theme_bw()
  ggsave(file=file.path("..", "figures", "chapter_19", "AmtrakHoltWinters.pdf"),
         g, width=6, height=3, units="in")


hwin
accuracy(hwin.pred$mean, ridership.ts)

## Deep Learning for Forecasting
### Series with Seasonality (No Trend)

ridership.ts <- ts(mlba::Amtrak$Ridership, start=c(1991, 1), freq=12)
nTest <- 36
nTrain <- length(ridership.ts) - nTest
train.ts <- window(ridership.ts, start=c(1991, 1), end=c(1991, nTrain))
test.ts <- window(ridership.ts, start=c(1991, nTrain + 1),
                                   end=c(1991, nTrain + nTest))




# define function for normalization of training set and it's inverse
minmax <- range(train.ts)
normalize_ts <- function(x) (x - minmax[1]) / (minmax[2] - minmax[1])
inv_normalize_ts <- function(x) (minmax[2] - minmax[1]) * x + minmax[1]
norm_train.ts <- normalize_ts(train.ts)

# convert timeseries into sequence of subseries of length (ninput + noutput)
ninput <- 12
noutput <- 1
nSubsequences <- length(norm_train.ts) - (ninput + noutput) + 1
getSubsequence <- function(i) norm_train.ts[i:(i - 1 + ninput+noutput)]
subsequences <- t(sapply(1:nSubsequences, getSubsequence))

# split subsequences into input (x) and output (y) and convert both to arrays
x.train <- subsequences[, 1:ninput]
y.train <- subsequences[, (ninput+1):(ninput+noutput)]
x.train <- array(data=x.train, dim=c(nrow(x.train), ninput, 1))
y.train <- array(data=y.train, dim=c(nrow(x.train), noutput, 1))
dim(x.train); dim(y.train)


# load required packages
# Keras and TensorFlow require a Python conda environment with these packages installed
library(reticulate)
use_condaenv('mlba-r')
library(keras)
library(tensorflow)

lstm_model <- keras_model_sequential() %>%
  layer_lstm(units = 50, # size of the layer
             batch_input_shape = c(1, ninput, 1), # batch size, timesteps, features
             dropout = 0.01,
             recurrent_dropout = 0.01,
             return_sequences = TRUE,
             stateful = TRUE) %>%
  layer_lstm(units = 50,
             dropout = 0.01,
             recurrent_dropout = 0.01,
             return_sequences = TRUE,
             stateful = TRUE) %>%
  layer_flatten() %>%
  layer_dense(units = 1)
summary(lstm_model)

lstm_model %>%
  compile(loss = 'mae', optimizer = 'adam', metrics = 'mse')


lstm_model %>% fit(
  x = x.train,
  y = y.train,
  batch_size = 1,
  epochs = 400,
  verbose = 1,
  shuffle = TRUE
)


lstm_model %>% save_model_weights_tf("lstm-model.ckpt")


lstm_model %>% load_model_weights_tf("lstm-model.ckpt")


# code for forecasting one month ahead with sliding window
forecast.ts <- c()
window <- norm_train.ts[(nTrain-12):nTrain]
for (i in 1:36) {
  x <- array(data=window, dim=c(1, ninput, 1))
  pred <- predict(lstm_model, x, batch_size=1)
  window <- c(window[2:length(window)], pred[1])
  forecast.ts <- c(forecast.ts, pred[1])
}

forecast.ts <- inv_normalize_ts(forecast.ts)
forecast.ts <- ts(forecast.ts, start=c(1991, 1 + nTrain), freq=12)

fitted <- predict(lstm_model, x.train, batch_size=1)
fitted.ts <- ts(inv_normalize_ts(fitted), start=c(1991, 1 + ninput), freq=12)

autoplot(train.ts, xlab="Time", ylab="Ridership (in 000s)", color=colData) +
  autolayer(test.ts, color=colData, linetype=2) +
  autolayer(fitted.ts, color=colModel, size=0.75) +
  autolayer(forecast.ts, color=colModel, size=0.75)

accuracy(fitted.ts, ridership.ts)
accuracy(forecast.ts, ridership.ts)


  addTrainValid <- function(g, train.ts, test.ts, yh) {
    delta <- 1/12
    date_t <- time(train.ts)[1]
    date_th <- time(test.ts)[1] - delta
    date_hf <- tail(time(test.ts), 1) + delta
    g <- g + geom_vline(xintercept=date_th, color="darkgrey") +
      geom_segment(aes(x=date_t, xend=date_th-delta, y=yh, yend=yh), color="darkgrey") +
      geom_segment(aes(x=date_th+delta, xend=date_hf-delta, y=yh, yend=yh), color="darkgrey")  +
      geom_text(aes(x=(date_t+date_th)/2, y=yh+50, label='Training')) +
      geom_text(aes(x=(date_th+date_hf)/2, y=yh+50, label='Test')) +
      scale_x_continuous(n.breaks=10)
    return (g)
  }

  g <- addTrainValid(last_plot(), train.ts, test.ts, 2300)
  ggsave(file=file.path("..", "figures", "chapter_19", "AmtrakLSTM-pred.pdf"),
         g + theme_bw(), width=6, height=3, units="in")


# one month-ahead forecasts

# create subsequences of length ninput for the last 48 months of the full dataset
norm.ts <- normalize_ts(ridership.ts)
nSubsequences <- nTrain - ninput + 1
getSubsequence <- function(i) norm.ts[i:(i + ninput - 1)]
subsequences <- t(sapply(nSubsequences:(length(norm.ts)-(ninput+noutput) + 1), getSubsequence))

predictions <- c()
for (i in 1:36) {
  x <- array(data=subsequences[i,], dim=c(1, ninput, 1))
  long_forecast <- predict(lstm_model, x, batch_size=1)
  predictions <- c(predictions, long_forecast[1])
}
predictions <- inv_normalize_ts(predictions)
predictions.ts <- ts(predictions, start=c(1991,nTrain+1), freq=12)

g <- autoplot(train.ts, xlab="Time", ylab="Ridership (in 000s)", color=colData) +
  autolayer(test.ts, color=colData, linetype=2) +
  autolayer(fitted.ts, color=colModel, size=0.75) +
  autolayer(predictions.ts, color=colModel, size=0.75)
g

accuracy(fitted.ts, ridership.ts)
accuracy(predictions.ts, ridership.ts)

g <- addTrainValid(last_plot(), train.ts, test.ts, 2300)
ggsave(file=file.path("..", "figures", "chapter_19", "AmtrakLSTM-pred-onemonth.pdf"),
         g + theme_bw(), width=6, height=3, units="in")


library(gridExtra)
sales.ts <- ts(mlba::DepartmentStoreSales$Sales, freq=4)
train.ts <- window(sales.ts, end=c(1, 20))
test.ts <- window(sales.ts, start=c(1, 21))

ses <- ets(train.ts, restrict=FALSE, model="ZMM", alpha=0.2, beta=0.15, gamma=0.05)
ses.pred <- forecast(ses, h=nTest, level=0)

g1 <- autoplot(train.ts, xlab="Year-quarter", ylab="Sales ($)", series="Data") +
  autolayer(ses$fitted, series="Model") +
  scale_x_yearqtr(format = "Y%Y-Q%q") +
  scale_color_manual(name="", values=c(colData, colModel))
g2 <- autoplot(train.ts - ses$fitted, xlab="Year-quarter", ylab="Forecast error") +
  scale_x_yearqtr(format = "Y%Y-Q%q")
grid.arrange(g1, g2, nrow=2)

g <- arrangeGrob(g1 + theme_bw() + theme(legend.position=c(.2,.85), legend.direction='horizontal'),
                 g2 + theme_bw())
ggsave(file=file.path("..", "figures", "chapter_19", "Exercise-DeptStore-ExpSmooth.pdf"),
       g, width=5, height=5, units="in")


shampoo.ts <- ts(mlba::ShampooSales$Shampoo.Sales, start=c(1995,1), freq=12)
autoplot(shampoo.ts, xlab="Year", ylab="Units (in 000s)") +
  geom_point() +
  scale_x_yearmon(format="%Y-%b")
ggsave(file=file.path("..", "figures", "chapter_19", "Exercise-Shampoo.pdf"),
       last_plot() + theme_bw(), width=5, height=3, units="in")


gas.ts <- ts(mlba::NaturalGasSales$Gas.Sales, start=c(2001,1), freq=4)
ma.gas <- rollmean(gas.ts, k=4, align="right")

autoplot(gas.ts, xlab="Season", ylab="Billion BTU") +
  geom_point() +
  autolayer(ma.gas, color="blue") +
  scale_x_yearqtr(format = "%Y-Winter")
ggsave(file=file.path("..", "figures", "chapter_19", "Exercise-Gas.pdf"),
       last_plot() + theme_bw(), width=6, height=3, units="in")
