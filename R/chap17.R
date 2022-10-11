
if (!require(mlba)) {
  library(devtools)
  install_github("gedeck/mlba/mlba", force=TRUE)
}

# Handling Time Series
## Time Series Components
### Example: Ridership on Amtrak Trains

library(forecast)
library(ggplot2)
Amtrak.data <- mlba::Amtrak

# create time series object using ts()
# ts() takes three arguments: start, end, and freq.
# with monthly data, the frequency of periods per cycle is 12 (per year).
# arguments start and end are (cycle [=year] number, seasonal period [=month] number) pairs.
# here start is Jan 1991: start = c(1991, 1); end is Mar 2004: end = c(2004, 3).
ridership.ts <- ts(Amtrak.data$Ridership,
    start = c(1991, 1), end = c(2004, 3), freq = 12)

# plot the series using the autoplot function to make use of ggplot
autoplot(ridership.ts, xlab="Time", ylab="Ridership (in 000s)") +
  scale_y_continuous(limits=c(1300, 2300))


  g <- last_plot() +
    scale_x_continuous(n.breaks=10) +
    theme_bw()
  ggsave(file=file.path("..", "figures", "chapter_17", "AmtrakFirstPlot.pdf"),
         g, width=6, height=3, units="in")


  library(gridExtra)
  library(lubridate)
  library(zoo)

  BareggTunnel <- mlba::BareggTunnel
  # convert Day information to a dates object
  dates <- as.POSIXct(BareggTunnel$Day, format='%d %b %Y')
  tunnel.ts <- ts(BareggTunnel$Number.of.vehicles,
                  start=c(2003, yday(dates[1])),
                  frequency=365)

  options(scipen=999)
  g1 <- autoplot(tunnel.ts, xlab="Time", ylab="Number of vehicles") +
    scale_x_yearmon() +
    scale_y_continuous(labels = scales::comma)
  g2 <- autoplot(window(tunnel.ts,
                 start=c(2004, yday(ISOdate(2004, 2, 1))),
                 end=c(2004, yday(ISOdate(2004, 6, 1)))),
                 xlab="Time", ylab="Number of vehicles") +
    scale_x_yearmon() +
    scale_y_continuous(labels = scales::comma)

  grid.arrange(g1, g2, nrow=2)
  g <- arrangeGrob(g1 + theme_bw(), g2 + theme_bw())
  ggsave(file=file.path("..", "figures", "chapter_17", "TS-TunnelPlots.pdf"), g,
           width=6, height=4)


library(gridExtra)

# to zoom in to a certain period, use window() to create a new, shorter time series
# we create a new, 3-year time series of ridership.ts from Jan 1997 to Dec 1999
ridership.ts.3yrs <- window(ridership.ts, start = c(1997, 1), end = c(1999, 12))
g1 <- autoplot(ridership.ts.3yrs, xlab="Time", ylab="Ridership (in 000s)") +
  scale_y_continuous(limits=c(1300, 2300))

# fit a trend line to the time series
g2 <- autoplot(ridership.ts, xlab="Time", ylab="Ridership (in 000s)") +
  scale_y_continuous(limits=c(1300, 2300)) +
  geom_smooth(method="lm", formula=y~poly(x, 2))

grid.arrange(g1, g2, nrow=2)


  g <- arrangeGrob(g1 + theme_bw(),
                   g2 + scale_x_continuous(n.breaks=10) + theme_bw())
  ggsave(file=file.path("..", "figures", "chapter_17", "AmtrakZoomPlots.pdf"), g,
         width=6, height=6)

# we can also use tslm to create the quadratic fit
ridership.lm <- tslm(ridership.ts ~ trend + I(trend^2))
autoplot(ridership.ts, xlab="Time", ylab="Ridership (in 000s)") +
  scale_y_continuous(limits=c(1300, 2300)) +
  autolayer(ridership.lm$fitted.values)

## Data Partitioning and Performance Evaluation
### Benchmark Performance: Naive Forecasts

nTest <- 36
nTrain <- length(ridership.ts) - nTest

# partition the data
train.ts <- window(ridership.ts, start = c(1991, 1), end = c(1991, nTrain))
test.ts <- window(ridership.ts, start = c(1991, nTrain + 1),
                                   end = c(1991, nTrain + nTest))

# generate the naive and seasonal naive forecasts
naive.pred <- naive(train.ts, h=nTest)
snaive.pred <- snaive(train.ts, h=nTest)

# compare the actual values and forecasts for both methods
colData <- "steelblue"; colModel <- "tomato"
autoplot(train.ts, xlab="Time", ylab="Ridership (in 000s$)", color=colData) +
  autolayer(test.ts, linetype=2, color=colData) +
  autolayer(naive.pred, PI=FALSE, color=colModel, size=0.75) +
  autolayer(snaive.pred, PI=FALSE, color=colModel, size=0.75)


# for the book visualization add additional annotation
delta <- 1/12
date_t <- time(train.ts)[1]
date_th <- time(test.ts)[1] - delta
date_hf <- tail(time(test.ts), 1) + delta
g <- last_plot() +
  geom_vline(xintercept=date_th, color="darkgrey") + geom_vline(xintercept=date_hf, color="darkgrey") +
  geom_segment(aes(x=date_t, xend=date_th-delta, y=2300, yend=2300), color="darkgrey") +
  geom_segment(aes(x=date_th+delta, xend=date_hf-delta, y=2300, yend=2300), color="darkgrey")  +
  geom_segment(aes(x=date_hf+delta, xend=date_hf+2, y=2300, yend=2300), color="darkgrey") +
  geom_text(aes(x=(date_t+date_th)/2, y=2350, label='Training')) +
  geom_text(aes(x=(date_th+date_hf)/2, y=2350, label='Test')) +
  geom_text(aes(x=date_hf+1, y=2350, label='Future')) +
  scale_x_continuous(n.breaks=10) +
  theme_bw()
ggsave(file=file.path("..", "figures", "chapter_17", "AmtrakNaive.pdf"),
       g, width=7, height=4.5)


accuracy(naive.pred, test.ts)
accuracy(snaive.pred, test.ts)

### Generating Future Forecasts

ts <- ts(mlba::CanadianWorkHours$Hours,
    start = c(mlba::CanadianWorkHours$Year[1], 1), freq = 1)

# plot the series using the autoplot function to make use of ggplot
autoplot(ts, xlab="Year", ylab="Hours per week")
ggsave(file=file.path("..", "figures", "chapter_17", "Exercise-CanadianWorkers.pdf"),
         last_plot() + theme_bw(), width=5, height=4, units="in")
