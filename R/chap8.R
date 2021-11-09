
# TODO: remove once package is available on CRAN
library(devtools)
install_github("gedeck/mlba/mlba", force=TRUE)
options(scipen=999, digits = 3)

# The Naive Bayes Classifier

library(mlba)
library(tidyverse)
library(e1071)
library(caret)
library(gains)

## Solution: Naive Bayes
### Example 3: Predicting Delayed Flights

# load and preprocess dataset
delays.df <- mlba::FlightDelays %>%
  mutate(
    # change numerical variables to categorical
    DAY_WEEK = factor(DAY_WEEK),
    ORIGIN = factor(ORIGIN),
    DEST = factor(DEST),
    CARRIER = factor(CARRIER),
    Flight.Status = factor(Flight.Status),
    # create hourly bins for departure time
    CRS_DEP_TIME = factor(round(CRS_DEP_TIME / 100))
  ) %>%
  select(DAY_WEEK, CRS_DEP_TIME, ORIGIN, DEST, CARRIER, Flight.Status)

# create training and holdout sets
set.seed(1)
idx <- createDataPartition(delays.df$Flight.Status, p=0.6, list=FALSE)
train.df <- delays.df[idx, ]
holdout.df <- delays.df[-idx, ]

# run naive bayes
delays.nb <- naiveBayes(Flight.Status ~ ., data = train.df)
delays.nb


# use prop.table() with margin = 1 to convert a count table to a proportions table,
# where each row sums up to 1 (use margin = 2 for column sums)
prop.table(table(train.df$Flight.Status, train.df$DEST), margin = 1)


## predict probabilities
pred.prob <- predict(delays.nb, newdata=holdout.df, type="raw")
## predict class membership
pred.class <- predict(delays.nb, newdata=holdout.df)

df <- data.frame(actual=holdout.df$Flight.Status, predicted=pred.class, pred.prob)

df[holdout.df$CARRIER == "DL" & holdout.df$DAY_WEEK == 7 & holdout.df$CRS_DEP_TIME == 10 &
     holdout.df$DEST == "LGA" & holdout.df$ORIGIN == "DCA",]


# training
confusionMatrix(predict(delays.nb, newdata=train.df), train.df$Flight.Status)

# holdout
confusionMatrix(predict(delays.nb, newdata=holdout.df), holdout.df$Flight.Status)


actual <- ifelse(holdout.df$Flight.Status == "delayed", 1, 0)
gain <- gains(actual, pred.prob[,"delayed"], groups=length(actual) - 2)

nactual <-sum(actual)
ggplot() +
  geom_line(aes(x=gain$cume.obs, y=gain$cume.pct.of.total * nactual)) +
  geom_line(aes(x=c(0, max(gain$cume.obs)), y=c(0, nactual)), color="darkgrey") +
  labs(x="# cases", y="Cumulative")


ggsave(file=file.path("..", "figures", "chapter_08", "Flights-NB-gain.pdf"),
       width=3, height=3,
       last_plot() + theme_bw())

### 

  (p_delayed = dnorm(213, mean=211.36215, sd=15.31))
  (p_ontime = dnorm(213, mean=211.99436, sd=12.79))

  p_ontime * 0.805
  p_delayed * 0.195
