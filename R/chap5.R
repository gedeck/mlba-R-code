
# TODO: remove once package is available on CRAN
library(devtools)
install_github("gedeck/mlba/mlba", force=TRUE)

# Evaluating Predictive Performance
## Evaluating Predictive Performance
### Prediction Accuracy Measures

library(tidyverse)
set.seed(1)

# load and preprocess file
car.df <- mlba::ToyotaCorolla %>%
    select(-one_of('Id', 'Model', 'Fuel_Type', 'Color')) %>%
    drop_na()


# randomly generate training and validation sets
idx <- caret::createDataPartition(car.df$Price, p=0.6, list=FALSE)
train.df <- car.df[idx, ]
valid.df <- car.df[-idx, ]

# run linear regression model
reg <- lm(Price~., data=train.df)
pred_t <- predict(reg)
pred_v <- predict(reg, newdata=valid.df)

## evaluate performance
# training
caret::RMSE(pred_t, train.df$Price)
# validation
caret::RMSE(pred_v, valid.df$Price)
# use utlitiy function from the mlba package to calculate various metrics at once
rbind(
  Training=mlba::regressionSummary(pred_t, train.df$Price),
  Validation=mlba::regressionSummary(pred_v, valid.df$Price)
)


library(ggplot2)
library(gridExtra)

train.residuals <- train.df$Price - pred_t
valid.residuals <- valid.df$Price - pred_v
res.min <- min(train.residuals, valid.residuals)
res.max <- max(train.residuals, valid.residuals)

g1 <- ggplot() +
    geom_histogram(aes(x=train.residuals), fill='lightgray', color='grey') +
    labs(x='', y='Training') +
    xlim(res.min, res.max) +
    theme_bw()

g2 <- ggplot() +
    geom_histogram(aes(x=valid.residuals), fill='lightgray', color='grey') +
    labs(x='', y='Validation') +
    xlim(res.min, res.max) +
    theme_bw()

df <- data.frame(
  residual=c(train.residuals, valid.residuals),
  role=c(rep('Training', length(train.residuals)), rep('Validation', length(valid.residuals)))
)
g3 <- ggplot(df, aes(x=role, y=residual)) +
    geom_boxplot() +
    labs(x='', y='') +
    theme_bw()

grid.arrange(g1, g2, g3, ncol=3)

g <- arrangeGrob(g1, g2, g3, ncol=3)
ggsave(file=file.path('..', 'figures', 'chapter_05', 'residuals-full-model.pdf'),
       g, width=8, height=2.25, units='in')


### Lift Chart

# predictions
pred_v <- predict(reg, newdata=valid.df)

# load package gains, compute gains (we will use package caret for categorical y later)
library(gains)
gain <- gains(valid.df$Price, pred_v)

# cumulative lift chart
options(scipen=999) # avoid scientific notation
# we will compute the gain relative to price
price <- valid.df$Price
plot(c(0,gain$cume.pct.of.total*sum(price))~c(0,gain$cume.obs),
     xlab="# cases", ylab="Cumulative Price", main="Lift Chart", type="l")

# baseline
lines(c(0,sum(price))~c(0, nrow(valid.df)), col="gray", lty=2)

# Decile-wise lift chart
barplot(gain$mean.resp/mean(price), names.arg = gain$depth,
        xlab = "Percentile", ylab = "Mean Response", main = "Decile-wise lift chart")

## Judging Classifier Performance
### Propensities and Cutoff for Classification

# note: function confusionMatrix requires library caret
library(caret)
owner.df <- mlba::ownerExample
## cutoff = 0.5
confusionMatrix(factor(ifelse(owner.df$Probability>0.5, 'owner', 'nonowner')), owner.df$Class)$table
# note: "reference" = "actual"
## cutoff = 0.25
confusionMatrix(factor(ifelse(owner.df$Probability>0.25, 'owner', 'nonowner')), owner.df$Class)$table
## cutoff = 0.75
confusionMatrix(factor(ifelse(owner.df$Probability>0.75, 'owner', 'nonowner')), owner.df$Class)$table


# replace data.frame with your own
df <- mlba::liftExample

# create empty accuracy table
accT = c()

# compute accuracy per cutoff
for (cut in seq(0,1,0.1)){
  cm <- confusionMatrix(factor(1 * (df$prob > cut), levels=c(0,1)),
                        factor(df$actual, levels=c(0,1)))
  accT = c(accT, cm$overall[1])
}

# plot accuracy
plot(accT ~ seq(0,1,0.1), xlab = "Cutoff Value", ylab = "", type = "l", ylim = c(0, 1))
lines(1-accT ~ seq(0,1,0.1), type = "l", lty = 2)
legend("topright",  c("accuracy", "overall error"), lty = c(1, 2), merge = TRUE)

### Performance in Case of Unequal Importance of Classes

library(pROC)
r <- roc(df$actual, df$prob)
plot.roc(r)

# compute auc
auc(r)

## Judging Ranking Performance
### Lift Charts for Binary Data
#### Sorting by Propensity

# first option with 'caret' library:
library(caret)
lift.example <- lift(relevel(as.factor(actual), ref="1") ~ prob, data = df)
xyplot(lift.example, plot = "gain")

# Second option with 'gains' library:
library(gains)
df <- mlba::liftExample
gain <- gains(df$actual, df$prob, groups=nrow(df))
plot(c(0, gain$cume.pct.of.total*sum(df$actual)) ~ c(0, gain$cume.obs),
     xlab = "# cases", ylab = "Cumulative", type="l")
lines(c(0,sum(df$actual))~c(0,nrow(df)), col="gray", lty=2)

### Decile Lift Charts
#### Interpreting the lift chart

# use gains() to compute deciles.
# when using the caret package, deciles must be computed manually.

gain <- gains(df$actual, df$prob,)
barplot(gain$mean.resp / mean(df$actual), names.arg = gain$depth, xlab = "Percentile",
        ylab = "Mean Response", main = "Decile-wise lift chart")
