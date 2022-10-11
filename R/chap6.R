
if (!require(mlba)) {
  library(devtools)
  install_github("gedeck/mlba/mlba", force=TRUE)
}
options(scipen=999, digits = 3)

# Multiple Linear Regression
## Estimating the Regression Equation and Prediction
### Example: Predicting the Price of Used Toyota Corolla Cars

library(caret)
car.df <- mlba::ToyotaCorolla
# select variables for regression
outcome <- "Price"
predictors <- c("Age_08_04", "KM", "Fuel_Type", "HP", "Met_Color", "Automatic",
                "CC", "Doors", "Quarterly_Tax", "Weight")
# reduce data set to first 1000 rows and selected variables
car.df <- car.df[1:1000, c(outcome, predictors)]

# partition data
set.seed(1)  # set seed for reproducing the partition
idx <- createDataPartition(car.df$Price, p=0.6, list=FALSE)
train.df <- car.df[idx, ]
holdout.df <- car.df[-idx, ]

# use lm() to run a linear regression of Price on all 11 predictors in the
# training set.
# use . after ~ to include all the remaining columns in train.df as predictors.
car.lm <- lm(Price ~ ., data = train.df)
#  use options() to ensure numbers are not displayed in scientific notation.
options(scipen = 999)
summary(car.lm)




# use predict() to make predictions on a new set.
pred <- predict(car.lm, holdout.df)

options(scipen=999, digits=0)
data.frame(
  'Predicted' = pred[1:20],
  'Actual' = holdout.df$Price[1:20],
  'Residual' = holdout.df$Price[1:20] - pred[1:20]
)
options(scipen=999, digits = 3)

# calculate performance metrics
rbind(
  Training=mlba::regressionSummary(predict(car.lm, train.df), train.df$Price),
  Holdout=mlba::regressionSummary(pred, holdout.df$Price)
)


library(ggplot2)
pred <- predict(car.lm, holdout.df)
all.residuals <- holdout.df$Price - pred

ggplot() +
    geom_histogram(aes(x=all.residuals), fill="lightgray", color="grey") +
    labs(x="Residuals", y="Frequency")


g <- ggplot() +
         geom_histogram(aes(x=all.residuals), fill="lightgray", color="grey") +
         labs(x="Residuals", y="Frequency") +
         theme_bw()
ggsave(file=file.path("..", "figures", "chapter_06", "residuals-histogram.pdf"),
       g, width=5, height=3, units="in")

### Cross-validation and caret

set.seed(1)
library(caret)
# define 5-fold
trControl <- caret::trainControl(method="cv", number=5, allowParallel=TRUE)
model <- caret::train(Price ~ ., data=car.df,
                      method="lm",  # specify the model
                      trControl=trControl)
model
coef(model$finalModel)


library(tidyverse)
collectMetrics <- function(model, train.df, holdout.df, nPredictors) {
  if (missing(nPredictors)) {
    coefs = coef(model$finalModel)
    nPredictors = length(coefs) - 1
  }
  return (cbind(
    CV=model$results %>% slice_min(RMSE) %>% dplyr::select(c(RMSE, MAE)),
    Training=mlba::regressionSummary(predict(model, train.df), train.df$Price),
    Holdout=mlba::regressionSummary(predict(model, holdout.df), holdout.df$Price),
    nPredictors=nPredictors
  ))
}

metric.full <- collectMetrics(model, train.df, holdout.df)


predict(model, car.df[1:3,])



## Variable Selection in Linear Regression
### How to Reduce the Number of Predictors
#### Exhaustive Search

# use regsubsets() in package leaps to run an exhaustive search.
# unlike with lm, categorical predictors must be turned into dummies manually.
library(leaps)
library(fastDummies)

# create dummies for fuel type
leaps.train.df <- dummy_cols(train.df, remove_first_dummy=TRUE,
                             remove_selected_columns=TRUE)
search <- regsubsets(Price ~ ., data=leaps.train.df, nbest=1,
                     nvmax=ncol(leaps.train.df), method="exhaustive")
sum <- summary(search)

# show models
sum$which

# show metrics
sum$rsq
sum$adjr2
sum$cp


optimal <- which.min(sum$cp)

# determine the variable names for the optimal model
X <- summary(search)$which[, -1]  # information about included predictors
xvars <- dimnames(X)[[2]] ## column names (all covariates except intercept)
xvars <- xvars[X[optimal,]]

# the optimal model contains all dummy variables of Fuel_Type
xvars <- c("Age_08_04", "KM", "HP", "Quarterly_Tax", "Weight", "Fuel_Type")

# rebuild model for best predictor set
set.seed(1)
trControl <- caret::trainControl(method="cv", number=5, allowParallel=TRUE)
model <- caret::train(Price ~ ., data=car.df[, c("Price", xvars)],
                      method="lm",  # specify the model
                      trControl=trControl)
model
coef(model$finalModel)

metric.exhaustive <- collectMetrics(model, train.df, holdout.df)

#### Popular Subset Selection Algorithms

# as model performance is estimated using AIC, we don't need to use cross-validation
trControl <- caret::trainControl(method="none")
model <- caret::train(Price ~ ., data=train.df, trControl=trControl,
                      # select backward elmination
                      method="glmStepAIC", direction='backward')

coef(model$finalModel)


model <- caret::train(Price ~ ., data=train.df, trControl=trControl,
                      method="glmStepAIC", direction='forward')

coef(model$finalModel)


model <- caret::train(Price ~ ., data=train.df, trControl=trControl,
                      method="glmStepAIC", direction='both')

coef(model$finalModel)


rbind(Training=mlba::regressionSummary(predict(model, train.df), train.df$Price),
      Holdout=mlba::regressionSummary(predict(model, holdout.df), holdout.df$Price))



# The models are identical to the best model obtained from the exhaustive search.
# We therefore duplicate the metrics.
metric.stepwise <- metric.exhaustive

## Regularization (Shrinkage Models)

set.seed(1)
library(caret)
trControl <- caret::trainControl(method='cv', number=5, allowParallel=TRUE)
tuneGrid <- expand.grid(lambda=10^seq(5, 2, by=-0.1), alpha=0)
model <- caret::train(Price ~ ., data=train.df,
                 method='glmnet',
                 family='gaussian',  # set the family for linear regression
                 trControl=trControl,
                 tuneGrid=tuneGrid)
model$bestTune
coef(model$finalModel, s=model$bestTune$lambda)


metric.ridge <- collectMetrics(model, train.df, holdout.df,
  length(coef(model$finalModel, s=model$bestTune$lambda)) - 1)
ridge.model <- model


rbind(
  Training=mlba::regressionSummary(predict(model, train.df), train.df$Price),
  Holdout=mlba::regressionSummary(predict(model, holdout.df), holdout.df$Price)
)


set.seed(1)
tuneGrid <- expand.grid(lambda=10^seq(4, 0, by=-0.1), alpha=1)
model <- caret::train(Price ~ ., data=train.df,
                 method='glmnet',
                 family='gaussian',  # set the family for linear regression
                 trControl=trControl,
                 tuneGrid=tuneGrid)
model$bestTune
coef(model$finalModel, s=model$bestTune$lambda)


lasso.model <- model
metric.lasso <- collectMetrics(lasso.model, train.df, holdout.df,
  sum(coef(lasso.model$finalModel, s=lasso.model$bestTune$lambda) != 0) - 1)


rbind(
  Training=mlba::regressionSummary(predict(model, train.df), train.df$Price),
  Holdout=mlba::regressionSummary(predict(model, holdout.df), holdout.df$Price)
)


library(tidyverse)
library(gridExtra)
g1 <- ggplot(ridge.model$results, aes(x=lambda, y=RMSE)) +
    geom_pointrange(aes(ymin=RMSE-RMSESD, ymax=RMSE+RMSESD), color='grey') +
    geom_line() +
    geom_point(data=ridge.model$results %>% subset(RMSE == min(RMSE)), color='red') +
    labs(x=expression(paste('Ridge parameter ', lambda)),
         y='RMSE (cross-validation)') +
    scale_x_log10()
g2 <- ggplot(lasso.model$results, aes(x=lambda, y=RMSE)) +
    geom_pointrange(aes(ymin=RMSE-RMSESD, ymax=RMSE+RMSESD), color='grey') +
    geom_line() +
    geom_point(data=lasso.model$results %>% subset(RMSE == min(RMSE)), color='red') +
    labs(x=expression(paste('Lasso parameter ', lambda)),
         y='RMSE (cross-validation)') +
    scale_x_log10()
grid.arrange(g1, g2, ncol=2)

g <- arrangeGrob(g1 + theme_bw(), g2 + theme_bw(), ncol=2)
ggsave(file=file.path('..', 'figures', 'chapter_06', 'shrinkage-parameter-tuning.pdf'),
       g, width=6, height=2.5, units='in')


data.frame(rbind(
  'full'= metric.full,
  'exhaustive' = metric.exhaustive,
  'stepwise' = metric.stepwise,
  'ridge' = metric.ridge,
  'lasso' = metric.lasso
))
