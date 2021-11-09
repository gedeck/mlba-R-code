
# TODO: remove once package is available on CRAN
library(devtools)
install_github("gedeck/mlba/mlba", force=TRUE)

# Combining Methods: Ensembles and Uplift Modeling
## Ensembles
### Bagging and Boosting in R
#### Combining Propensities

library(adabag)
library(rpart)
library(caret)

bank.df <- mlba::UniversalBank
bank.df <- bank.df[ , -c(1, 5)]  # Drop ID and zip code columns.

# transform Personal.Loan into categorical variable
bank.df$Personal.Loan = as.factor(bank.df$Personal.Loan)

# partition the data
train.index <- sample(c(1:dim(bank.df)[1]), dim(bank.df)[1]*0.6)
train.df <- bank.df[train.index, ]
valid.df <- bank.df[-train.index, ]

# single tree
tr <- rpart(Personal.Loan ~ ., data = train.df)
pred <- predict(tr, valid.df, type = "class")
confusionMatrix(pred, valid.df$Personal.Loan)

# bagging
bag <- bagging(Personal.Loan ~ ., data = train.df)
pred <- predict(bag, valid.df, type = "class")
confusionMatrix(as.factor(pred$class), valid.df$Personal.Loan)

# boosting
boost <- boosting(Personal.Loan ~ ., data = train.df)
pred <- predict(boost, valid.df, type = "class")
confusionMatrix(as.factor(pred$class), valid.df$Personal.Loan)
