
if (!require(mlba)) {
  library(devtools)
  install_github("gedeck/mlba/mlba", force=TRUE)
}
options(scipen=999)

# Overview of the Machine Learning Process
## Preliminary Steps 
### Loading and Looking at the Data in R

housing.df <- read.csv('WestRoxbury.csv') # load data from file
housing.df <- mlba::WestRoxbury # load data from mlba package
dim(housing.df)  # find the dimension of data frame
head(housing.df)  # show the first six rows
View(housing.df)  # show all the data in a new tab

# Practice showing different subsets of the data
housing.df[1:10, 1]  # show the first 10 rows of the first column only
housing.df[1:10, ]  # show the first 10 rows of each of the columns
housing.df[5, 1:10]  # show the fifth row of the first 10 columns
housing.df[5, c(1:2, 4, 8:10)]  # show the fifth row of some columns
housing.df[, 1]  # show the whole first column
housing.df$TOTAL.VALUE  # a different way to show the whole first column
housing.df$TOTAL.VALUE[1:10]  # show the first 10 rows of the first column
length(housing.df$TOTAL.VALUE)  # find the length of the first column
mean(housing.df$TOTAL.VALUE)  # find the mean of the first column
summary(housing.df)  # find summary statistics for each column

### Sampling from a Database

housing.df <- mlba::WestRoxbury

# random sample of 5 observations
s <- sample(row.names(housing.df), 5)
housing.df[s,]

# oversample houses with over 10 rooms
s <- sample(row.names(housing.df), 5, prob=ifelse(housing.df$ROOMS>10, 0.9, 0.01))
housing.df[s,]

# rebalance
housing.df$REMODEL <- factor(housing.df$REMODEL)
table(housing.df$REMODEL)
upsampled.df <- caret::upSample(housing.df, housing.df$REMODEL, list=TRUE)$x
table(upsampled.df$REMODEL)



### Preprocessing and Cleaning the Data
#### Types of Variables

library(tidyverse)

# get overview
str(housing.df)

# make REMODEL a factor variable
housing.df$REMODEL <- factor(housing.df$REMODEL)
str(housing.df$REMODEL)
levels(housing.df$REMODEL) # show factor's categories (levels)

# use tidyverse to load and preprocess data in one statement
# the %>% operator inserts the result of the expression on the left
# as the first argument into the function on the right
housing.df <- mlba::WestRoxbury %>%
  mutate(REMODEL=factor(REMODEL))

#### Handling Categorical Variables

library(fastDummies)
library(tidyverse)

housing.df <- dummy_cols(mlba::WestRoxbury,
                 remove_selected_columns=TRUE,  # remove the original column
                 remove_first_dummy=TRUE)  # removes the first created dummy variable
housing.df %>% head(2)

#### Missing Values

# To illustrate missing data procedures, we first convert a few entries for
# BEDROOMS to NA's. Then we impute these missing values using the median of the
# remaining values.
rows.to.missing <- sample(row.names(housing.df), 10)
housing.df[rows.to.missing,]$BEDROOMS <- NA
summary(housing.df$BEDROOMS)
# Now we have 10 NA's and the median of the remaining values is 3.

# replace the missing values using the median of the remaining values
# use median() with na.rm=TRUE to ignore missing values when computing the median.
housing.df <- housing.df %>%
  replace_na(list(BEDROOMS=median(housing.df$BEDROOMS, na.rm=TRUE)))

summary(housing.df$BEDROOMS)

## Predictive Power and Overfitting
### Creating and Using Data Partitions
#### Holdout Partition

housing.df <- mlba::WestRoxbury %>%
  mutate(REMODEL=factor(REMODEL))

# use set.seed() to get the same partitions when re-running the R code.
set.seed(1)

## partitioning into training (60%) and holdout (40%)
# randomly sample 60% of the row IDs for training; the remaining 40% serve
# as holdout
train.rows <- sample(rownames(housing.df), nrow(housing.df)*0.6)
# collect all the columns with training row ID into training set:
train.df <- housing.df[train.rows, ]
# assign row IDs that are not already in the training set, into holdout
holdout.rows <- setdiff(rownames(housing.df), train.rows)
holdout.df <- housing.df[holdout.rows, ]

## partitioning into training (50%), validation (30%), holdout (20%)
# randomly sample 50% of the row IDs for training
train.rows <- sample(rownames(housing.df), nrow(housing.df)*0.5)

# sample 30% of the row IDs into the validation set, drawing only from records
# not already in the training set
# use setdiff() to find records not already in the training set
valid.rows <- sample(setdiff(rownames(housing.df), train.rows),
              nrow(housing.df)*0.3)

# assign the remaining 20% row IDs serve as holdout
holdout.rows <- setdiff(rownames(housing.df), union(train.rows, valid.rows))

# create the 3 data frames by collecting all columns from the appropriate rows
train.df <- housing.df[train.rows, ]
valid.df <- housing.df[valid.rows, ]
holdout.df <- housing.df[holdout.rows, ]

## partitioning into training (60%) and holdout (40%) using caret
set.seed(1)
idx <- caret::createDataPartition(housing.df$TOTAL.VALUE, p=0.6, list=FALSE)
train.df <- housing.df[idx, ]
holdout.df <- housing.df[-idx, ]



## Building a Predictive Model
### Modeling Process
#### Cross-Validation

library(tidyverse)
library(mlba)
library(fastDummies)

housing.df <- mlba::WestRoxbury %>%
  # remove rows with missing values
  drop_na() %>%
  # remove column TAX
  select(-TAX) %>%
  # make REMODEL a factor and convert to dummy variables
  mutate(REMODEL=factor(REMODEL)) %>%
  dummy_cols(select_columns=c('REMODEL'),
             remove_selected_columns=TRUE, remove_first_dummy=TRUE)


set.seed(1)
idx <- caret::createDataPartition(housing.df$TOTAL.VALUE, p=0.6, list=FALSE)
train.df <- housing.df[idx, ]
holdout.df <- housing.df[-idx, ]


reg <- lm(TOTAL.VALUE ~ ., data=train.df)
train.res <- data.frame(actual=train.df$TOTAL.VALUE, predicted=reg$fitted.values,
                        residuals=reg$residuals)
head(train.res)


pred <- predict(reg, newdata=holdout.df)
holdout.res <- data.frame(actual=holdout.df$TOTAL.VALUE, predicted=pred,
                          residuals=holdout.df$TOTAL.VALUE - pred)
head(holdout.res)


library(caret)
# compute metrics on training set
data.frame(
    ME = round(mean(train.res$residuals), 5),
    RMSE = RMSE(pred=train.res$predicted, obs=train.res$actual),
    MAE = MAE(pred=train.res$predicted, obs=train.res$actual)
)

# compute metrics on holdout set
data.frame(
    ME = round(mean(holdout.res$residuals), 5),
    RMSE = RMSE(pred=holdout.res$predicted, obs=holdout.res$actual),
    MAE = MAE(pred=holdout.res$predicted, obs=holdout.res$actual)
)


# For demonstration purposes, we construct the new.data from the original dataset
housing.df <- mlba::WestRoxbury
new.data <- housing.df[100:102, -1] %>%
  mutate(REMODEL=factor(REMODEL, levels=c("None", "Old", "Recent"))) %>%
  dummy_cols(select_columns=c('REMODEL'),
           remove_selected_columns=TRUE, remove_first_dummy=TRUE)
new.data
pred <- predict(reg, newdata = new.data)
pred
