
if (!require(mlba)) {
  library(devtools)
  install_github("gedeck/mlba/mlba", force=TRUE)
}
options(scipen=999)

# Classification and Regression Trees
## Classification Trees
### Example 1: Riding Mowers

library(rpart)
library(rpart.plot)
mowers.df <- mlba::RidingMowers
class.tree <- rpart(Ownership ~ ., data = mowers.df,
                    control = rpart.control(minsplit = 0),
                    method = "class")
rpart.rules(class.tree)

plot_common_styling <- function(g, filename) {
  g <- g +
    geom_point(size=2) +
    scale_color_manual(values=c("darkorange", "steelblue")) +
    scale_fill_manual(values=c("darkorange", "lightblue")) +
    labs(x="Income ($000s)", y="Lot size (000s sqft)") +
    theme_bw() +
    theme(legend.position=c(0.89, 0.91),
          legend.title=element_blank(),
          legend.key=element_blank(),
          legend.background=element_blank())
  ggsave(file=file.path("..", "figures", "chapter_09", filename),
         g, width=5, height=3, units="in")
  return(g)
}
g <- ggplot(mowers.df, mapping=aes(x=Income, y=Lot_Size, color=Ownership, fill=Ownership))
plot_common_styling(g, "mowers_tree_0.pdf")
g <- g + geom_vline(xintercept=59.7)
plot_common_styling(g, "mowers_tree_1.pdf")
g <- g + geom_segment(x=59.9, y=21, xend=25, yend=21, color="black")
plot_common_styling(g, "mowers_tree_2.pdf")
g <- g + geom_segment(x=59.9, y=19.8, xend=120, yend=19.8, color="black")
plot_common_styling(g, "mowers_tree_3.pdf")
g <- g + geom_segment(x=84.75, y=19.8, xend=84.75, yend=10, color="black")
plot_common_styling(g, "mowers_tree_4.pdf")
g <- g + geom_segment(x=61.5, y=19.8, xend=61.5, yend=10, color="black")
plot_common_styling(g, "mowers_tree_5.pdf")

### Measures of Impurity
#### Normalization

ggplot() +
  scale_x_continuous(limits=c(0,1)) +
  geom_hline(yintercept=0.5, linetype=2, color="grey") +
  geom_hline(yintercept=1, linetype=2, color="grey") +
  geom_function(aes(color="Entropy measure"),
                fun = function(x) {- x*log2(x) - (1-x)*log2(1-x)},
                xlim=c(0.0001, 0.9999), n=100) +
  geom_function(aes(color="Gini index"),
                fun = function(x) {1 - x^2 - (1-x)^2}) +
  labs(y="Impurity measure", x=expression(~italic(p)[1]), color="Impurity measure") +
  scale_color_manual(values=c("Entropy measure"="darkorange", "Gini Index"="steelblue"))

ggsave(file=file.path("..", "figures", "chapter_09", "gini_entropy.pdf"),
       last_plot() + theme_bw(), width=5, height=2.5, units="in")



library(rpart)
library(rpart.plot)
mowers.df <- mlba::RidingMowers

# use rpart() to run a classification tree.
# define rpart.control() in rpart() to determine the depth of the tree.
class.tree <- rpart(Ownership ~ ., data = mowers.df,
    control=rpart.control(maxdepth=2), method="class")
## plot tree
# use rpart.plot() to plot the tree. You can control plotting parameters such
# as color, shape, and information displayed (which and where).
rpart.plot(class.tree, extra=1, fallen.leaves=FALSE)


  pdf(file.path("..", "figures", "chapter_09", "CT-mowerTree1.pdf"), width=3, height=3)
    rpart.plot(class.tree, extra=1, fallen.leaves=FALSE)
  dev.off()

  class.tree <- rpart(Ownership ~ ., data = mowers.df,
      control=rpart.control(minsplit=1), method="class")
  rpart.plot(class.tree, extra=1, fallen.leaves=FALSE)
  pdf(file.path("..", "figures", "chapter_09", "CT-mowerTree3.pdf"), width=5, height=5)
    rpart.plot(class.tree, extra=1, fallen.leaves=FALSE)
  dev.off()


class.tree

## Evaluating the Performance of a Classification Tree
### Example 2: Acceptance of Personal Loan

library(tidyverse)
library(caret)

# Load and preprocess data
bank.df <- mlba::UniversalBank %>%
  # Drop ID and zip code columns.
  select(-c(ID, ZIP.Code)) %>%
  # convert Personal.Loan to a factor with labels Yes and No
  mutate(Personal.Loan = factor(Personal.Loan, levels=c(0, 1), labels=c("No", "Yes")),
         Education = factor(Education, levels=c(1, 2, 3), labels=c("UG", "Grad", "Prof")))

# partition
set.seed(1)
idx <- createDataPartition(bank.df$Personal.Loan, p=0.6, list=FALSE)
train.df <- bank.df[idx, ]
holdout.df <- bank.df[-idx, ]

# classification tree
default.ct <- rpart(Personal.Loan ~ ., data=train.df, method="class")
# plot tree
rpart.plot(default.ct, extra=1, fallen.leaves=FALSE)


pdf(file.path("..", "figures", "chapter_09", "CT-universalTree1.pdf"), width=5, height=5)
  rpart.plot(default.ct, extra=1, fallen.leaves=FALSE)
dev.off()


deeper.ct <- rpart(Personal.Loan ~ ., data=train.df, method="class", cp=0, minsplit=1)
# count number of leaves
sum(deeper.ct$frame$var == "<leaf>")
# plot tree
rpart.plot(deeper.ct, extra=1, fallen.leaves=FALSE)


pdf(file.path("..", "figures", "chapter_09", "CT-universalTree2.pdf"), width=5, height=2.5)
  rpart.plot(deeper.ct, extra=1, fallen.leaves=FALSE)
dev.off()


# classify records in the holdout data.
# set argument type = "class" in predict() to generate predicted class membership.
default.ct.point.pred.train <- predict(default.ct,train.df,type = "class")
# generate confusion matrix for training data
confusionMatrix(default.ct.point.pred.train, train.df$Personal.Loan)
### repeat the code for the holdout set, and the deeper tree


default.ct.point.pred.holdout <- predict(default.ct,holdout.df,type = "class")
confusionMatrix(default.ct.point.pred.holdout, holdout.df$Personal.Loan)

deeper.ct.point.pred.train <- predict(deeper.ct,train.df,type = "class")
confusionMatrix(deeper.ct.point.pred.train, train.df$Personal.Loan)

deeper.ct.point.pred.holdout <- predict(deeper.ct,holdout.df,type = "class")
confusionMatrix(default.ct.point.pred.holdout, holdout.df$Personal.Loan)

## Avoiding Overfitting
### Stopping Tree Growth
#### Stopping Tree Growth: Grid Search for Parameter Tuning

set.seed(1)
trControl <- trainControl(method="cv", number=5, allowParallel=TRUE)
model1 <- train(Personal.Loan ~ ., data=train.df,
                method="rpart", trControl=trControl,
                tuneGrid=data.frame(cp=c(1, 0.1, 0.01, 0.001, 0.0001)))
model1$results
# focus grid search around cp=0.001
model2 <- train(Personal.Loan ~ ., data=train.df,
                method="rpart", trControl=trControl,
                tuneGrid=data.frame(cp=c(0.005, 0.002, 0.001, 0.0005, 0.0002)))
model2$results

### Pruning the Tree
#### Stopping Tree Growth: Conditional Inference Trees

# argument xval refers to the number of folds to use in rpart's built-in
# cross-validation procedure
# argument cp sets the smallest value for the complexity parameter.
cv.ct <- rpart(Personal.Loan ~ ., data=train.df, method="class",
    cp=0.00001, minsplit=5, xval=5)
# use printcp() to print the table.
printcp(cv.ct)


# prune by lower cp
pruned.ct <- prune(cv.ct,
    cp=cv.ct$cptable[which.min(cv.ct$cptable[,"xerror"]),"CP"])
sum(pruned.ct$frame$var == "<leaf>")
rpart.plot(pruned.ct, extra=1, fallen.leaves=FALSE)


pdf(file.path("..", "figures", "chapter_09", "CT-universalTree-pruned.pdf"), width=5, height=2.5)
  rpart.plot(pruned.ct, extra=1, fallen.leaves=FALSE)
dev.off()

### Best-Pruned Tree

  # prune by lower cp
  minErrorRow <- cv.ct$cptable[which.min(cv.ct$cptable[,"xerror"]), ]
  cutoff <- minErrorRow["xerror"] + minErrorRow["xstd"]
  best.cp <- cv.ct$cptable[cv.ct$cptable[,"xerror"] < cutoff,][1, "CP"]

  best.ct <- prune(cv.ct, cp=best.cp)
  sum(best.ct$frame$var == "<leaf>")
  rpart.plot(best.ct, extra=1, fallen.leaves=FALSE)
  pdf(file.path("..", "figures", "chapter_09", "CT-universalTree-best.pdf"), width=4, height=2.75)
    rpart.plot(best.ct, extra=1, fallen.leaves=FALSE)
  dev.off()

## Classification Rules from Trees

rpart.rules(best.ct)

## Regression Trees

# select variables for regression
outcome <- "Price"
predictors <- c("Age_08_04", "KM", "Fuel_Type", "HP", "Met_Color", "Automatic",
                "CC", "Doors", "Quarterly_Tax", "Weight")
# reduce data set to first 1000 rows and selected variables
car.df <- mlba::ToyotaCorolla[1:1000, c(outcome, predictors)]

# partition data
set.seed(1)  # set seed for reproducing the partition
idx <- createDataPartition(car.df$Price, p=0.6, list=FALSE)
car.train.df <- car.df[idx, ]
car.holdout.df <- car.df[-idx, ]

# use method "anova" for a regression model
cv.rt <- rpart(Price ~ ., data=car.train.df, method="anova",
    cp=0.00001, minsplit=5, xval=5)

# prune by lower cp
minErrorRow <- cv.rt$cptable[which.min(cv.ct$cptable[,"xerror"]), ]
cutoff <- minErrorRow["xerror"] + minErrorRow["xstd"]
best.cp <- cv.ct$cptable[cv.ct$cptable[,"xerror"] < cutoff,][1, "CP"]

best.rt <- prune(cv.rt, cp=best.cp)

# set digits to a negative number to avoid scientific notation
rpart.plot(best.rt, extra=1, fallen.leaves=FALSE, digits=-4)

pdf(file.path("..", "figures", "chapter_09", "RT-ToyotaTree.pdf"), width=7, height=4)
  rpart.plot(best.rt, extra=1, fallen.leaves=FALSE, digits=-4)
dev.off()

## Improving Prediction: Random Forests and Boosted Trees
### Random Forests

library(randomForest)
## random forest
rf <- randomForest(Personal.Loan ~ ., data=train.df, ntree=500,
    mtry=4, nodesize=5, importance=TRUE)

## variable importance plot
varImpPlot(rf, type=1)

## confusion matrix
rf.pred <- predict(rf, holdout.df)
confusionMatrix(rf.pred, holdout.df$Personal.Loan)


  pdf(file.path("..", "figures", "chapter_09", "VarImp.pdf"), width=7, height=4)
    varImpPlot(rf, type=1, main="")
  dev.off()

### Boosted Trees

library(caret)
library(xgboost)

xgb <- train(Personal.Loan ~ ., data=train.df, method="xgbTree", verbosity=0)

# compare ROC curves for classification tree, random forest, and boosted tree models
library(ROCR)
rocCurveData <- function(model, data) {
  prob <- predict(model, data, type="prob")[, "Yes"]
  predob <- prediction(prob, data$Personal.Loan)
  perf <- performance(predob, "tpr", "fpr")
  return (data.frame(tpr=perf@x.values[[1]], fpr=perf@y.values[[1]]))
}

performance.df <- rbind(
  cbind(rocCurveData(best.ct, holdout.df), model="Best-pruned tree"),
  cbind(rocCurveData(rf, holdout.df), model="Random forest"),
  cbind(rocCurveData(xgb, holdout.df), model="xgboost")
)
colors <- c("Best-pruned tree"="grey", "Random forest"="blue", "xgboost"="tomato")
ggplot(performance.df, aes(x=tpr, y=fpr, color=model)) +
  geom_line() +
  scale_color_manual(values=colors) +
  geom_segment(aes(x=0, y=0, xend=1, yend=1), color="grey", linetype="dashed") +
  labs(x="1 - Specificity", y="Sensitivity", color="Model")


library(gridExtra)
g <- last_plot() + theme_bw()
g1 <- g + guides(color="none")
g2 <- g + scale_x_continuous(limits=c(0, 0.2)) + scale_y_continuous(limits=c(0.8, 1.0))

g <- arrangeGrob(g1, g2, widths=c(3, 4.5), ncol=2)
ggsave(file=file.path("..", "figures", "chapter_09", "xgboost-ROC-1.pdf"),
       g, width=8, height=3, units="in")


xgb.focused <- train(Personal.Loan ~ ., data=train.df,
             method="xgbTree", verbosity=0,
             scale_pos_weight=10)

performance.df <- rbind(
cbind(rocCurveData(xgb, holdout.df), model="xgboost"),
cbind(rocCurveData(xgb.focused, holdout.df), model="xgboost (focused)")
)

colors <- c("xgboost"="tomato", "xgboost (focused)"="darkgreen")
ggplot(performance.df, aes(x=tpr, y=fpr, color=model)) +
  geom_line() +
  scale_color_manual(values=colors) +
  geom_segment(aes(x=0, y=0, xend=1, yend=1), color="grey", linetype="dashed") +
  labs(x="1 - Specificity", y="Sensitivity", color="Model")


library(gridExtra)
g <- last_plot() + theme_bw()
g1 <- g + guides(color="none")
g2 <- g + scale_x_continuous(limits=c(0, 0.2)) + scale_y_continuous(limits=c(0.8, 1.0))

g <- arrangeGrob(g1, g2, widths=c(3, 4.5), ncol=2)
ggsave(file=file.path("..", "figures", "chapter_09", "xgboost-ROC-2.pdf"),
       g, width=8, height=3, units="in")
