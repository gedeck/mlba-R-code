
if (!require(mlba)) {
  library(devtools)
  install_github("gedeck/mlba/mlba", force=TRUE)
}
options(scipen=999) # avoid scientific notation

# Evaluating Predictive Performance
## Evaluating Predictive Performance
### Prediction Accuracy Measures

library(tidyverse)
set.seed(1)

# load and preprocess file
car.df <- mlba::ToyotaCorolla %>%
    select(-one_of("Id", "Model", "Fuel_Type", "Color")) %>%
    drop_na()


# randomly generate training and holdout sets
idx <- caret::createDataPartition(car.df$Price, p=0.6, list=FALSE)
train.df <- car.df[idx, ]
holdout.df <- car.df[-idx, ]

# run linear regression model
reg <- lm(Price~., data=train.df)
pred_t <- predict(reg)
pred_h <- predict(reg, newdata=holdout.df)

## evaluate performance
# training
caret::RMSE(pred_t, train.df$Price)
# holdout
caret::RMSE(pred_h, holdout.df$Price)
# use utility function from the mlba package to calculate various metrics at once
rbind(
  Training=mlba::regressionSummary(pred_t, train.df$Price),
  Holdout=mlba::regressionSummary(pred_h, holdout.df$Price)
)


library(ggplot2)
library(gridExtra)

train.residuals <- train.df$Price - pred_t
holdout.residuals <- holdout.df$Price - pred_h
res.min <- min(train.residuals, holdout.residuals)
res.max <- max(train.residuals, holdout.residuals)

g1 <- ggplot() +
    geom_histogram(aes(x=train.residuals), fill="lightgray", color="grey") +
    labs(x="", y="Training") +
    xlim(res.min, res.max) +
    theme_bw()

g2 <- ggplot() +
    geom_histogram(aes(x=holdout.residuals), fill="lightgray", color="grey") +
    labs(x="", y="Holdout") +
    xlim(res.min, res.max) +
    theme_bw()

df <- data.frame(
  residual=c(train.residuals, holdout.residuals),
  role=c(rep("Training", length(train.residuals)), rep("Holdout", length(holdout.residuals)))
)
g3 <- ggplot(df, aes(x=role, y=residual)) +
    geom_boxplot() +
    labs(x="", y="") +
    theme_bw()

grid.arrange(g1, g2, g3, ncol=3)

g <- arrangeGrob(g1, g2, g3, ncol=3)

ggsave(file=file.path("..", "figures", "chapter_05", "residuals-full-model.pdf"),
       g, width=8, height=2.25, units="in")

### Cumulative Gains and Lift Charts

library(ggplot2)
library(gridExtra)

# load package gains, compute gains (we will use package caret for categorical y later)
library(gains)
price <- holdout.df$Price
gain <- gains(price, pred_h)

# cumulative lift chart
# we will compute the gain relative to price
df <- data.frame(
  ncases=c(0, gain$cume.obs),
  cumPrice=c(0, gain$cume.pct.of.total * sum(price))
)
g1 <- ggplot(df, aes(x=ncases, y=cumPrice)) +
  geom_line() +
  geom_line(data=data.frame(ncases=c(0, nrow(holdout.df)), cumPrice=c(0, sum(price))),
            color="gray", linetype=2) + # adds baseline
  labs(x="# cases", y="Cumulative Price", title="Cumulative gains chart")

# Decile-wise lift chart
df <- data.frame(
  percentile=gain$depth,
  meanResponse=gain$mean.resp / mean(price)
)
g2 <- ggplot(df, aes(x=percentile, y=meanResponse)) +
  geom_bar(stat="identity") +
  labs(x="Percentile", y="Decile Mean / Global Mean", title="Decile-wise lift chart")

grid.arrange(g1, g2, ncol=2)


g <- arrangeGrob(
  g1 + theme_bw(),
  g2 + theme_bw() + scale_x_continuous(breaks = seq(10, 100, by = 10)),
  ncol=2)
ggsave(file=file.path("..", "figures", "chapter_05", "MLR-toyotalift.pdf"),
       g, width=8, height=4, units="in")

## Judging Classifier Performance
### The Confusion (Classification) Matrix

  mowers.df <- mlba::RidingMowers
  g1 <- ggplot(mowers.df, mapping=aes(x=Income, y=Lot_Size, color=Ownership, fill=Ownership)) +
    geom_point(size=4) +
    scale_shape_manual(values = c(15, 21)) +
    scale_color_manual(values = c("darkorange", "steelblue")) +
    scale_fill_manual(values = c("darkorange", "lightblue"))

  makePlot  <- function(df, title, alpha) {
    no_personal_loan <- subset(df, Personal.Loan == 0)
    personal_loan <- subset(df, Personal.Loan == 1)
    g <- ggplot(universal.df, aes(x=Income, y=CCAvg)) +
        geom_point(data=no_personal_loan, alpha=alpha,
                  color="lightblue") +
        geom_point(data=personal_loan, color="steelblue") +
        labs(title=title, colour="Personal\nLoan", x="Annual income ($000s)",
             y="Monthly average credict card spending ($000s)") +
        scale_color_brewer(palette="Set1",
               guide=guide_legend(override.aes=list(size=3, alpha=1))) +
        scale_x_log10() +
        scale_y_log10() +
        theme_bw()
    return (g)
  }

  universal.df <- mlba::UniversalBank
  g2 <- makePlot(universal.df, "", 0.5)

  grid.arrange(g1, g2, nrow=2)

  g <- arrangeGrob(g1 + theme_bw(), g2 + theme_bw(), nrow=2)
  ggsave(file=file.path("..", "figures", "chapter_05", "ClassSeparation.pdf"),
         g, width=5, height=8, units="in")

### Propensities and Threshold for Classification

# note: function confusionMatrix requires library caret
library(caret)
owner.df <- mlba::OwnerExample
## threshold = 0.5
confusionMatrix(factor(ifelse(owner.df$Probability>0.5, "owner", "nonowner")),
                owner.df$Class)$table
# note: "reference" = "actual"
## threshold = 0.25
confusionMatrix(factor(ifelse(owner.df$Probability>0.25, "owner", "nonowner")),
                owner.df$Class)$table
## threshold = 0.75
confusionMatrix(factor(ifelse(owner.df$Probability>0.75, "owner", "nonowner")),
                owner.df$Class)$table


# replace data.frame with your own
df <- mlba::LiftExample

# compute accuracy per threshold
result = data.frame()
for (cut in seq(0,1,0.1)){
  cm <- confusionMatrix(factor(1 * (df$prob > cut), levels=c(0,1)),
                        factor(df$actual, levels=c(0,1)))
  result <- bind_rows(result,
                      c(Threshold=cut, cm$overall["Accuracy"]))
}
result$Error <- 1 - result$Accuracy

# plot accuracy and error rate
ggplot(result, aes(x=Threshold)) +
  geom_line(aes(y=Accuracy, linetype="Accuracy")) +
  geom_line(aes(y=Error, linetype="Overall error"), color="gray") +
  labs(x="Threshold Value", y="", linetype="Legend")


  g <- last_plot() + theme_bw() +
        scale_linetype(guide=guide_legend(override.aes=list(color="black")))
  ggsave(file=file.path("..", "figures", "chapter_05", "cutoffPlot.pdf"),
         g, width=6, height=4, units="in")

### Performance in Case of Unequal Importance of Classes
#### ROC Curve

library(ROCR)
predob <- prediction(df$prob, df$actual)
perf <- performance(predob, "tpr", "fpr")
perf.df <- data.frame(
  tpr=perf@x.values[[1]],
  fpr=perf@y.values[[1]]
)
ggplot(perf.df, aes(x=tpr, y=fpr)) +
  geom_line() +
  geom_segment(aes(x=0, y=0, xend=1, yend=1), color="grey", linetype="dashed") +
  labs(x="1 - Specificity", y="Sensitivity")

# get the AUC value
performance(predob, measure="auc")@y.values[[1]]


ggsave(file=file.path("..", "figures", "chapter_05", "rocRidingMower.pdf"),
       last_plot() + theme_bw(), width=4, height=4, units="in")

#### Precision and Recall

library(ROCR)

predob <- prediction(df$prob, df$actual)
perf <- performance(predob, "tpr", "fpr")
perf.df <- data.frame(
  threshold=performance(predob, "prec")@x.values[[1]],
  precision=performance(predob, "prec")@y.values[[1]],
  recall=performance(predob, "rec")@y.values[[1]]
)
perf.df$f1 = 2*perf.df$precision*perf.df$recall / (perf.df$recall + perf.df$precision)
g <- ggplot(perf.df, aes(x=threshold)) +
  geom_line(aes(y=precision, color="precision", linetype="precision"), size=1) +
  geom_line(aes(y=recall, color="recall", linetype="recall"), size=1) +
  geom_line(aes(y=f1, color="f1", linetype="f1"), size=1) +
  labs(x="Threshold", y="Metric", linetype="Metrics", color="Metrics")
g

ggsave(file=file.path("..", "figures", "chapter_05", "precision-recall.pdf"),
       g + theme_bw() + theme(legend.key.size = unit(1, "cm")),
       width=6, height=4, units="in")

## Judging Ranking Performance
### Cumulative Gains and Lift Charts for Binary Data
#### Cumulative Gains Chart

df <- mlba::LiftExample %>%
  mutate(actual=relevel(factor(actual), ref="1"))

# first option with 'ROCR' library:
pred <- prediction(df$prob, df$actual)
perf <- performance(pred, "tpr", "rpp")
plot(perf, xlab="Ratio of Cases", ylab="Ratio of Samples Found")
lines(c(0, 1), c(0, 1), lty='dashed')

# second option with 'caret' library:
# load data and make actual a factor variable with 1 as the reference class
lift.example <- caret::lift(actual ~ prob, data=df)
ggplot(lift.example, plot = "gain")

# third option with 'gains' library:
library(gains)
df <- mlba::LiftExample
gain <- gains(df$actual, df$prob, groups=nrow(df))
result <- data.frame(
  ncases=c(0, gain$cume.obs),
  cumulative=sum(df$actual)*c(0, gain$cume.pct.of.total)
)
ggplot(result, aes(x=ncases, y=cumulative)) +
  geom_line() +
  geom_segment(aes(x=0, y=0, xend=nrow(df), yend=sum(df$actual)),
               color="gray", linetype=2) + # adds baseline
  labs(x="# Cases", y="# Samples Found")


g3 <- last_plot()
# use ggplot with ROCR result
perf.df <- data.frame(
  tpr=perf@x.values[[1]],
  fpr=perf@y.values[[1]]
)
g1 <- ggplot(perf.df, aes(x=tpr, y=fpr)) +
  geom_line() +
  geom_segment(aes(x=0, y=0, xend=1, yend=1), color="grey", linetype="dashed") +
  labs(x="Ratio of Cases", y="Ratio of Samples Found")

g2 <- ggplot(lift.example, plot = "gain")

g <- arrangeGrob(g1 + theme_bw(), g2 + theme_bw(), g3 + theme_bw(), ncol=3)
ggsave(file=file.path("..", "figures", "chapter_05", "gainsChartClassification.pdf"),
       g, width=12, height=4, units="in")

### Decile-wise Lift Charts
#### Interpreting the Cumulative Gains Chart

df <- mlba::LiftExample

# use gains() to compute deciles.
gain <- gains(df$actual, df$prob)
barplot(gain$mean.resp / mean(df$actual), names.arg=seq(10, 100, by=10),
        xlab="Percentile", ylab="Decile Mean / Global Mean")


pdf(file=file.path("..", "figures", "chapter_05", "decileLiftClassification.pdf"),
    width=6, height=4)
  barplot(gain$mean.resp / mean(df$actual), names.arg=seq(10, 100, by=10),
          xlab="Percentile", ylab="Decile Mean / Global Mean")
dev.off()

# Here is code to achieve the same using ROCR with the performance data used for
# the cumulative gains chart
pred <- prediction(df$prob, df$actual)
perf <- performance(pred, "tpr", "rpp")

x <- perf@x.values[[1]]
y <- perf@y.values[[1]]


last <- c(0, 0)
decileLift <- tibble()
for (decile in seq(0.1, 1, by=0.1)) {
  idx <- max(which(x < decile))
  current <- c(x[idx], y[idx])
  deltas <- current - last
  currentLift <- deltas[2] / deltas[1]
  decileLift <- rbind(decileLift, c(current[1], currentLift))
  last <- current
}
colnames(decileLift) <- c("Percentile", "Decile Mean / Global Mean")
print(decileLift)
barplot(decileLift[,"Decile Mean / Global Mean"], names.arg=round(100*decileLift$Percentile, 0),
        xlab="Percentile", ylab="Decile Mean / Global Mean")

### Cumulative Gains as a Function of Threshold

df <- mlba::LiftExample %>%  mutate(actual=relevel(factor(actual), ref="1"))


plot(caret::lift(actual ~ prob, data=df), plot="lift")

## Oversampling
### Evaluating Model Performance if Only Oversampled Holdout Set Exists
#### II. Adjusting the Cumulative Gains Chart for Oversampling

  lift <- c(7, 2.5, 0.5, 0.25, 0.25, 0.1, 0.1, 0.1, 0.1, 0.1)
  pdf(file=file.path("..", "figures", "chapter_05", "prob5-4.pdf"),
    width=6, height=4)
    barplot(lift, names.arg=seq(10, 100, by=10),
            xlab="Percentile", ylab="Decile Mean / Global Mean")
  dev.off()


df <- read.csv('p5.csv')

set.seed(1)
idx <- caret::createDataPartition(df$sales, p=0.6, list=FALSE)
train.df <- df[idx,]
holdout.df <- df[-idx,]
model <- train(sales ~ ., data=train.df, method="lm", trControl=trainControl(method="none"))
result <- data.frame(
  predicted=predict(model, holdout.df),
  actual=holdout.df$sales
)

price <- holdout.df$sales

# cumulative lift chart
# we will compute the gain relative to price
gain <- gains(holdout.df$sales, predict(model, holdout.df), groups=100)
df <- data.frame(
  ncases=c(0, gain$cume.obs),
  cumPrice=c(0, gain$cume.pct.of.total * sum(price))
)
options(scipen=999) # avoid scientific notation
g1 <- ggplot(df, aes(x=ncases, y=cumPrice)) +
  geom_line() +
  geom_line(data=data.frame(ncases=c(0, nrow(holdout.df)), cumPrice=c(0, sum(price))),
            color="gray", linetype=2) + # adds baseline
  labs(x="# cases", y="Cumulative Price", title="Cumulative gains chart")
# Decile-wise lift chart
gain <- gains(holdout.df$sales, predict(model, holdout.df))
df <- data.frame(
  percentile=gain$depth,
  meanResponse=gain$mean.resp / mean(holdout.df$sales)
)
g2 <- ggplot(df, aes(x=percentile, y=meanResponse)) +
  geom_bar(stat="identity") +
  labs(x="Percentile", y="Decile Mean / Global Mean", title="Decile-wise lift chart")
grid.arrange(g1, g2, ncol=2)

g <- arrangeGrob(g1 + theme_bw(), g2 + theme_bw(), ncol=2)
ggsave(file=file.path("..", "figures", "chapter_05", "exercise-gains-lift-software.pdf"),
       g, width=8, height=4, units="in")
