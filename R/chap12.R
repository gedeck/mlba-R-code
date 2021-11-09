
# TODO: remove once package is available on CRAN
library(devtools)
install_github("gedeck/mlba/mlba", force=TRUE)
library(mlba)

# Discriminant Analysis
## Introduction
### Example 1: Riding Mowers

library(ggplot2)
mowers.df <- mlba::RidingMowers
g <- ggplot(mowers.df, mapping=aes(x=Income, y=Lot_Size, color=Ownership, fill=Ownership)) +
  geom_point(size=4) +
  geom_abline(intercept=40, slope=-0.34) +
  scale_shape_manual(values = c(15, 21)) +
  scale_color_manual(values = c('darkorange', 'steelblue')) +
  scale_fill_manual(values = c('darkorange', 'lightblue'))

g

ggsave(file=file.path("..", "figures", "chapter_12", "riding-mower.pdf"),
       g + theme_bw(), width=6, height=4, units="in")

### Example 2: Personal Loan Acceptance

library(gridExtra)
makePlot  <- function(df, title, alpha) {
  no_personal_loan <- subset(df, Personal.Loan == 0)
  personal_loan <- subset(df, Personal.Loan == 1)

  g <- ggplot(universal.df, aes(x=Income, y=CCAvg)) +
      geom_point(data=no_personal_loan, alpha=alpha,
                color="lightblue") +
      geom_point(data=personal_loan, color="steelblue") +
      labs(title=title, colour="Personal\nLoan", x='Annual income ($000s)',
           y='Monthly average credict card spending ($000s)') +
      scale_color_brewer(palette="Set1",
             guide=guide_legend(override.aes=list(size=3, alpha=1))) +
      scale_x_log10() +
      scale_y_log10() +
      theme_bw()

  return (g)
}

set.seed(1)
universal.df <- mlba::UniversalBank
idx <- sample(dim(universal.df)[1], 200)
g1 <- makePlot(universal.df[idx, ], 'Sample of 200 customers', 1.0)
g2 <- makePlot(universal.df, 'All 5000 customers', 0.5)
grid.arrange(g1, g2, ncol=2)

g <- arrangeGrob(g1, g2, ncol=2)
ggsave(file=file.path("..", "figures", "chapter_12", "personalLoan_sampled.pdf"),
      g, width=8, height=4, units="in")

## Fisher's Linear Classification Functions

library(caret)

mowers.df <- mlba::RidingMowers
trControl <- caret::trainControl(method='none')
da.reg <- train(Ownership ~ Income + Lot_Size, data=mowers.df,
                method='lda', trControl=trControl)

means <- colSums(da.reg$finalModel$means) / 2

library(DiscriMiner)
mowers.df <- mlba::RidingMowers
da.reg <- linDA(mowers.df[,1:2], mowers.df[,3])
da.reg$functions


da.reg <- linDA(mowers.df[,1:2], mowers.df[,3])
# compute propensities manually (below); or, use lda() in package MASS with predict()
propensity.owner <- exp(da.reg$scores[,2])/(exp(da.reg$scores[,1])+exp(da.reg$scores[,2]))
data.frame(Actual=mowers.df$Ownership,
           da.reg$classification, da.reg$scores, propensity.owner=propensity.owner)

## Classifying More Than Two Classes
### Example 3: Medical Dispatch to Accident Scenes

library(DiscriMiner)
library(caret)

accidents.df <-  mlba::accidents
lda.reg <- linDA(accidents.df[,1:10], accidents.df[,11])
lda.reg$functions
confusionMatrix(as.factor(lda.reg$classification), as.factor(accidents.df$MAX_SEV))


propensity <- exp(lda.reg$scores[,1:3])/
    (exp(lda.reg$scores[,1])+exp(lda.reg$scores[,2])+exp(lda.reg$scores[,3]))

res <- data.frame(Classification = da.reg$classification,
           Actual = accidents.df$MAX_SEV,
           Score = round(lda.reg$scores,2),
           Propensity = round(propensity,2))
head(res)
