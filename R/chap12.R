
if (!require(mlba)) {
  library(devtools)
  install_github("gedeck/mlba/mlba", force=TRUE)
}

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

  g <- ggplot(universal.df, aes(x=Income, y=CCAvg, color=Personal.Loan)) +
      geom_point(aes(color="nonacceptor"), data=no_personal_loan, alpha=alpha) +
      geom_point(aes(color="acceptor"), data=personal_loan) +
      labs(title=title, colour="Personal Loan", x='Annual income ($000s)',
           y='Monthly average credict card spending ($000s)') +
      scale_color_manual(values=c("lightblue", "steelblue"),
             guide=guide_legend(override.aes=list(size=3, alpha=1))) +
      scale_x_log10() +
      scale_y_log10() +
      theme_bw()

  return (g)
}

set.seed(1)
universal.df <- mlba::UniversalBank
idx <- sample(dim(universal.df)[1], 200)
g1 <- makePlot(universal.df[idx, ], 'Sample of 200 customers', 1.0) +
        theme(legend.position = c(0.2, 0.85))
g2 <- makePlot(universal.df, 'All 5000 customers', 0.5) +
        guides(color="none")
grid.arrange(g1, g2, ncol=2)

g <- arrangeGrob(g1, g2, ncol=2)
ggsave(file=file.path("..", "figures", "chapter_12", "personalLoan_sampled.pdf"),
      g, width=8, height=4, units="in")

## Fisher's Linear Classification Functions

library(caret)
mowers.df <- mlba::RidingMowers
trControl <- caret::trainControl(method='none')
model <- train(Ownership ~ Income + Lot_Size, data=mowers.df,
               method='lda', trControl=trControl)
model$finalModel  # access the wrapped LDA model

# DiscriMiner exposes the Fisher's linear classification function
library(DiscriMiner)
mowers.df <- mlba::RidingMowers
da.mower <- linDA(mowers.df[,1:2], mowers.df[,3])
da.mower$functions


da.mower <- linDA(mowers.df[,1:2], mowers.df[,3])
# compute propensities manually (below); or, use lda() in package MASS or caret with predict()
propensity.owner <- exp(da.mower$scores[,2])/(exp(da.mower$scores[,1])+exp(da.mower$scores[,2]))
data.frame(Actual=mowers.df$Ownership, Predicted=da.mower$classification,
           da.mower$scores, propensity.owner=propensity.owner)


library(ggplot2)
da.mower <- train(Ownership ~ Income + Lot_Size, data=mowers.df,
                  method='lda', trControl=trControl)
means <- colSums(da.mower$finalModel$means) / 2
sIncome <- da.mower$finalModel$scaling['Income', 'LD1']
sLotSize <- da.mower$finalModel$scaling['Lot_Size', 'LD1']
m <- - sIncome / sLotSize
y0 <- means['Lot_Size'] - m * means['Income']

mowers.df <- mlba::RidingMowers
g <- ggplot(mowers.df, mapping=aes(x=Income, y=Lot_Size, color=Ownership, fill=Ownership)) +
  geom_point(size=4) +
  geom_point(data=data.frame(da.mower$finalModel$means), color='black', fill='black', shape=4, size=3) +
  geom_abline(aes(linetype='ad hoc line', intercept=40, slope=-0.34), color='darkgrey') +
  geom_abline(aes(linetype='LDA line', intercept=y0, slope=m)) +
  scale_shape_manual(values = c(15, 21)) +
  scale_color_manual(values = c('darkorange', 'steelblue')) +
  scale_fill_manual(values = c('darkorange', 'lightblue')) +
  scale_linetype_manual(values=c(2, 1), labels=c('ad hoc line', 'LDA line'))
g
ggsave(file=file.path("..", "figures", "chapter_12", "LDA-riding-mower.pdf"),
       g + theme_bw(), width=6, height=4, units="in")

## Prior Probabilities

trControl <- caret::trainControl(method='none')
model <- train(Ownership ~ Income + Lot_Size, data=mowers.df,
               method='lda', trControl=trControl)
model.prior <- train(Ownership ~ Income + Lot_Size, data=mowers.df,
               method='lda', prior=c(0.85, 0.15),
               trControl=trControl)

family.13 <- mowers.df[13,]
predict(model, family.13)
predict(model.prior, family.13)

## Classifying More Than Two Classes
### Example 3: Medical Dispatch to Accident Scenes

library(DiscriMiner)
library(caret)

accidents.df <-  mlba::Accidents
lda.model <- linDA(accidents.df[,1:10], accidents.df[,11])
lda.model$functions
confusionMatrix(as.factor(lda.model$classification), as.factor(accidents.df$MAX_SEV))


propensity <- exp(lda.model$scores[,1:3])/
    (exp(lda.model$scores[,1])+exp(lda.model$scores[,2])+exp(lda.model$scores[,3]))

res <- data.frame(Actual = accidents.df$MAX_SEV,
           Classification = lda.model$classification,
           Score = round(lda.model$scores,2),
           Propensity = round(propensity,2))
head(res)




library(tidyverse)
accidents.df <-  mlba::Accidents %>%
  mutate(MAX_SEV = factor(MAX_SEV))
da.model <- train(MAX_SEV ~ ., data=accidents.df, method='lda')
res <- data.frame(Actual=accidents.df$MAX_SEV,
           Classification=predict(da.model),
           Propensity=predict(da.model, type='prob') %>% round(2))
head(res)


