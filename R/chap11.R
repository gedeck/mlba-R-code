
# TODO: remove once package is available on CRAN
library(devtools)
install_github("gedeck/mlba/mlba", force=TRUE)

# Neural Nets
## Fitting a Network to Data
### Training the Model
#### Back Propagation of Error

library(neuralnet)
df <- mlba::TinyData
df$Like <- df$Acceptance=="like"
df$Dislike <- df$Acceptance=="dislike"
set.seed(1)


nn <- neuralnet(Like + Dislike ~ Salt + Fat, data = df, linear.output = F, hidden = 3)

# display weights
nn$weights

# display predictions
prediction(nn)

# plot network
plot(nn, rep="best")


library(caret)
predict <- compute(nn, data.frame(df$Salt, df$Fat))
predicted.class=apply(predict$net.result,1,which.max)-1
confusionMatrix(factor(ifelse(predicted.class=="1", "dislike", "like")),
                factor(df$Acceptance))

### Example 2: Classifying Accident Severity

library(neuralnet)
library(nnet)
library(caret)

accidents.df <- mlba::accidentsnn
# selected variables
vars <- c("ALCHL_I", "PROFIL_I_R", "VEH_INVL")
preprocessed <- cbind(accidents.df[,c(vars)],
                      class.ind(accidents.df$SUR_COND),
                      class.ind(accidents.df$MAX_SEV_IR))
names(preprocessed)=c(vars,
                   paste("SUR_COND_", c(1, 2, 3, 4, 9), sep=""),
                   paste("MAX_SEV_IR_", c(0, 1, 2), sep=""))

# partition the data
set.seed(1)
training=sample(row.names(accidents.df), dim(accidents.df)[1]*0.6)
validation=setdiff(row.names(accidents.df), training)

trainData <- preprocessed[training,]
validData <- preprocessed[validation,]

# run nn with 2 hidden nodes
# use hidden= with a vector of integers specifying number of hidden nodes in each layer
nn <- neuralnet(MAX_SEV_IR_0 + MAX_SEV_IR_1 + MAX_SEV_IR_2 ~
                  ALCHL_I + PROFIL_I_R + VEH_INVL + SUR_COND_1 + SUR_COND_2
                  + SUR_COND_3 + SUR_COND_4, data = trainData, hidden = 2)

training.prediction=compute(nn, trainData[,-c(8:11)])
training.class=apply(training.prediction$net.result,1,which.max)-1
confusionMatrix(as.factor(training.class), as.factor(accidents.df[training,]$MAX_SEV_IR))

validation.prediction=compute(nn, validData[,-c(8:11)])
validation.class=apply(validation.prediction$net.result,1,which.max)-1
confusionMatrix(as.factor(validation.class), as.factor(accidents.df[validation,]$MAX_SEV_IR))
