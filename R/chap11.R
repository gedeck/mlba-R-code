
if (!require(mlba)) {
  library(devtools)
  install_github("gedeck/mlba/mlba", force=TRUE)
}

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

library(tidyverse)
library(fastDummies)

# convert SUR_COND and ALCHL_I to dummy variables (remove firest dummy)
# convert outcome MAX_SEV_IR to dummy variables keeping all
accidents.df <- mlba::AccidentsNN %>%
  dummy_cols(select_columns=c("ALCHL_I", "SUR_COND"),
             remove_selected_columns=TRUE, remove_first_dummy=TRUE) %>%
  dummy_cols(select_columns=c("MAX_SEV_IR"),
             remove_selected_columns=TRUE)

# partition the data
set.seed(1)
idx <- createDataPartition(mlba::AccidentsNN$MAX_SEV_IR, p=0.6, list=FALSE)
train.df <- accidents.df[idx, ]
holdout.df <- accidents.df[-idx, ]
train.actual <- mlba::AccidentsNN[idx, ]$MAX_SEV_IR
holdout.actual <- mlba::AccidentsNN[-idx, ]$MAX_SEV_IR

nn <- neuralnet(MAX_SEV_IR_0 + MAX_SEV_IR_1 + MAX_SEV_IR_2 ~ .,
                data=train.df, hidden=2)

# predict the three outcome variables and assign class using maximum score
pred.train <- predict(nn, train.df)
class.train <- apply(pred.train, 1, which.max)-1
confusionMatrix(factor(class.train), factor(train.actual))

pred.holdout <- predict(nn, holdout.df)
class.holdout <- apply(pred.holdout, 1, which.max)-1
confusionMatrix(factor(class.holdout), factor(holdout.actual))


# 1 not able to predict 2 0.8496
# 2, 3 0.8697
# 4, 5 0.8647

## Deep Learning
### Example: Classification of Fashion Images

# code to prepare a similar picture using R
library(reticulate)
use_condaenv('mlba-r')
library(keras)

fashion_mnist <- keras::dataset_fashion_mnist()
clothes.labels <- c('Top', 'Trouser', 'Pullover', 'Dress', 'Coat',
                    'Sandal', 'Shirt', 'Sneaker', 'Bag', 'Boot')
x_train <- fashion_mnist$train$x
y_train <- fashion_mnist$train$y

rotate <- function(x) t(apply(x, 2, rev))
plot_image <- function(x, title = "", title.color = "black") {
  x <- rotate(x)
  image(x, axes=FALSE, col=grey(seq(0, 1, length = 256)),
        main=list(title, col=title.color))
}
plot_sample <- function() {
  par(mfrow=c(4, 5), mar=c(0, 0.2, 1, 0.2))
  for (offset in c(0, 5)) {
    range <- (1+offset):(5+offset)
    for (i in range) {
      examples <- which(y_train %in% (i-1))
      example <- examples[1]
      plot_image(x_train[example, , ], clothes.labels[y_train[example] + 1])
    }
    for (i in range) {
      examples <- which(y_train %in% (i-1))
      example <- examples[2]
      plot_image(x_train[example, , ])
    }
  }
}
plot_sample()

pdf(file=file.path("..", "figures", "chapter_11", "fashion-mnist-sample.pdf"), width=5, height=4)
  plot_sample()
dev.off()

#### Data Preprocessing

# load required packages
# Keras and TensorFlow require a Python conda environment with these packages installed
library(reticulate)
use_condaenv('mlba-r')
library(keras)
library(tensorflow)

# load the data and split into training and validation sets
fashion_mnist <- keras::dataset_fashion_mnist()
x_train <- fashion_mnist$train$x
y_train <- fashion_mnist$train$y
x_valid <- fashion_mnist$test$x
y_valid <- fashion_mnist$test$y

# pixel values need to be scaled to range [0, 1]
x_train <- x_train / 255
x_valid <- x_valid / 255

# input require an additional dimension to describe pixel values
# dimensions are (samples, row, column, pixel)
x_train <- array_reshape(x_train, c(dim(x_train), 1))
x_valid <- array_reshape(x_valid, c(dim(x_valid), 1))

# output values need to be converted into a matrix with one-hot-encoding of classes
# dimensions are (samples, classes)
y_train <- to_categorical(y_train, 10)
y_valid <- to_categorical(y_valid, 10)
dim(x_train)
dim(y_train)


# Model definition (architecture taken from
# https://keras.rstudio.com/articles/examples/mnist_cnn.html )
input_shape = dim(x_train)[2:4]
num_classes <- 10

model <- keras_model_sequential()
model %>%
  layer_conv_2d(filters=32, kernel_size=c(5,5), activation='relu',
                input_shape=input_shape) %>%
  layer_conv_2d(filters=64, kernel_size=c(3,3), activation='relu') %>%
  layer_max_pooling_2d(pool_size=c(2,2)) %>%
  layer_dropout(rate=0.25) %>%
  layer_flatten() %>%
  layer_dense(units = 128, activation = 'relu') %>%
  layer_dropout(rate = 0.5) %>%
  layer_dense(units = num_classes, activation = 'softmax')

model

# compile model
model %>% compile(
  loss = loss_categorical_crossentropy,
  optimizer = optimizer_adadelta(),
  metrics = c('accuracy')
)

# train and evaluate
model %>% fit(
  x_train, y_train,
  batch_size = 128,
  epochs = 20,
  verbose = 1,
  validation_data = list(x_valid, y_valid)
)

#### Training a Deep Learning Network

library(gridExtra)
df <- read.csv("cv-training.csv")
g1 <- ggplot(df, aes(x=epoch)) +
  geom_line(aes(y=loss), color="steelblue") +
  geom_line(aes(y=val_loss), color="tomato") +
  geom_hline(yintercept=min(df$val_loss), color="black", linetype="dotted") +
  geom_vline(xintercept=which.min(df$val_loss), color="black", linetype="dotted") +
  labs(x="Epoch", y="Loss") +
  theme_bw()
g2 <- ggplot(df, aes(x=epoch)) +
  geom_line(aes(y=accuracy), color="steelblue") +
  geom_line(aes(y=val_accuracy), color="tomato") +
  geom_hline(yintercept=max(df$val_accuracy), color="black", linetype="dotted") +
  labs(x="Epoch", y="Accuracy") +
  theme_bw()
grid.arrange(g1, g2, ncol=2)


g <- arrangeGrob(g1, g2, ncol=2)
ggsave(file=file.path("..", "figures", "chapter_11", "fashion-mnist-learning.pdf"),
       g, width=5, height=2.5, units="in")

#### Applying the Predictive Model

  model <- load_model_tf("cnn-model.tf")


propensities <- predict(model, x_valid)
propensities[1:5, ]

# convert to class using winner takes all
predClass <- apply(propensities, 1, which.max)
predClass[1:5]

# confusion matrix
caret::confusionMatrix(factor(predClass), factor(fashion_mnist$test$y + 1))


