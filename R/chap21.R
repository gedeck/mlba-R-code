
if (!require(mlba)) {
  library(devtools)
  install_github("gedeck/mlba/mlba", force=TRUE)
}

# Text Mining
## The Tabular Representation of Text: Term-Document Matrix and ``Bag-of-Words"

library(tm)

# define vector of sentences ("docs")
text <- c("this is the first sentence",
          "this is a second sentence",
          "the third sentence is here")

# convert sentences into a corpus
corp <- Corpus(VectorSource(text))

# compute term frequency
tdm <- TermDocumentMatrix(corp)
inspect(tdm)

## Preprocessing the Text

text <- c("this is the first     sentence!!",
          "this is a second Sentence :)",
          "the third sentence, is here",
          "forth of all sentences")
corp <- Corpus(VectorSource(text))
tdm <- TermDocumentMatrix(corp)
inspect(tdm)

### Tokenization

# tokenization
corp <- tm_map(corp, stripWhitespace)
corp <- tm_map(corp, removePunctuation)
tdm <- TermDocumentMatrix(corp)
inspect(tdm)

### Text Reduction

stopwords('english')


# stopwords
library(SnowballC)
corp <- tm_map(corp, removeWords, stopwords("english"))

# stemming
corp <- tm_map(corp, stemDocument)

tdm <- TermDocumentMatrix(corp)
inspect(tdm)

### Term Frequency--Inverse Document Frequency (TF-IDF)

tfidf <- weightTfIdf(tdm)
inspect(tfidf)

## Example: Online Discussions on Autos and Electronics
### Importing and Labeling the Records

library(tm)
# step 1: import and label records
# read zip file into a corpus
corp <- Corpus(ZipSource(mlba::AutosElectronics, recursive = T))

# create an array of records labels
label <- c(rep(1, 1000), rep(0, 1000))

# step 2: text preprocessing
# tokenization
corp <- tm_map(corp, stripWhitespace)
corp <- tm_map(corp, removePunctuation)
corp <- tm_map(corp, removeNumbers)

# stopwords
corp <- tm_map(corp, removeWords, stopwords("english"))

# stemming
corp <- tm_map(corp, stemDocument)

# step 3: TF-IDF and latent semantic analysis
# compute TF-IDF
tdm <- TermDocumentMatrix(corp)
tfidf <- weightTfIdf(tdm)

# extract (20) concepts
library(lsa)
lsa.tfidf <- lsa(tfidf, dim = 20)

# convert to data frame
words.df <- as.data.frame(as.matrix(lsa.tfidf$dk))

### Fitting a Predictive Model

library(caret)

# prepare training and holdout sets
set.seed(1)
df <- cbind(label=factor(label), words.df)
idx <- caret::createDataPartition(df$label, p=0.6, list=FALSE)
train.df <- df[idx, ]
holdout.df <- df[-idx, ]

# fit logistic regression
logit.reg <- train(label ~ ., data=train.df,
                          trControl=trainControl(method="none"),
                          method="glm", family="binomial")

# compute accuracy on holdout set
pred <- predict(logit.reg, newdata=holdout.df)
confusionMatrix(pred, holdout.df$label)


library(gains)

prob <- predict(logit.reg, newdata=holdout.df, type="prob")[,2]
actual <- ifelse(holdout.df$label == 1, 1, 0)
gain <- gains(actual, prob)
barplot(gain$mean.resp/mean(actual), names.arg=seq(10, 100, by=10),
        xlab="Percentile", ylab="Decile mean / global mean")


pdf(file=file.path("..", "figures", "chapter_21", "decileLiftClassification.pdf"),
    width=6, height=4)
    barplot(gain$mean.resp/mean(actual), names.arg=seq(10, 100, by=10),
            xlab="Percentile", ylab="Decile mean / global mean")
dev.off()


## Example: Sentiment Analysis of Movie Reviews
### Data Loading, Preparation, and Partitioning

library(tidyverse)
library(text2vec)

# load and split data into training and holdout set
data <- mlba::IMDBdataset10K %>%
  mutate(
    id = row_number(),
    sentiment = as.factor(sentiment)
  )

set.seed(1)
trainIndex <- createDataPartition(data$sentiment, p=0.8, list=FALSE)
data_train <- data[trainIndex, ]
data_holdout <- data[-trainIndex, ]


prep_fun <- tolower
tok_fun <- word_tokenizer

it_train <- itoken(data_train$review, ids=data_train$id,
                   preprocessor=prep_fun, tokenizer=tok_fun)
it_holdout <- itoken(data_holdout$review, ids=data_holdout$id,
                     preprocessor=prep_fun, tokenizer=tok_fun)

vocab <- create_vocabulary(it_train)
vocab <- prune_vocabulary(vocab, term_count_min = 5L)
vectorizer <- vocab_vectorizer(vocab)
tcm_train <- create_tcm(it_train, vectorizer, skip_grams_window = 5L)



### Generating and Applying pb

# determine word vectors
glove <- GlobalVectors$new(rank=100, x_max=10)
wv_main <- glove$fit_transform(tcm_train, n_iter=10, convergence_tol=0.01, n_threads=8)
wv_context <- glove$components
word_vectors <- wv_main + t(wv_context)




dtm_train <- create_dtm(it_train, vectorizer)
common_terms <- intersect(colnames(dtm_train), rownames(word_vectors) )
dtm_averaged <-  normalize(dtm_train[, common_terms], "l1")
sentence_vectors_train <- dtm_averaged %*% word_vectors[common_terms, ]

dtm_holdout <- create_dtm(it_holdout, vectorizer)
common_terms <- intersect(colnames(dtm_holdout), rownames(word_vectors) )
dtm_averaged <-  normalize(dtm_holdout[, common_terms], "l1")
sentence_vectors_holdout <- dtm_averaged %*% word_vectors[common_terms, ]

### Fitting a Predictive Model

train.df <- as.data.frame(as.matrix(sentence_vectors_train))
train.df$sentiment <- data_train$sentiment

trControl <- caret::trainControl(method="cv", number=5, allowParallel=TRUE)
logit.reg <- caret::train(sentiment ~ ., data=train.df, trControl=trControl,
                      # fit logistic regression with a generalized linear model
                      method="glm", family="binomial")

holdout.df <- as.data.frame(as.matrix(sentence_vectors_holdout))
holdout.df$sentiment <- data_holdout$sentiment

caret::confusionMatrix(predict(logit.reg, holdout.df), holdout.df$sentiment)




library(ROCR)
prob <- predict(logit.reg, newdata=holdout.df, type="prob")$positive

predob <- prediction(prob, holdout.df$sentiment)
perf <- performance(predob, "tpr", "fpr")
perf.df <- data.frame(
  tpr=perf@x.values[[1]],
  fpr=perf@y.values[[1]]
)
ggplot(perf.df, aes(x=tpr, y=fpr)) +
  geom_line() +
  geom_segment(aes(x=0, y=0, xend=1, yend=1), color="grey", linetype="dashed") +
  labs(x="1 - Specificity", y="Sensitivity")


  ggsave(file=file.path("..", "figures", "chapter_21", "glove-ROC.pdf"),
   last_plot() + theme_bw())
