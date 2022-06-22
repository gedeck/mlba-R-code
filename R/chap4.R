
if (!require(mlba)) {
  library(devtools)
  install_github("gedeck/mlba/mlba", force=TRUE)
}

options(dplyr.summarise.inform = FALSE)
library(tidyverse)

# Dimension Reduction
## Data Summaries
### Example 1: House Prices in Boston

boston.housing.df <- mlba::BostonHousing
head(boston.housing.df, 9)
summary(boston.housing.df)

# compute mean, standard dev., min, max, median, length, and missing values of CRIM
mean(boston.housing.df$CRIM)
sd(boston.housing.df$CRIM)
min(boston.housing.df$CRIM)
max(boston.housing.df$CRIM)
median(boston.housing.df$CRIM)
length(boston.housing.df$CRIM)

# find the number of missing values of variable CRIM
sum(is.na(boston.housing.df$CRIM))

# compute mean, standard dev., min, max, median, length, and missing values for all
# variables
data.frame(mean=sapply(boston.housing.df, mean),
           sd=sapply(boston.housing.df, sd),
           min=sapply(boston.housing.df, min),
           max=sapply(boston.housing.df, max),
           median=sapply(boston.housing.df, median),
           length=sapply(boston.housing.df, length),
           miss.val=sapply(boston.housing.df,
                           function(x) sum(length(which(is.na(x))))))



### Summary Statistics

round(cor(boston.housing.df),2)

### Aggregation and Pivot Tables

boston.housing.df <- mlba::BostonHousing
table(boston.housing.df$CHAS)

# tidyverse version
boston.housing.df %>% count(CHAS)


# create bins of size 1
boston.housing.df <- boston.housing.df %>%
  mutate(RM.bin = cut(RM, c(1:9), labels=FALSE))

# compute the average of MEDV by (binned) RM and CHAS
# in aggregate() use the argument by= to define the list of aggregating variables,
# and FUN= as an aggregating function.
aggregate(boston.housing.df$MEDV, by=list(RM=boston.housing.df$RM.bin,
          CHAS=boston.housing.df$CHAS), FUN=mean)

# tidyverse version
boston.housing.df %>%
  group_by(RM.bin, CHAS) %>%
  summarise(mean(MEDV))


# use install.packages("reshape") the first time the package is used
library(reshape)
boston.housing.df <- mlba::BostonHousing
# create bins of size 1
boston.housing.df <- boston.housing.df %>%
  mutate(RM.bin = cut(RM, c(1:9), labels=FALSE))

# use melt() to stack a set of columns into a single column of data.
# stack MEDV values for each combination of (binned) RM and CHAS
mlt <- melt(boston.housing.df, id=c("RM.bin", "CHAS"), measure=c("MEDV"))
head(mlt, 5)

# use cast() to reshape data and generate pivot table
cast(mlt, RM.bin ~ CHAS, subset=variable=="MEDV",
     margins=c("grand_row", "grand_col"), mean)

# tidyverse version
boston.housing.df %>%
  group_by(RM.bin, CHAS) %>%
  summarize(mean=mean(MEDV)) %>%
  spread(CHAS, mean)

## Reducing the Number of Categories in Categorical Variables

boston.housing.df <- mlba::BostonHousing

tbl <- table(boston.housing.df$CAT.MEDV, boston.housing.df$ZN)
prop.tbl <- prop.table(tbl, margin=2)
barplot(prop.tbl, xlab="ZN", ylab="", yaxt="n",main="Distribution of CAT.MEDV by ZN")
axis(2, at=(seq(0,1, 0.2)), paste(seq(0,100,20), "%"))

library(tidyverse)
df <- data.frame(prop.tbl)
ggplot(df, aes(x=Var2, y=Freq, group=Var1, fill=Var1)) +
  geom_bar(stat="identity", color="grey", width=1) +
  scale_y_continuous(labels = scales::percent, expand=expansion()) +
  scale_fill_manual("CAT.MEDV", values=c("0"="#eeeeee", "1"="darkgrey")) +
  labs(x="ZN", y="", title="Distribution of CAT.MEDV by ZN")


  g <- last_plot() + theme_bw()
  ggsave(file=file.path("..", "figures", "chapter_04", "reduction-pivot-bar.pdf"),
         g, width=9, height=4, units="in")


library(forecast)
tru.data <- mlba::ToysRUsRevenues
tru.ts <- ts(tru.data[, 3], start = c(1992, 1), end = c(1995, 4), freq = 4)
autoplot(tru.ts) +
  geom_point(size=0.5) +
  labs(x="Time", y="Revenue ($ millions)") +
  theme_bw()
ggsave(file=file.path("..", "figures", "chapter_04", "ToysRUs.pdf"),
       last_plot())

## Principal Components Analysis
### Example 2: Breakfast Cereals

library(tidyverse)
cereals.df <- mlba::Cereals %>% select(calories, rating)
# compute PCs on two dimensions
pcs <- prcomp(cereals.df %>% select(calories, rating))
summary(pcs)
pcs$rot
scores <- pcs$x
head(scores, 5)


getPCaxis <- function(f, pcs, pcLabel) {
  return (data.frame(
    rbind(pcs$center + f * pcs$rotation[, pcLabel],
          pcs$center - f * pcs$rotation[, pcLabel]))
  )
}
PC1 <- getPCaxis(90, pcs, "PC1")
PC2 <- getPCaxis(50, pcs, "PC2")
ggplot(cereals.df, aes(x=calories, y=rating)) +
  geom_point() +
  geom_line(data=PC1) +
  geom_line(data=PC2) +
  coord_cartesian(xlim=c(0, 200), ylim=c(0, 110)) +
  labs(x="Calories", y="Rating") +
  annotate(geom="text", x=30, y=80, label="z[1]",parse=TRUE) +
  annotate(geom="text", x=120, y=80, label="z[2]",parse=TRUE) +
  theme_bw()

ggsave(file=file.path("..", "figures", "chapter_04", "pca_subset.pdf"),
       width=5, height=3, last_plot())

### Principal Components

# load and preprocess the data
cereals.df <- mlba::Cereals %>%
  column_to_rownames("name") %>%
  select(-c(mfr, type)) %>%
  drop_na()

pcs <- prcomp(cereals.df)
summary(pcs)
pcs$rotation[,1:5]

### Normalizing the Data

# Use function prcomp() with scale. = T to run PCA on normalized data
pcs.cor <- prcomp(cereals.df, scale. = T)

summary(pcs.cor)
pcs.cor$rotation[,1:5]


library(ggrepel)
ggplot(data.frame(pcs.cor$x), aes(x=PC1, y=PC2, label=rownames(pcs.cor$x))) +
  geom_point(shape=21) +
  geom_text_repel(size=2, max.overlaps=7) +
  theme_bw()

f <- 1.3
ggsave(file=file.path("..", "figures", "chapter_04", "pca_full.pdf"),
       width=f * 4, height=f * 5, last_plot())

## Dimension Reduction Using Classification and Regression Trees
### Using Principal Components for Classification and Prediction

  wine.df <- mlba::Wine %>% select(-Type)
  pcs.cor <- prcomp(wine.df)
  summary(pcs.cor)
  pcs.cor$rotation[,1:4]
