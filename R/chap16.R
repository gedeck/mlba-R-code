
if (!require(mlba)) {
  library(devtools)
  install_github("gedeck/mlba/mlba", force=TRUE)
}

# Cluster Analysis
## Measuring Distance Between Two Records
### Euclidean Distance

library(tidyverse)
# load data and use Company column as row names
utilities.df <- mlba::Utilities %>%
  column_to_rownames("Company")

# compute Euclidean distance
# (to compute other distance measures, change the value in the method argument)
d <- dist(utilities.df, method = "euclidean")

### Normalizing Numerical Variables

# normalize input variables
utilities.df.norm <- scale(utilities.df)

# compute normalized distance based on Sales and Fuel Cost
d.norm <- dist(utilities.df.norm[,c("Sales","Fuel_Cost")], method="euclidean")

## Hierarchical (Agglomerative) Clustering
### Dendrograms: Displaying Clustering Process and Results

library(ggplot2)
library(ggdendro)
d.norm <- dist(utilities.df.norm, method="euclidean")

# in hclust() set argument method \galit{to}
# "ward.D", "single", "complete", "average", "median", or "centroid"
hc1 <- hclust(d.norm, method="single")
plot(hc1, hang=-1, ann=FALSE)  # use baseR
ggdendrogram(hc1)      # use ggdendro package (shown in figure below)
hc2 <- hclust(d.norm, method="average")
plot(hc2, hang=-1, ann=FALSE)
ggdendrogram(hc2)


library(gridExtra)
addCutline <- function(g, hc, ncluster) {
  heights <- rev(hc$height)
  cut_at <- 0.5 * (heights[ncluster] + heights[ncluster - 1])
  return (g + geom_hline(yintercept=cut_at, color='red', linetype=2))
}
g1 <- ggdendrogram(hc1)
g2 <- ggdendrogram(hc2)
grid.arrange(addCutline(g1, hc1, 6), addCutline(g2, hc2, 6), nrow=2)
g <- arrangeGrob(addCutline(g1, hc1, 6), addCutline(g2, hc2, 6), nrow=2)
ggsave(file=file.path("..", "figures", "chapter_16", "utilities-dendrograms.pdf"),
       g, width=5, height=8, units="in")


memb <- cutree(hc1, k = 6)
memb
memb <- cutree(hc2, k = 6)
memb

### Validating Clusters

# set labels as cluster membership and utility name
row.names(utilities.df.norm) <- paste(memb, ": ", row.names(utilities.df), sep = "")

# plot heatmap
heatmap(utilities.df.norm, Colv=NA, hclustfun=hclust)


# grey scale
# rev() reverses the color mapping to large = dark
heatmap(as.matrix(utilities.df.norm), Colv = NA, hclustfun = hclust,
        col=rev(paste("gray",1:99,sep="")))

pdf(file=file.path("..", "figures", "chapter_16", "utilities-heatmap.pdf"),
    width=5, height=5)
heatmap(utilities.df.norm, Colv=NA, hclustfun=hclust)
dev.off()

## Non-hierarchical Clustering: The 
### Choosing the Number of Clusters ($k$)

set.seed(123) # set random seed for reproducability
# load and preprocess data
utilities.df <- mlba::Utilities %>%
  column_to_rownames("Company")

# normalized distance:
utilities.df.norm <- scale(utilities.df)

# run kmeans algorithm
km <- kmeans(utilities.df.norm, 6)

# show cluster membership
sort(km$cluster)


  # centroids
  km$centers
  # within-cluster sum of squares
  km$withinss
  # cluster size
  km$size


library(GGally)
centroids <- data.frame(km$centers)
centroids['Cluster'] = paste('Cluster', seq(1, 6))

ggparcoord(centroids, columns=1:8, groupColumn='Cluster', showPoints=TRUE) +
    scale_color_viridis_d() +
    labs(x='Variable', y='Value')


  ggsave(file=file.path("..", "figures", "chapter_16", "utilities-clusterProfile.pdf"),
         last_plot() + theme_bw(), width=8.5, height=3.2, units="in")


result <- tibble()
for (k in 1:6) {
  km <- kmeans(utilities.df.norm, k)
  result <- bind_rows(result, tibble(k=k, average_withinss=mean(km$withinss)))
}

ggplot(result, aes(x=k, y=average_withinss)) +
  geom_line() +
  geom_point() +
  labs(y="Average within-cluster squared distance",
       x=expression(paste("Number of clusters ", italic("k")))) +
  theme_bw()
ggsave(file=file.path("..", "figures", "chapter_16", "utilities-ellbow.pdf"),
       last_plot(), width=4, height=4, units="in")


  dist(km$centers)
