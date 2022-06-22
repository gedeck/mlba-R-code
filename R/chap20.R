
if (!require(mlba)) {
  library(devtools)
  install_github("gedeck/mlba/mlba", force=TRUE)
}
set.seed(1)

# Social Network Analytics
## Introduction

library(igraph)

# define links in data
edges <- rbind(
  c("Dave", "Jenny"), c("Peter", "Jenny"), c("John", "Jenny"),
  c("Dave", "Peter"), c("Dave", "John"), c("Peter", "Sam"),
  c("Sam", "Albert"), c("Peter", "John")
)

# generate and plot network
# set argument directed = FALSE in graph.edgelist() to plot an undirected network.
g <- graph.edgelist(edges, directed = FALSE)
plot(g, vertex.size = 5, vertex.label.dist = 2)


pdf(file=file.path("..", "figures", "chapter_20", "fig1.pdf"), width=5, height=5)
    par(mar=c(0,0,0,1)+.1)
    plot(g, vertex.size = 5, vertex.label.dist = 2)
dev.off()

## Directed vs. Undirected Networks

# generate and plot network
# set argument directed = TRUE in graph.edgelist() to plot a directed network.
g <- graph.edgelist(edges, directed = TRUE)
plot(g, vertex.size = 5, vertex.label.dist = 2)


pdf(file=file.path("..", "figures", "chapter_20", "fig2.pdf"), width=5, height=5)
    par(mar=c(0,0,0,1)+.5)
    plot(g, vertex.size = 5, vertex.label.dist=2)
dev.off()


edges <- rbind(c("A", "B"), c("B", "C"), c("C", "A"))
g <- graph.edgelist(edges, directed = FALSE)
E(g)$width <- c(20, 5, 5)
plot(g, vertex.size = 5, vertex.label.dist = 2)

pdf(file=file.path("..", "figures", "chapter_20", "fig3.pdf"), width=2, height=2)
    par(mar=c(0,0,0,0)+.2)
    plot(g, vertex.size = 5, vertex.label.dist = 2)
dev.off()
# $

## Visualizing and Analyzing Networks
### Plot Layout

library(igraph)
drug.df <- mlba::Drug

# convert edges to edge list matrix
edges <- as.matrix(drug.df[, c(1,2)])
g <- graph.edgelist(edges,directed=FALSE)

# plot network
# nodes' size is proportional to their eigenvector centrality
plot(g, vertex.label = NA, vertex.size = eigen_centrality(g)$vector * 20)


pdf(file=file.path("..", "figures", "chapter_20", "SNA_Drug_Laundry.pdf"), width=5, height=5)
    par(mar=c(0,0,0,0)+.1)
    plot(g, vertex.label = NA, vertex.size = eigen_centrality(g)$vector * 20)
dev.off()


edges <- rbind(
  c("Dave", "Jenny"), c("Peter", "Jenny"), c("John", "Jenny"),
  c("Dave", "Peter"), c("Dave", "John"), c("Peter", "Sam"),
  c("Sam", "Albert"), c("Peter", "John")
)
g <- graph.edgelist(edges)
pdf(file=file.path("..", "figures", "chapter_20", "fig5_circle.pdf"), width=3, height=3)
    par(mar=c(0,0,0,1)+.7)
    plot(g, layout = layout_in_circle, vertex.size = 5, vertex.label.dist = 2)
dev.off()
pdf(file=file.path("..", "figures", "chapter_20", "fig5_grid.pdf"), width=3, height=3)
    par(mar=c(0,0,0,1)+.7)
    plot(g, layout = layout_on_grid, vertex.size = 5, vertex.label.dist = 2)
dev.off()


# Building on the code presented in Figure 19.1
plot(g, layout = layout_in_circle, vertex.size = 5, vertex.label.dist = 2)
plot(g, layout = layout_on_grid, vertex.size = 5, vertex.label.dist = 2)

## Social Data Metrics and Taxonomy
### Node-Level Centrality Metrics

edges <- rbind(
  c("Dave", "Jenny"), c("Peter", "Jenny"), c("John", "Jenny"),
  c("Dave", "Peter"), c("Dave", "John"), c("Peter", "Sam"),
  c("Sam", "Albert"), c("Peter", "John")
)
g <- graph.edgelist(edges, directed=FALSE)

degree(g)
betweenness(g)
betweenness(g)/sum(betweenness(g))
closeness(g)
eigen_centrality(g)

### Egocentric Network

# get Peter's 1-level ego network
# for a 2-level ego network set argument order = 2 in make_ego_graph().
peter.ego <- make_ego_graph(g, order = 1, nodes = "Peter")
plot(peter.ego[[1]], vertex.size = 1, vertex.label.dist = 0.5)


pdf(file=file.path("..", "figures", "chapter_20", "fig6_1.pdf"), width=5, height=5)
    par(mar=c(0,0,0,1)+.5)
    peter.ego <- make_ego_graph(g, order = 1, nodes = "Peter")
    g.ego <- peter.ego[[1]]
    V(g.ego)$color <- "orange"
    V(g.ego)["Peter"]$color <- "red"
    plot(g.ego, vertex.size = 5, vertex.label.dist = 2)
dev.off()
pdf(file=file.path("..", "figures", "chapter_20", "fig6_2.pdf"), width=5, height=5)
    par(mar=c(0,0,0,1)+.5)
    peter.ego <- make_ego_graph(g, order = 2, nodes = "Peter")
    g.ego <- peter.ego[[1]]
    V(g.ego)$color <- "orange"
    V(g.ego)["Peter"]$color <- "red"
    plot(g.ego, vertex.size = 5, vertex.label.dist = 2)
dev.off()

### Network Metrics

degree.distribution(g) # normalized
edge_density(g)

## Collecting Social Network Data with R
### Collaborative Filtering

library(twitteR)
# replace key and secret number with those you obtained from Twitter
setup_twitter_oauth(consumer_key = "XXX", consumer_secret = "XXX",
                    access_token = "XXX", access_secret = "XXX")

# get recent tweets
recent.25.tweets <- searchTwitter("text mining", resultType="recent", n = 25)


library(Rfacebook)
# replace the app id and secret number with those you obtained from Facebook
fb_oauth <- fbOAuth(app_id = "XXX", app_secret = "XXX")
fb_oauth_credentials <- fromJSON(names(fb_oauth$credentials))

# get recent posts on page "dataminingbook"
fb_page <- getPage(page = "dataminingbook", token = fb_oauth_credentials$access_token)

# a facebook page contains the following information:
t(t(names(fb_page)))
fb_page[1,]

# get information about most recent post
post <- getPost(post=fb_page$id[1], n=20, token=fb_oauth_credentials$access_token)

post$likes
post$comments

