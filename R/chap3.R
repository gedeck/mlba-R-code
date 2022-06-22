
if (!require(mlba)) {
  library(devtools)
  install_github("gedeck/mlba/mlba", force=TRUE)
}

# Data Visualization
## Data Examples
### Example 1: Boston Housing Data

housing.df <- mlba::BostonHousing
head(housing.df, 9)

## Basic Charts: Bar Charts, Line 
### Example 2: Ridership on Amtrak Trains

# Amtrak data
library(forecast)
Amtrak.df <- mlba::Amtrak
ridership.ts <- ts(Amtrak.df$Ridership, start=c(1991, 1), end=c(2004, 3), freq=12)

# Boston housing data
housing.df <- mlba::BostonHousing
# compute mean MEDV per CHAS = (0, 1)
MEDV.per.CHAS  <- aggregate(housing.df$MEDV, by=list(housing.df$CHAS), FUN=mean)
names(MEDV.per.CHAS) <- c("CHAS", "MeanMEDV")
MEDV.per.CHAS$CHAS <- factor(MEDV.per.CHAS$CHAS)
# compute % mean CAT.MEDV
CATMEDV.per.CHAS <- aggregate(housing.df$CAT.MEDV, by=list(housing.df$CHAS), FUN=mean)
names(CATMEDV.per.CHAS) <- c("CHAS", "MeanCATMEDV")
CATMEDV.per.CHAS$CHAS <- factor(CATMEDV.per.CHAS$CHAS)


## line chart for the Amtrak data
plot(ridership.ts, xlab="Year", ylab="Ridership (in 000s)", ylim=c(1300, 2300))

## scatter plot with axes names for Boston housing data
plot(housing.df$MEDV ~ housing.df$LSTAT, xlab="LSTAT", ylab="MEDV")

## barchart of CHAS vs. mean MEDV
barplot(MEDV.per.CHAS$MeanMEDV,  names.arg=MEDV.per.CHAS$CHAS,
        xlab="CHAS", ylab="Avg. MEDV")

## barchart of CHAS vs. % CAT.MEDV
barplot(CATMEDV.per.CHAS$MeanCATMEDV * 100,  names.arg=CATMEDV.per.CHAS$CHAS,
        xlab="CHAS", ylab="% of CAT.MEDV")


library(ggplot2)
library(gridExtra)

g1 <- autoplot(ridership.ts) +
  xlab("Year") + ylab("Ridership (in 000)")

## scatter plot with axes names
g2 <- ggplot(housing.df) +
  geom_point(aes(x=LSTAT, y=MEDV), colour="navy", alpha=0.5)

g3 <- ggplot(MEDV.per.CHAS) +
  geom_bar(aes(x=CHAS, y=MeanMEDV, fill=CHAS), stat="identity")

g4 <- ggplot(CATMEDV.per.CHAS) +
  geom_bar(aes(x=CHAS, y=MeanCATMEDV, fill=CHAS), stat="identity") +
  ylab("% of CAT.MEDV")

grid.arrange(g1, g2, g3, g4, ncol=2, nrow=2)


# replace g1 graph with a real ggplot version
g1 <- ggplot(ridership.ts, aes(x=time(ridership.ts), y=ridership.ts)) +
  geom_line() + scale_x_continuous() + scale_y_continuous() +
  xlab("Year") + ylab("Ridership (in 000s)")

g <- arrangeGrob(g1 + theme_bw(),
                 g2 + theme_bw(),
                 g3 + theme_bw() + theme(legend.position="none"),
                 g4 + theme_bw() + theme(legend.position="none"),
                 ncol=2, nrow=2)
ggsave(file=file.path("..", "figures", "chapter_03", "basicPlots.pdf"), g)

### Distribution Plots: Boxplots and Histograms

## histogram of MEDV
hist(housing.df$MEDV, xlab="MEDV")

## boxplot of MEDV for different values of CHAS
boxplot(housing.df$MEDV ~ housing.df$CHAS, xlab="CHAS", ylab="MEDV")


ggplot(housing.df) + geom_histogram(aes(x=MEDV), bins=9)

ggplot(housing.df) + geom_boxplot(aes(x=as.factor(CHAS), y=MEDV)) + xlab("CHAS")


ggplot(housing.df) + geom_histogram(aes(x=MEDV), bins=9)
ggsave(file=file.path("..", "figures", "chapter_03", "Viz-Histogram.pdf"),
       last_plot() + theme_bw(), width=2.5, height=2, units="in")

ggplot(housing.df) + geom_boxplot(aes(x = as.factor(CHAS), y=MEDV)) + xlab("CHAS")
ggsave(file=file.path("..", "figures", "chapter_03", "Viz-Boxplots.pdf"),
       last_plot() + theme_bw(), width=2.5, height=2, units="in")


## side-by-side boxplots
# use par() to split the plots into panels.
par(mfcol=c(1, 4))
boxplot(housing.df$NOX ~ housing.df$CAT.MEDV, xlab="CAT.MEDV", ylab="NOX")
boxplot(housing.df$LSTAT ~ housing.df$CAT.MEDV, xlab="CAT.MEDV", ylab="LSTAT")
boxplot(housing.df$PTRATIO ~ housing.df$CAT.MEDV, xlab="CAT.MEDV", ylab="PTRATIO")
boxplot(housing.df$INDUS ~ housing.df$CAT.MEDV, xlab="CAT.MEDV", ylab="INDUS")


g1 <- ggplot(housing.df, aes(x=factor(CAT.MEDV), y=NOX)) + geom_boxplot()
g2 <- ggplot(housing.df, aes(x=factor(CAT.MEDV), y=LSTAT)) + geom_boxplot()
g3 <- ggplot(housing.df, aes(x=factor(CAT.MEDV), y=PTRATIO)) + geom_boxplot()
g4 <- ggplot(housing.df, aes(x=factor(CAT.MEDV), y=INDUS)) + geom_boxplot()
grid.arrange(g1, g2, g3, g4, ncol=4, nrow=1)


g <- arrangeGrob(g1 + theme_bw() + xlab("CAT.MEDV"),
                 g2 + theme_bw() + xlab("CAT.MEDV"),
                 g3 + theme_bw() + xlab("CAT.MEDV"),
                 g4 + theme_bw() + xlab("CAT.MEDV"),
                 ncol=4, nrow=1)
ggsave(file=file.path("..", "figures", "chapter_03", "Viz-BoxplotPanel.pdf"), g,
       width=10, height=2.5, units="in")

### Heatmaps: Visualizing Correlations and Missing Values

## simple heatmap of correlations (without values)
heatmap(cor(housing.df), Rowv=NA, Colv=NA)

## heatmap with values
library(gplots)
heatmap.2(cor(housing.df), Rowv=FALSE, Colv=FALSE, dendrogram="none",
          cellnote=round(cor(housing.df),2),
          notecol="black", key=FALSE, trace="none", margins=c(10,10))


library(reshape) # to generate input for the plot
cor.mat <- round(cor(housing.df), 2) # rounded correlation matrix
melted.cor.mat <- melt(cor.mat)
ggplot(melted.cor.mat, aes(x=X1, y=X2, fill=value)) +
  geom_tile() + xlab("") + ylab("") +
  scale_fill_distiller(palette="RdBu", limits=c(-1, 1))


g <- last_plot() + theme_bw() +
  theme(axis.text.x=element_text(angle=90, vjust=0.5, hjust=1))
ggsave(file=file.path("..", "figures", "chapter_03", "Viz-HeatmapCorr.pdf"),
       g, width=5.5, height=4.5, units="in")


# replace dataFrame with your data.
dataFrame = mlba::NYPDMotorVehicleCollisions

# we treat empty strings as missing values
# is.na() returns a Boolean (TRUE/FALSE) output indicating the location of missing
# values.
# multiplying the Boolean value by 1 converts the output into binary (0/1).
missing = dataFrame
missing[missing == ""] = NA
missing = 1 * is.na(missing)
heatmap(missing, Rowv=NA, Colv=NA)


library(ggplot2)
library(reshape) # to generate input for the plot
melted.missing <- melt(missing)
ggplot(melted.missing, aes(x=X1, y=X2, fill=value)) +
  geom_tile(show.legend=FALSE) +
  scale_fill_gradient(low="white", high="#666666") +
  scale_x_continuous(expand=c(0, 0))


g <- last_plot() + theme_bw() + xlab("") + ylab("") +
  theme(plot.margin=margin(r=10, t=5))
ggsave(file=file.path("..", "figures", "chapter_03", "Viz-HeatmapMissing.pdf"),
       g, width=8, height=4.2, units="in")

## Multidimensional Visualization
### Adding Variables: Color, Size, Shape, Multiple Panels, and Animation

## color scatter plot
par(xpd=TRUE) # allow legend to be displayed outside of plot area
plot(housing.df$NOX ~ housing.df$LSTAT, ylab="NOX", xlab="LSTAT",
     col=ifelse(housing.df$CAT.MEDV == 1, "blue", "red"))
legend("topleft", inset=c(0, -0.2), legend=c("CAT.MEDV = 1", "CAT.MEDV = 0"),
       col=c("blue", "red"), pch=1, cex=0.5)

## panel plots
# compute mean MEDV per RAD and CHAS
# In aggregate() use argument drop = FALSE to include all combinations
# (exiting and missing) of RAD X CHAS.
data.for.plot <- aggregate(housing.df$MEDV, by=list(housing.df$RAD, housing.df$CHAS),
                           FUN=mean, drop=FALSE)
names(data.for.plot) <- c("RAD", "CHAS", "meanMEDV")
# plot the data
par(mfcol=c(2,1))
barplot(height=data.for.plot$meanMEDV[data.for.plot$CHAS == 0],
        names.arg=data.for.plot$RAD[data.for.plot$CHAS == 0],
        xlab="RAD", ylab="Avg. MEDV", main="CHAS = 0")
barplot(height=data.for.plot$meanMEDV[data.for.plot$CHAS == 1],
        names.arg=data.for.plot$RAD[data.for.plot$CHAS == 1],
        xlab="RAD", ylab="Avg. MEDV", main="CHAS = 1")


g1 <- ggplot(housing.df, aes(x=LSTAT, y=NOX, color=factor(CAT.MEDV))) +
  labs(color="CAT.MEDV") + geom_point(alpha=0.6)

g2 <- ggplot(data.for.plot) +
  geom_bar(aes(x=factor(RAD), y=meanMEDV), stat="identity") +
  labs(x="RAD", y="Avg. MEDV") +
  facet_grid(CHAS ~ .)

grid.arrange(g1, g2, ncol=2)


g <- arrangeGrob(g1 + theme_bw(),
                 g2 + theme_bw() + theme(legend.position="none"),
                 ncol=2, nrow=1)
ggsave(file=file.path("..", "figures", "chapter_03", "Viz-ColorPanel.pdf"), g,
       width=8, height=3, units="in")


## simple plot
# use plot() to generate a matrix of 4X4 panels with variable name on the diagonal,
# and scatter plots in the remaining panels.
plot(housing.df[, c(1, 3, 12, 13)])


library(GGally)
ggpairs(housing.df[, c(1, 3, 12, 13)],
  lower=list(continuous=wrap("points", alpha=0.25, size=0.3)))


ggsave(file=file.path("..", "figures", "chapter_03", "Viz-MatrixScatterplot.pdf"),
       last_plot(), width=4, height=4, units="in")

### Manipulations: Rescaling, Aggregation and Hierarchies, Zooming, Filtering

options(scipen=999) # avoid scientific notation

## scatter plot: regular and log scale
plot(housing.df$MEDV ~ housing.df$CRIM, xlab="CRIM", ylab="MEDV")
# to use logarithmic scale set argument log = to either 'x', 'y', or 'xy'.
plot(housing.df$MEDV ~ housing.df$CRIM,
    xlab="CRIM", ylab="MEDV", log="xy")

## boxplot: regular and log scale
boxplot(housing.df$CRIM ~ housing.df$CAT.MEDV,
    xlab="CAT.MEDV", ylab="CRIM")
boxplot(housing.df$CRIM ~ housing.df$CAT.MEDV,
    xlab="CAT.MEDV", ylab="CRIM", log="y")


## scatter plot: regular and log scale
# alternative log-scale plot with ggplot
g1 <- ggplot(housing.df) + geom_point(aes(x=CRIM, y=MEDV))
g2 <- g1 +
  scale_x_log10(breaks=10^(-2:2),
    labels=format(10^(-2:2), scientific=FALSE, drop0trailing=TRUE))
  scale_y_log10(breaks=c(5, 10, 20, 40))

## boxplot: regular and log scale
g3 <- ggplot(housing.df, aes(x=factor(CAT.MEDV), y=CRIM)) + geom_boxplot()
g4 <- g3 + scale_y_log10()

grid.arrange(g1, g2, g3, g4, ncol=2)


g <- arrangeGrob(g1+theme_bw(), g2+theme_bw(),
                 g3+theme_bw(), g4+theme_bw(),
                 ncol=2, nrow=2)
ggsave(file=file.path("..", "figures", "chapter_03", "Viz-LogScale.pdf"), g,
       width=5, height=4, units="in")

#### Aggregation and Hierarchies

library(forecast)
Amtrak.df <- mlba::Amtrak
ridership.ts <- ts(Amtrak.df$Ridership, start=c(1991, 1), end=c(2004, 3), freq=12)
## fit curve
ridership.lm <- tslm(ridership.ts ~ trend + I(trend^2))

plot(ridership.ts, xlab="Year", ylab="Ridership (in 000s)", ylim=c(1300, 2300))
lines(ridership.lm$fitted, lwd=2)

## zoom in, monthly, and annual plots
ridership.2yrs <- window(ridership.ts, start=c(1991,1), end=c(1992,12))
plot(ridership.2yrs, xlab="Year", ylab="Ridership (in 000s)", ylim=c(1300, 2300))
monthly.ridership.ts <- tapply(ridership.ts, cycle(ridership.ts), mean)
plot(monthly.ridership.ts, xlab="Month", ylab="Average Ridership",
    ylim=c(1300, 2300), type="l", xaxt="n")
## set x labels
axis(1, at=c(1:12), labels=c("Jan","Feb","Mar", "Apr","May","Jun",
                             "Jul","Aug","Sep",  "Oct","Nov","Dec"))

annual.ridership.ts <- aggregate(ridership.ts, FUN=mean)
plot(annual.ridership.ts, xlab="Year", ylab="Average Ridership",
    ylim=c(1300, 2300))


g1 <- autoplot(ridership.ts) + ylim(1300, 2300) +
  labs(x="Year", y="Ridership (in 000s)") +
  geom_smooth(formula=y ~ poly(x, 2), method="lm",
              colour="navy", se=FALSE, na.rm=TRUE) +
  scale_x_continuous(n.breaks=10)

## zoom in, monthly, and annual plots
g2 <- autoplot(window(ridership.ts, start=c(1991,1), end=c(1992,12))) +
  ylim(1300, 2300) +
  labs(x="Year", y="Ridership (in 000s)")

months=c("Jan", "Feb", "Mar", "Apr", "May", "Jun",
         "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")
df <- data.frame(
  monthly.ridership=tapply(ridership.ts, cycle(ridership.ts), mean),
  month=factor(months, levels=months))
g3 <- ggplot(df, aes(x=month, y=monthly.ridership)) + ylim(1300, 2300) +
  geom_line(group=1) +
  labs(x="Month", y="Average Ridership")

annual.ridership.ts <- aggregate(ridership.ts, FUN=mean)
g4 <- autoplot(annual.ridership.ts) + ylim(1300, 2300) +
  labs(x="Year", y="Average Ridership")


g <- arrangeGrob(g1+theme_bw() + theme(plot.margin=margin(t=5, r=10)),
                 g3+theme_bw() + theme(plot.margin=margin(t=23, r=10)),
                 g2+theme_bw() + theme(plot.margin=margin(t=5, r=10)),
                 g4+theme_bw() + theme(plot.margin=margin(t=5, r=10)),
                 ncol=2, nrow=2)
ggsave(file=file.path("..", "figures", "chapter_03", "Viz-AmtrakTimePlotTS.pdf"), g,
       width=8, height=5, units="in")

#### Filtering

utilities.df <- mlba::Utilities

plot(utilities.df$Fuel_Cost ~ utilities.df$Sales,
     xlab = "Sales", ylab = "Fuel Cost", xlim = c(2000, 20000))
text(x = utilities.df$Sales, y = utilities.df$Fuel_Cost,
     labels = utilities.df$Company, pos = 4, cex = 0.8, srt = 20, offset = 0.2)


library(ggrepel)
ggplot(utilities.df, aes(y = Fuel_Cost, x = Sales)) +
  geom_point() +
  geom_text_repel(aes(label=Company)) +
  labs(x="Sales", y="Fuel Cost")


ggsave(file=file.path("..", "figures", "chapter_03", "Viz-LabeledScatter.pdf"),
       last_plot() + theme_bw(), width=5, height=5, units="in")

### Reference: Trend Lines and Labels

# use function alpha() in library scales to add transparent colors
library(scales)
universal.df <- mlba::UniversalBank
no_personal_loan <- subset(universal.df, Personal.Loan == 0)
personal_loan <- subset(universal.df, Personal.Loan == 1)
plot(jitter(no_personal_loan$CCAvg, 1) ~ jitter(no_personal_loan$Income, 1),
     col=alpha("lightblue", 0.5), pch=20, xlab="Income", ylab="CCAvg")
points(jitter(personal_loan$CCAvg, 1) ~ jitter(personal_loan$Income, 1),
       col="steelblue", pch=20, xlab="Income", ylab="CCAvg")


ggplot(universal.df, aes(x=Income, y=CCAvg)) +
    geom_jitter(data=no_personal_loan, width=5, height=0.2, alpha=0.5,
                color="lightblue") +
    geom_jitter(data=personal_loan, width=5, height=0.2, color="steelblue") +
    labs(colour="Personal\nLoan")


g = last_plot() + theme_bw() +
  scale_color_brewer(palette="Set1",
    guide=guide_legend(override.aes=list(size=3, alpha=1)))
ggsave(file=file.path("..", "figures", "chapter_03", "Viz-LargeData.pdf"),
       g, width=7, height=5, units="in")

### Multivariate Plot: Parallel Coordinates Plot

library(MASS)
par(mfcol = c(2,1))
parcoord(housing.df[housing.df$CAT.MEDV == 0, -14], main = "CAT.MEDV = 0")
parcoord(housing.df[housing.df$CAT.MEDV == 1, -14], main = "CAT.MEDV = 1")


library(GGally)

ggparcoord(subset(housing.df, select=-CAT.MEDV), scale="uniminmax", alpha=0.3) +
  facet_wrap(vars(housing.df$CAT.MEDV), nrow=2)


g = last_plot() + theme_bw() +
  facet_wrap(vars(housing.df$CAT.MEDV), nrow=2,
             labeller=as_labeller(c(`0` = "CAT.MEDV: 0", `1` = "CAT.MEDV: 1")))
ggsave(file=file.path("..", "figures", "chapter_03", "Viz-Parallel.pdf"),
       g, width=7, height=5, units="in")

### Interactive Visualization

library(plotly)
g <- ggplot(housing.df) + geom_point(aes(x=CRIM, y=MEDV))
ggplotly(g)

## Specialized Visualizations
### Visualizing Networked Data

library(igraph)
ebay.df <- mlba::eBayNetwork

# transform node ids to factors
ebay.df[,1] <- as.factor(ebay.df[,1])
ebay.df[,2] <- as.factor(ebay.df[,2])

graph.edges <- as.matrix(ebay.df[,1:2])
g <- graph.edgelist(graph.edges, directed=FALSE)
isBuyer <- sapply(V(g)$name, function(node){ node %in% ebay.df[,2] })

plot(g, vertex.label=NA, vertex.color=ifelse(isBuyer,  "lightblue", "steelblue"),
     vertex.size=ifelse(isBuyer, 7, 10))


set.seed(1)


library(ggnetwork)
g = set_vertex_attr(g, "isBuyer", value=ifelse(isBuyer, "buyer", "seller"))
ggplot(g, aes(x=x, y=y, xend=xend, yend=yend)) +
    geom_edges(color="grey50") +
    geom_nodes(aes(color=isBuyer, size=isBuyer)) +
    scale_color_manual(values = c("buyer"="lightblue", "seller"="steelblue")) +
    scale_size_manual(values = c("buyer"=2, "seller"=5)) +
    theme_blank(legend.position="none")


ggsave(file=file.path("..", "figures", "chapter_03", "Viz-Network.pdf"),
       last_plot(), width=4, height=4, units="in")

### Visualizing Hierarchical Data: Treemaps

library(treemapify)
# load data and add column for negative feedback
tree.df <- mlba::eBayTreemap
tree.df$negative.feedback <- 1* (tree.df$Seller.Feedback < 0)
data <- aggregate(. ~ Brand + Category + Sub.Category, data=tree.df, mean)

ggplot(data, aes(area=High.Bid, subgroup=Category, fill=-negative.feedback,
                 label=Brand)) +
    geom_treemap() + geom_treemap_subgroup_border() +
    geom_treemap_subgroup_text(place="topleft", colour="black",
                               min.size=5, size=12) +
    geom_treemap_text(fontface="italic", colour="black", place="centre",
                      min.size=5, size=8) +
    scale_fill_gradient(low="#0396CC", high="#ABDCFF")


g <- last_plot() + theme_blank(legend.position="none")
ggsave(file=file.path("..", "figures", "chapter_03", "Viz-Treemap-eBay.pdf"),
       g, width=7, height=4, units="in")

### Visualizing Geographical Data: Map Charts

library(ggmap)
us <- c(left=-125, bottom=24, right=-67, top=50)
SCstudents <- mlba::SCstudents
map <- get_stamenmap(us, zoom=5)
ggmap(map) +
  geom_point(aes(x=longitude, y=latitude), data=SCstudents,
    alpha=0.4, colour="red", size=0.75)


g <- last_plot()
ggsave(file=file.path("..", "figures", "chapter_03", "Viz-GoogleMapScom.pdf"),
       g, width=7, height=4, units="in")


library(mosaic)

gdp.df <- mlba::GDP
names(gdp.df)[5] <- "GDP2015"
happiness.df <- mlba::Veerhoven

# gdp map
g1 <- mWorldMap(gdp.df, key="Country.Name", fill="GDP2015") + coord_map()

# well-being map
g2 <- mWorldMap(happiness.df, key="Nation", fill="Score") + coord_map() +
  scale_fill_continuous(name="Happiness")
grid.arrange(g1, g2, nrow=2)


g1 <- g1 + scale_fill_continuous(breaks=c(5e12, 10e12, 15e12),
                                 labels=c("5 trillion", "10 trillion", "15 trillion"))
ggsave(file=file.path("..", "figures", "chapter_03", "Viz-Worldmap.pdf"),
       arrangeGrob(g1, g2, nrow=2))
