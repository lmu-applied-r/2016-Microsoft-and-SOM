
# Miscellaneous -----------------------------------------------------------

# Install packages from CRAN
# install.packages("kohonen")

# Import packages
library(kohonen)

# Import data (preprocessed TaFeng Grocery data)
load("taFengGrocery.Rda")

# Reproducibility
set.seed(42)

# Colourpalette used for the heatmaps
cpalette <- function(n, alpha = 1) {
  colorRampPalette(c("blue","cyan","green","yellow","orange","red"))(n)
}

# Winsorizing outliers to x-quantiles
winsorize <- function(x, minval = NULL, maxval = NULL,
                      probs=c(0.05, 0.95)){
  
  if(is.null(minval) || is.null(maxval)){
    quantiles <- quantile(x=x, probs=probs, na.rm = TRUE)
    if(is.null(minval)) minval <- quantiles[1]
    if(is.null(maxval)) maxval <- quantiles[2]
  }
  
  x[x<minval] <- minval
  x[x>maxval] <- maxval
  
  return(x)
}

# Data Preprocessing ------------------------------------------------------

# In order to guarantee equal influence for all features we have to
# eliminate all outliers and normalise the features.

# Select Features
taFeng <- taFeng[,c("totalBaskets","totalItems","avgItems","totalMoney",
                    "avgItemPrice","prcSalesItems")]

# Winsorise Features
taFeng <- sapply(taFeng, winsorize)

# Normalise Features
taFeng <- scale(taFeng)

# SOM Grid ----------------------------------------------------------------

# Usually SOM grids are based on the 'hexagonal' layout. Compared to the
# 'rectangular' layout every node has more neighbours.

taFeng.grid <- somgrid(xdim = 16, ydim = 16, topo = "hexagonal")
plot(taFeng.grid)

# SOM Training ------------------------------------------------------------

# som() allows us to create a Self-organising map in two dimensions. 
# This implementation uses the Euclidean distance for identifying the Best
# Matching Unit (BMU). 
# If the data contains categorical features you have to dummycode them first.

taFeng.som <- som(data = as.matrix(taFeng),
                  grid = taFeng.grid,
                  rlen = 250,
                  alpha = c(0.05, 0.01), #default
                  toroidal = FALSE, #default
                  n.hood = "circular") #default


# SOM Visualisations ------------------------------------------------------

# For analyzing the SOM the package offers a wide variety of evalution plots.

# 1. Training Progress
plot(taFeng.som, type = "changes")

# Shows the mean distance to the closest codebook vector during training. The
# line should reach a minimum plateau. If that is not the case you have to
# increase the 'rlen' parameter in som().

# 2. Counts
plot(taFeng.som, type = "counts")
# Shows the number of objects mapped to the individual units. Empty units are 
# depicted in gray. If there are too many gray nodes you have to use a smaller
# grid. You should aim for at least 5-10 observations per node.

# 3. Neighbour Disctance / Unified distance matrix (U-Matrix)
plot(taFeng.som, type = "dist.neighbours")
# Visualises the distance between node weights of neighbouring nodes. Large
# distances indicate higher dissimilarities. Sometimes you can already see
# some clusters in this plot.

# 4. Codes / Fan Diagram
plot(taFeng.som, type = "codes")
# Visualises the weight vector for every node. In this plot you can identify
# first patterns but it will only be useful if the number of features is samll.

# 5. Heatmaps

# If there are 5 or more features the fan diagram can become confusing.
# The solution is to plot every feature in its own heatmap.

par(mfrow = c(3,2))

plot(taFeng.som, type = "property", property = taFeng.som$codes[,1], main = colnames(taFeng.som$codes)[1], palette.name = cpalette)
plot(taFeng.som, type = "property", property = taFeng.som$codes[,2], main = colnames(taFeng.som$codes)[2], palette.name = cpalette)
plot(taFeng.som, type = "property", property = taFeng.som$codes[,3], main = colnames(taFeng.som$codes)[3], palette.name = cpalette)
plot(taFeng.som, type = "property", property = taFeng.som$codes[,4], main = colnames(taFeng.som$codes)[4], palette.name = cpalette)
plot(taFeng.som, type = "property", property = taFeng.som$codes[,5], main = colnames(taFeng.som$codes)[5], palette.name = cpalette)
plot(taFeng.som, type = "property", property = taFeng.som$codes[,6], main = colnames(taFeng.som$codes)[6], palette.name = cpalette)

par(mfrow = c(1,1))


# Clustering --------------------------------------------------------------

# After creating the SOM we often want to cluster the observations. But we
# first have to identify a decent value for the number of clusters.
# We can leverage the within-cluster sum of squares that is calculated
# by the kmeans. After creating the plot we are looking for the so called
# elbow criteria.

wss <- (nrow(taFeng.som$codes)-1)*sum(apply(taFeng.som$codes,2,var)) 
for (i in 2:15)  wss[i] <- sum(kmeans(taFeng.som$codes, centers=i)$withinss)
plot(1:15,wss, type = "b", xlab = "Number of clusters", ylab = " within-cluster sum of squares")

# Colour palette for the clusters
pretty_palette <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", 
                    "#0072B2", "#D55E00", "#CC79A7")

# hierarchical clustering
taFeng.cluster <- cutree(hclust(dist(taFeng.som$codes)), 5)
# The last parameter is indicating that we are building five clusters.

# Plot clusters
plot(taFeng.som, type="mapping", bgcol = pretty_palette[taFeng.cluster], main = " ") 
add.cluster.boundaries(taFeng.som, taFeng.cluster)

# The cluster boundaries can also be plotted in other plots by simply
# using the add.cluster.boundaries() function.

plot(taFeng.som, type = "property", property = taFeng.som$codes[,1], main = colnames(taFeng.som$codes)[1], palette.name = cpalette)
add.cluster.boundaries(taFeng.som, taFeng.cluster)

plot(taFeng.som, type = "codes", bgcol = pretty_palette[taFeng.cluster])
add.cluster.boundaries(taFeng.som, taFeng.cluster)

# Now it's your task to do the interpretation on the created clusters. If
# you can't do a decent interpretation just change some parameters and try
# again :)
