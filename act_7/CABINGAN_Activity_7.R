#CABINGAN, Stephanie
#Activity 7

# ggplot
install.packages("factoextra")
library(factoextra)

# Read Data
df <- read.csv("morphology_data.csv")

# Exclude sex and population column
clmph_data <- df[,-2]
clmph_data

# Cleaning
good <- complete.cases(clmph_data)
head(clmph_data[good, ])
morpho_data <- clmph_data[good, ]

# Standardization
morpho_scaled <- scale(morpho_data[-1])

# Kmeans
kmeans_morpho <- kmeans(morpho_scaled, centers = 3, nstart = 25)

# View cluster assignments
head(kmeans_morpho$cluster)

# Summary of results
kmeans_morpho

# Scatterplot
SVL <- morpho_scaled[,1]
TD <- morpho_scaled[,2]
plot(
  SVL, TD,
  col = kmeans_morpho$cluster,
  pch = 19,
  xlab = "SVL (scaled)",
  ylab = "TD (scaled)",
  main = "Morphology K-means Clustering"
)

points(kmeans_morpho$centers, col = 1:3, pch = 8, cex = 2, lwd = 2)

# Kmeans Loop
ss <- numeric(10)
                 
for(k in 1:10){
  kmeans(morpho_scaled, centers = k, nstart = 25)
  ss[k] <- kmeans_morpho$tot.withinss
}

# Elbow Curve
fviz_nbclust(
  morpho_scaled,
  kmeans,
  method = "wss",
  k.max = 10
)

# Kmeans cluster w/ Optimal K
kml <- kmeans(morpho_scaled, centers = 2, nstart = 10)

# View cluster assignments
head(kml$cluster)

# Summary of results
kml


# Scatterplot 2.0
SVL_2 <- morpho_scaled[,1]
TD_2 <- morpho_scaled[,2]

plot(
  SVL_2, TD_2,
  col = kml$cluster,
  pch = 19,
  xlab = "SVL (scaled)",
  ylab = "TD (scaled)",
  main = "Morphology K-means Clustering with K = 2"
)

points(kmeans_morpho$centers, col = 1:2, pch = 19, cex = 2, lwd = 2)
