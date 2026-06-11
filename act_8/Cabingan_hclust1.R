# CABINGAN, Stephanie Selene Isabel  E.
# Activity 8

## A. 

# Read Data
df <- read.csv("morphology_data.csv")

# Exclude sex and population column
clmph_data <- df[,3:19]
clmph_data

# Cleaning
good <- complete.cases(clmph_data)
head(clmph_data[good, ])
morpho_data <- clmph_data[good,]

# Standardization
morpho_scaled <- scale(morpho_data)

# Compute Euclidean Distance
d <- dist(morpho_scaled, method = "euclidean")

# Single Linkage
mp_single <- hclust(d, method = "single")
rownames(morpho_scaled) <- df$Pop

plot(
  mp_single,
  main = "Single Linkage",
  labels = rownames(morpho_scaled),
  cex = 0.7
)

# Ward's Linkage
mp_ward <- hclust(d, method = "ward.D2")

plot(
  mp_ward,
  main = "Ward D2",
  labels = rownames(morpho_scaled),
  cex = 0.7
)

# Single Linkage Loop
mpsingle_loop <- numeric(10)

for(k in 2:10){
  mps_clusters <- cutree(mp_single, k)
  mps_sil <- silhouette(mps_clusters, d)
  mpsingle_loop[k] <- mean(mps_sil[, 3])
}

plot(
  mpsingle_loop,
  type = "b",
  xlab = "Cluster No.",
  ylab = "Silhouette Width",
  main = "Silhouette Scores for Single Linkage Method"
)

# Ward Method Loop
mpw_loop <- numeric(10)

for(k in 2:10){
  mpw_clusters <- cutree(mp_ward, k)
  mpw_sil <- silhouette(mpw_clusters, d)
  mpw_loop[k] <- mean(mpw_sil[, 3])
}

plot(
  mpw_loop,
  type = "b",
  xlab = "Cluster No.",
  ylab = "Silhouette Width",
  main = "Silhouette Scores for Ward Method"
)


# Single Linkage Loop Dendrogram
opk_single <- which.max(mpsingle_loop) + 1
rownames(morpho_scaled) <- df$Pop

plot(
  mp_single,
  main = "Single Linkage Loop with K = 2",
  labels = rownames(morpho_scaled),
  cex = 0.7
)

rect.hclust(
  mp_single,
  k = opk_single,
  border = c("lightblue", "pink")
)

# Ward Linkage Loop Dendrogram
opk_ward <- which.max(mpw_loop) + 1
rownames(morpho_scaled) <- df$Pop

plot(
  mp_ward,
  main = "Ward Linkage Loop with K = 2",
  labels = rownames(morpho_scaled),
  cex = 0.7
)

rect.hclust(
  mp_ward,
  k = opk_ward,
  border = c("green", "yellow")
)

## B.


