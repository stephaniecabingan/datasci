# CABINGAN, Stephanie Selene Isabel E.
# ACTIVITY 9

# Install packages
install.packages("devtools")
devtools::install_github("arleyc/PCAtest")
library(PCAtest)
library(MASS)

install.packages("ggplot2")
library(ggplot2)

install.packages("dpylr")
library(dplyr)

install.packages("factoextra")
library(factoextra)

# A. 

# Read data
df <- read.csv("Meneses_et.al_2026_Platymantis.csv")

# Subset Species and Morphological Data
species_data <- df[,-c(2:9)]
species_data

# Data Structure and Class
class(spmo_data)
str(spmo_data)

# Cleaning data
good <- complete.cases(species_data)
head(species_data[good, ])
spmo_data <- species_data[good,]

# Scaling data
numeric_data <- sapply(spmo_data, is.numeric)
spmo_scaled <- scale(spmo_data[,numeric_data])
spmo_scaled

# Pairwise correlation
spmo_corr <- cor(spmo_scaled)

round(spmo_corr, 2)

# Eigenvalues
eig <- eigen(spmo_corr)
eig

# Proportion of variance
spmo_var <- eig$values / sum(eig$values)
spmo_var

# Eigenvalues to percentages of total variance
spmo_pervar <- (eig$values / sum(eig$values)) * 100

# Comptute PC scores
eig_vec <- eig$vectors
scores <- spmo_scaled %*% eig_vec

head(scores)

# Plot
spmo_pca <- prcomp(spmo_scaled)
species <- as.factor(spmo_data$Species)
colors <- as.factor(species)

par(mfrow = c(1,2))

plot(
  spmo_pca,
  type = "lines",
  main = "Scree Plot"
)

plot(
  spmo_pca$x[,1],
  spmo_pca$x[,2],
  col = colors,
  pch = 16,
  cex = 0.6,
  xlab = "PC1",
  ylab = "PC2",
  main = "PCA Biplot"
)

# B.

# PCATest package
pcatest <- PCAtest(spmo_scaled, 100, 100, 0.05, varcorr = FALSE, counter = FALSE, plot = TRUE)

# What is the difference between the two methods (broken-stick and PCAtest)
# Broken-stick would depend on eigenvalue vectors while PCAtest randomizes the dataset and runs PCA for every randomization.

# Broken-stick Barplot
broken_stick <- function(p){
  sapply(1:p, function(k){
    sum(1 / (k:p)) / p
  })
}

bs <- broken_stick(ncol(spmo_scaled))

comparison <- data.frame(
  PC = paste0("PC"),
  Observed = spmo_pervar,
  BrokenStick = bs
)

comparison

plot(
    spmo_pervar,
     type = "b",
     pch = 19,
    col = "black",
     ylim = c(0, max(spmo_pervar)),
     xlab = "Principal Component",
     ylab = "Proportion of Variance",
     main = "Broken-Stick Analysis"
)

lines(bs,
      type = "b",
      pch = 17,
      col = "pink1"
)

legend("topright",
       legend = c("Observed", "Broken Stick"),
       pch = c(19, 17),
       col = c("black", "pink1")
)