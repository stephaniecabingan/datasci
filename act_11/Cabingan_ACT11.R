# Cabingan
# Act 11

# Read packages
library(terra)
library(dplyr)

# Read species data
df <- read.csv("herpetozoa-033-095-s001.csv")

# Clean data
good <- complete.cases(df)
head(df[good, ])
herp_data <- df[good, ]

# A.SLR

# Read elevation data
elevation <- rast("ph_strm_geo.tif")

#Subset longitude and latitude data
longlat_data <- herp_data[2:3]
longlat_data

# Convert coordinates to spatial vector
longlat_vect <- vect(longlat_data)
longlat_vect

# Extract elevation data for each point
longlat_ext <- terra::extract(elevation, longlat_vect)
longlat_ext

# Subset species data to only have SPPTotal
spp_data <- select(herp_data, SPPTotal)
spp_data

# Combine species richness data with extracted elevation data + rename Band_1 to elevation
merged_data <- cbind(spp_data, longlat_ext$Band_1)
colnames(merged_data) <- c("SPPTotal", "Elevation")
merged_data <- as.data.frame(merged_data)
merged_data

## Simple Linear Regression
x <- merged_data$Elevation
y  <- merged_data$SPPTotal
x
y

# Mean of values
xbar <- mean(x)
ybar <- mean(y)
xbar
ybar

# Calculate the slope
b <- sum((x - xbar) * (y - ybar)) / sum((x - xbar)^2)
b

# Calculate the intercept
a <- ybar - b * xbar
a

# Write the equation
cat("Regression Equation:\n")
cat("y =", round(a,3), "+", round(b,3), "x\n")

pdf("Cabingan_linear_reg.pdf", width = 20, height = 8)

# Calculate the predicted arm length
yhat <- a + b * x

data.frame(
  Height = x,
  ArmLength = y,
  Predicted = round(yhat,2)
)

# Plot data
plot(x, y,
     pch = 19,
     col = "red4",
     xlab = "Elevation (m)",
     ylab = "Species Richness",
     main = "Simple Linear Regression")

## Add fitted regression line
abline(a, b,
       col = "pink3",
       lwd = 2)

## Add predicted values
points(x, yhat,
       pch = 17,      # Triangle
       col = "purple4",
       cex = 1.2)

## Draw residual lines
segments(x0 = x, y0 = y,
         x1 = x, y1 = yhat,
         lty = 2,
         col = "gray50")

## Add legend
legend("topright",
       legend = c("Observed", "Predicted", "Regression Line"),
       pch = c(19, 17, NA),
       lty = c(NA, NA, 1),
       col = c("red4", "purple4", "pink3"),
       lwd = c(NA, NA, 2),
       bty = "n")

dev.off()

## B. MLR

# Read precipitation and temperature data
prec <- rast("annual_precipitation.tif")
temp <- rast("annual_temperature.tif")

# Extract data for each point
precd <- terra::extract(prec, longlat_vect)
tempd <- terra::extract(temp, longlat_vect)
precd
tempd

# Combine data
prectemp <- cbind(spp_data, longlat_ext$Band_1, precd$annual_precipitation, tempd$annual_temperature)
colnames(prectemp) <- c("SPPTotal", "Elevation", "Annual Precipitation", "Annual Temperature")
prectemp <- as.data.frame(prectemp)
prectemp

# MLR using lm() function
x <- prectemp$Elevation
x1 <- prectemp$'Annual Precipitation'
x2 <- prectemp$'Annual Temperature'
y <- prectemp$SPPTotal
model <- lm(y ~ x + x1 + x2 + x2)

# Display model summary
summary(model)

# Results
coef(model)
a <- coef(model)[1]
b <- coef(model)[2]
c <- coef(model)[3]
d <- coef(model)[4]

cat("Regression Equation:\n")
cat("y =", round(a,3), "+", round(b,3), "x", "+", round(c,3), "x1", "+", round(d,3), "x2\n\n")

# Summary of Results
yhat <- predict(model)
residuals <- residuals(model)
results <- data.frame(
  Elevation = x,
  "Annual Precipitation" = x1,
  "Annual Temperature" = x2,
  Observed = y,
  Predicted = round(yhat,2),
  Residual = round(residuals,2)
)

print(results)