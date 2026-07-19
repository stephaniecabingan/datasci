# Cabingan
# Act 13

# Read packages
library(ranger)

# Read data
df <- read.csv("call_data_12October2025.csv")
acoustic <- df[,-1]
acoustic$Species <- as.factor(acoustic$Species)

# Design cartesian grid
hyper_grid <- expand.grid(
  num.trees = c(100, 300, 500),
  mtry = c(2, 4, 6),
  min.node.size = c(1, 5, 10)
)
oob_errors <- numeric(nrow(hyper_grid))

cat("Starting grid search across", nrow(hyper_grid), "combinations...\n")

for (i in 1:nrow(hyper_grid)) {
  model_iteration <- ranger(
    formula        = Species ~ ., 
    data           = acoustic, 
    num.trees      = hyper_grid$num.trees[i],
    mtry           = hyper_grid$mtry[i],
    min.node.size  = hyper_grid$min.node.size[i],
    seed           = 123,                 
    num.threads    = NULL                 
  )
  
  oob_errors[i] <- model_iteration$prediction.error
}

grid_results <- cbind(hyper_grid, OOB_Error = oob_errors)

# Selecting optimal hyperparameters
grid_sorted <- grid_results[order(grid_results$OOB_Error), ]
best_combination <- grid_sorted[1, ]
cat("--- OPTIMAL HYPERPARAMETERS FOUND ---\n")
print(best_combination)

# Best Model
cat("\nTraining the final model using the absolute best parameters...\n")

final_acoustic_model <- ranger(
  formula        = Species ~ .,
  data           = acoustic,
  num.trees      = best_combination$num.trees,
  mtry           = best_combination$mtry,
  min.node.size  = best_combination$min.node.size,
  importance     = "impurity", # Enables feature importance tracking
  seed           = 123
)

print(final_acoustic_model)

# Plot
single_tree <- rpart(
  formula = Species ~ ., 
  data    = acoustic, 
  method  = "class",                        
  control = rpart.control(minsplit = best_combination$min.node.size) 
)

pdf("Cabingan_randforest_reg.pdf", width = 11, height = 8.5)

rpart.plot(
  single_tree, 
  type        = 5,                          
  shadow.col  = "gray",                     
  nn          = TRUE,                       
  main        = "Frog Species Species Decision Tree Layout"
)

dev.off()