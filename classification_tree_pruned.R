library(tree)
library(ROSE)   # for oversampling

# --- Preprocess Data ---
data <- healthcare.dataset.stroke.data

# Drop the 'id' column
data$id <- NULL

# Ensure 'bmi' is numeric and impute missing values with the median
data$bmi <- as.numeric(data$bmi)
data$bmi[is.na(data$bmi)] <- median(data$bmi, na.rm = TRUE)

# Convert categorical variables to factor
categorical_cols <- c("gender", "ever_married", "work_type", "Residence_type", "smoking_status", "stroke")
data[categorical_cols] <- lapply(data[categorical_cols], as.factor)

test_errors <- c()

set.seed(42) 

for (i in 1:10) {
  # 80/20 split
  sample_idx <- sample(1:nrow(data), 0.8 * nrow(data))
  train <- data[sample_idx, ]
  test <- data[-sample_idx, ]
  
  # Oversample minority class in train set
  balanced_train <- ovun.sample(stroke ~ ., data = train, method = "over", 
                                N = 2 * table(train$stroke)[1])$data
  
  tree.model <- tree(stroke ~ ., data = balanced_train)
  
  # Cross-validation for pruning
  cv_model <- cv.tree(tree.model, FUN = prune.misclass)
  
  # Get optimal size (smallest deviance)
  best_size <- cv_model$size[which.min(cv_model$dev)]
  
  # Prune the tree
  pruned_tree <- prune.tree(tree.model, best = best_size)
  
  preds <- predict(tree.model, newdata = test, type = "class")
  
  # Misclassification error
  error <- mean(preds != test$stroke)
  test_errors <- c(test_errors, error)
}

plot(pruned_tree)
text(pruned_tree, pretty = 0)

mean_test_error <- mean(test_errors)
cat("Mean Test Prediction Error over 10 iterations:", round(mean_test_error, 4), "\n")



