library(ROSE)
library(tree)

# Handle missing values
data <- healthcare.dataset.stroke.data
data$id <- NULL
data$bmi[is.na(data$bmi)] <- median(data$bmi, na.rm = TRUE)

# Convert categoricals to factors
categorical_cols <- c("gender", "work_type", "Residence_type", "smoking_status", "stroke")
data[categorical_cols] <- lapply(data[categorical_cols], as.factor)

# Split into train/test
set.seed(42)
sample <- sample(1:nrow(data), 0.7 * nrow(data))
train <- data[sample, ]
test <- data[-sample, ]

# Balance the training set using ROSE
train_bal <- ROSE(stroke ~ ., data = train, seed = 1)$data

# Check class balance
table(train_bal$stroke)


# Fit the decision tree model
tree.model.bal <- tree(stroke ~ ., data = train_bal)

# Plot the tree
plot(tree.model.bal)
text(tree.model.bal, pretty = 0)

# Summary
summary(tree.model.bal)

# Predict on original test set
pred <- predict(tree.model.bal, test, type = "class")

# Confusion matrix
cm <- table(Predicted = pred, Actual = test$stroke)
print(cm)

# Accuracy, Recall 
accuracy <- sum(diag(cm)) / sum(cm)
recall <- cm["1", "1"] / sum(cm[ , "1"])  # Sensitivity for stroke
cat("Accuracy:", accuracy, "\n")
cat("Recall for stroke:", recall, "\n")





# Prune the tree

# Cross-validate to find optimal size
set.seed(42)
cv_results <- cv.tree(tree.model.bal, FUN = prune.misclass)

# Plot cross-validation results
plot(cv_results$size, cv_results$dev, type = "b",
     xlab = "Tree Size (Number of Terminal Nodes)",
     ylab = "Deviance (Misclassification Error)",
     main = "Cross-Validation for Pruning")

# Pick best size
best_size <- cv_results$size[which.min(cv_results$dev)]
cat("Optimal tree size:", best_size, "\n")

# Prune the tree
pruned_tree <- prune.tree(tree.model.bal, best = best_size)

# Plot the pruned tree
plot(pruned_tree)
text(pruned_tree, pretty = 0)


# Predict on test set
pred_pruned <- predict(pruned_tree, test, type = "class")

# Confusion matrix
cm_pruned <- table(Predicted = pred_pruned, Actual = test$stroke)
print(cm_pruned)

# Accuracy and recall
accuracy_pruned <- sum(diag(cm_pruned)) / sum(cm_pruned)
recall_pruned <- ifelse("1" %in% colnames(cm_pruned), cm_pruned["1", "1"] / sum(cm_pruned[ , "1"]), 0)

cat("Pruned Tree Accuracy:", round(accuracy_pruned, 4), "\n")
cat("Pruned Tree Recall:", round(recall_pruned, 4), "\n")