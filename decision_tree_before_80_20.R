library(ROSE)
library(tree)

data <- healthcare.dataset.stroke.data
data$id <- NULL
data$bmi[is.na(data$bmi)] <- median(data$bmi, na.rm = TRUE)

# Convert categoricals to factors
categorical_cols <- c("gender", "work_type", "Residence_type", "smoking_status", "stroke")
data[categorical_cols] <- lapply(data[categorical_cols], as.factor)


set.seed(42)
data_bal <- ROSE(stroke ~ ., data = data, seed = 1)$data

# Check balance
cat("Balanced class distribution:\n")
print(table(data_bal$stroke))

# decision tree
tree.model <- tree(stroke ~ ., data = data_bal)

# Plot the tree
plot(tree.model)
text(tree.model, pretty = 0)

# Tree summary
summary(tree.model)


# Prune the tree

set.seed(42)
cv_results <- cv.tree(tree.model, FUN = prune.misclass)

# Plot CV deviance vs tree size
plot(cv_results$size, cv_results$dev, type = "b",
     xlab = "Tree Size (Number of Terminal Nodes)",
     ylab = "Deviance (Misclassification Error)",
     main = "Cross-Validation for Pruning")

# Get best size
best_size <- cv_results$size[which.min(cv_results$dev)]
cat("Optimal tree size:", best_size, "\n")

# Prune the tree
pruned_tree <- prune.tree(tree.model, best = best_size)

# Plot pruned tree
plot(pruned_tree)
text(pruned_tree, pretty = 0)


# Predict on full balanced data
pred <- predict(pruned_tree, data_bal, type = "class")

# Confusion matrix
cm <- table(Predicted = pred, Actual = data_bal$stroke)
print(cm)

# Accuracy & recall
accuracy <- sum(diag(cm)) / sum(cm)
recall <- ifelse("1" %in% colnames(cm), cm["1", "1"] / sum(cm[ , "1"]), 0)

cat("Pruned Tree Accuracy (on full data):", round(accuracy, 4), "\n")
cat("Pruned Tree Recall (on full data):", round(recall, 4), "\n")

