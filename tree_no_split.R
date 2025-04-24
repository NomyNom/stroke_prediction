library(ROSE)

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

library(tree)

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

