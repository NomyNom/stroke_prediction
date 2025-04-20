library(ROSE)      # for oversampling
library(caret)     # for confusion matrix, accuracy
library(dplyr)

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

logreg_errors <- c()

set.seed(42)
for (i in 1:10) {
  # 80/20 split
  sample_idx <- sample(1:nrow(data), 0.8 * nrow(data))
  train <- data[sample_idx, ]
  test <- data[-sample_idx, ]
  
  # Oversample minority class in training set
  balanced_train <- ovun.sample(stroke ~ ., data = train, method = "over", 
                                N = 2 * table(train$stroke)[1])$data
  
  # Logistic Regression
  logreg_model <- glm(stroke ~ ., data = balanced_train, family = binomial)
  
  # Predict probabilities
  probs <- predict(logreg_model, newdata = test, type = "response")
  
  # Convert to class predictions (threshold = 0.5)
  preds <- ifelse(probs > 0.5, 1, 0)
  
  # Misclassification error
  error <- mean(preds != as.numeric(as.character(test$stroke)))
  logreg_errors <- c(logreg_errors, error)
}

# Report average test prediction error
mean_logreg_error <- mean(logreg_errors)
cat("Mean Test Prediction Error (Logistic Regression) over 10 iterations:", round(mean_logreg_error, 4), "\n")
summary(logreg_model)
