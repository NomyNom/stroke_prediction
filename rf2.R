library(randomForest)
library(caret)
library(ROSE)

# Clean data

data$id <- NULL                     
data <- na.omit(data)               

# Convert categorical variables
data$gender <- as.factor(data$gender)
data$work_type <- as.factor(data$work_type)
data$Residence_type <- as.factor(data$Residence_type)
data$smoking_status <- as.factor(data$smoking_status)
data$stroke <- as.factor(data$stroke)
data$bmi <- as.numeric(data$bmi)

# Remove unimportant variable
data$ever_married <- NULL

# Balance data using ROSE

set.seed(123)
data_balanced <- ROSE(stroke ~ ., data = data, seed = 1)$data

# Check class balance
print(table(data_balanced$stroke))


# Build Random Forest Model

rf.model <- randomForest(stroke ~ ., data = data_balanced, importance = TRUE)

# Print model
print(rf.model)

# Variable importance plot
varImpPlot(rf.model)


# Predictions and Evaluation
# Predict on balanced training data
rf.pred <- predict(rf.model, type = "class")
rf.probs <- predict(rf.model, type = "prob")[, 2]

# Confusion Matrix
conf_matrix <- confusionMatrix(rf.pred, data_balanced$stroke)
print(conf_matrix)

# Mean Squared Error (MSE)
actual <- as.numeric(as.character(data_balanced$stroke))  # convert factor to 0/1
rf.mse <- mean((rf.probs - actual)^2)
print(paste("Random Forest MSE:", round(rf.mse, 5)))
