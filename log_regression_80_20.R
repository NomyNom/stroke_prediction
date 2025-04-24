library(ROSE)      # for oversampling

# --- Preprocess Data ---
data <- healthcare.dataset.stroke.data

data$id <- NULL
data <- na.omit(data)
data$gender <- factor(data$gender)
data$ever_married <- factor(data$ever_married)
data$work_type <- factor(data$work_type)
data$Residence_type <- factor(data$Residence_type)
data$smoking_status <- factor(data$smoking_status)
data$stroke <- factor(data$stroke, levels=c("0","1"))
data$bmi <- as.numeric(data$bmi)

set.seed(42)
n <- nrow(data)
splits <- lapply(1:10, function(i) sample(n, size=floor(0.8*n)))

log_mse <- numeric(10)
accuracy <- numeric(10)

for(i in seq_along(splits)) {
  train <- data[splits[[i]],]
  test  <- data[-splits[[i]],]
  balanced_train <- ovun.sample(stroke~., data=train, method="over", seed=42)$data
  model <- glm(stroke~age+avg_glucose_level+hypertension+heart_disease,
               data=balanced_train, family=binomial)
  probs <- predict(model, newdata=test, type="response")
  actual <- as.numeric(as.character(test$stroke))
  log_mse[i] <- mean((probs-actual)^2)
  preds <- as.numeric(probs>0.5)
  accuracy[i] <- mean(preds==actual)
}
summary(model)
cat("Mean logistic MSE over 10 splits:", round(mean(log_mse), 5), "\n")
cat("Mean logistic accuracy over 10 splits:", round(mean(accuracy), 5), "\n")

