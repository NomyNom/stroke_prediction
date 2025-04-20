data <- read.csv(file.choose())


# Remove ID column
data$id <- NULL

# Remove rows with missing values
data <- na.omit(data)

# Convert categorical variables to factors
data$gender <- as.factor(data$gender)
data$ever_married <- as.factor(data$ever_married)
data$work_type <- as.factor(data$work_type)
data$Residence_type <- as.factor(data$Residence_type)
data$smoking_status <- as.factor(data$smoking_status)
data$stroke <- as.factor(data$stroke)
data$bmi <- as.numeric(data$bmi)

str(data) # view structure and types
summary(data) # get a quick data summary


# Logistic Regression Model
model <- glm(stroke ~ ., data = data, family = "binomial")
summary(model) 


data_clean <- na.omit(data)

# Fit logistic regression model
logistic_model <- glm(stroke ~ age + avg_glucose_level + hypertension + heart_disease,
                      data = data_clean,
                      family = "binomial")

# View summary
summary(logistic_model)

# Get predicted probabilities (between 0 and 1)
pred_probs <- predict(logistic_model, type = "response")

# Actual values (convert factor to numeric: 0 and 1)
actual <- as.numeric(as.character(data_clean$stroke))

# Calculate Mean Squared Error
mse <- mean((pred_probs - actual)^2)
mse






