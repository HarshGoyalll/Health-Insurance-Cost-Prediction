# Load the necessary libraries
library(randomForest)

# Set the working directory to where the file is located
setwd("/Users/apple/Desktop/HealthInsuranceAmountPrediction-main/Dataset/")

# Load the dataset
insurance_data <- read.csv("insurance.csv")

# Assuming your dataset has the columns 'age', 'sex', 'bmi', 'children', 'smoker', 'region', and 'expenses', you can proceed with the random forest model as shown in the previous example.

# Fit the random forest model
model <- randomForest(expenses ~ age + sex + bmi + children + smoker + region, data = insurance_data)

# Make predictions on the training data
predictions <- predict(model, newdata = insurance_data)

# Calculate Mean Squared Error
mse <- mean((predictions - insurance_data$expenses)^2)
print(paste("Mean Squared Error: ", mse))

# Calculate accuracy as a percentage
accuracy <- 100 * (1 - mse / var(insurance_data$expenses))
print(paste("Accuracy: ", accuracy, "%"))
