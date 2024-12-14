# Author: Thomas D. Robertson II
# Load necessary libraries
if (!requireNamespace("tidyverse", quietly = TRUE)) install.packages("tidyverse")
if (!requireNamespace("readxl", quietly = TRUE)) install.packages("readxl")
if (!requireNamespace("caret", quietly = TRUE)) install.packages("caret")
if (!requireNamespace("pROC", quietly = TRUE)) install.packages("pROC")
if (!requireNamespace("mice", quietly = TRUE)) install.packages("mice")
if (!requireNamespace("caret", quietly = TRUE)) install.packages("caret")
if (!requireNamespace("corrplot", quietly = TRUE)) install.packages("corrplot")
if (!requireNamespace("ggplot2", quietly = TRUE)) install.packages("ggplot2")

library("tidyverse")
library("readxl")
library("caret")
library("pROC")
library("mice")
library("caret")
library("corrplot")
library("ggplot2")

# Load the dataset
data <- read.csv(choose.files(), stringsAsFactors = FALSE)

# Replace zeros with NA for specific columns
columns_with_zeros <- c("Glucose", "BloodPressure", "SkinThickness", "Insulin", "BMI")
data[columns_with_zeros] <- lapply(data[columns_with_zeros], function(x) ifelse(x == 0, NA, x))

# Perform multiple imputation using MICE
set.seed(123)  # Ensures reproducibility
imputed_data <- mice(data, method = 'pmm', m = 5, maxit = 50, seed = 500)

# Check imputation summary
summary(imputed_data)

# Complete the dataset using combination of imputed datasets
completed_data <- complete(imputed_data)

# Validate imputations with visualizations
# Before and after comparison for Glucose
ggplot(data, aes(x = Glucose)) + 
  geom_histogram(fill = 'blue', alpha = 0.5, bins = 20) + 
  ggtitle("Glucose Distribution Before Imputation")

ggplot(completed_data, aes(x = Glucose)) + 
  geom_histogram(fill = 'green', alpha = 0.5, bins = 20) + 
  ggtitle("Glucose Distribution After Imputation")

# Check correlation matrix to measure relationships with values shown
corrplot(cor(completed_data, use = "complete.obs"), method = "number")

# Split data into training and test sets
set.seed(123)
trainIndex <- createDataPartition(completed_data$Outcome, p = 0.8, list = FALSE)
trainData <- completed_data[trainIndex, ]
testData <- completed_data[-trainIndex, ]

# Train a logistic regression model
model <- glm(Outcome ~ ., data = trainData, family = binomial)

# Make predictions on test data
predictions <- predict(model, testData, type = "response")

# Evaluate model performance using ROC curve
roc_obj <- roc(testData$Outcome, predictions)
plot(roc_obj, main = "ROC Curve for Diabetes Prediction")
auc(roc_obj)  # Calculate AUC

# Calculate confusion matrix
threshold <- 0.5
predicted_classes <- ifelse(predictions > threshold, 1, 0)
confusionMatrix(table(predicted_classes, testData$Outcome))

# Visualize ROC curve with AUC value
ggplot(data.frame(tpr = roc_obj$sensitivities, fpr = 1 - roc_obj$specificities), aes(x = fpr, y = tpr)) +
  geom_line() +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed") +
  ggtitle("ROC Curve with AUC = 0.85") +
  xlab("False Positive Rate") +
  ylab("True Positive Rate") +
  theme_minimal() +
  theme(legend.position = "none") +
  coord_fixed() +
  xlim(0, 1) +
  ylim(0, 1)

# Save the model for future use
# saveRDS(model, "diabetes_prediction_model.rds")
