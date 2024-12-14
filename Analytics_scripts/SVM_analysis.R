# Author: Fengjun Han
# Load necessary libraries
library(e1071)  # For SVM model
library(caret)  # For data partitioning and evaluation
library(tidyverse)
library(tidymodels)

# Load the dataset
data <- read.csv("diabetes.csv")

# Check for missing values
print(colSums(is.na(data)))

# Impute missing values for numeric columns (except 'Insulin')
numeric_cols <- sapply(data, is.numeric)
for (col in names(data)[numeric_cols]) {
  if (col != 'Insulin') {
    data[[col]][is.na(data[[col]])] <- mean(data[[col]], na.rm = TRUE)
  }
}

# IQR Cleaning for 'Insulin' column
Q1 <- quantile(data$Insulin, 0.25, na.rm = TRUE)
Q3 <- quantile(data$Insulin, 0.75, na.rm = TRUE)
IQR_Insulin <- Q3 - Q1

# Define lower and upper bounds for outlier detection
lower_bound <- Q1 - 1.5 * IQR_Insulin
upper_bound <- Q3 + 1.5 * IQR_Insulin

# Replace outliers in 'Insulin' with NA
data$Insulin[data$Insulin < lower_bound | data$Insulin > upper_bound] <- NA

# Define clusters for 'Glucose' to impute 'Insulin' values based on clusters
data$GlucoseCluster <- cut(data$Glucose, 
                           breaks = c(0, 100, 140, 200, Inf), 
                           labels = c("Low", "Moderate", "High", "Very High"))

# Calculate the mean 'Insulin' for each 'GlucoseCluster', ignoring zero values
cluster_means <- aggregate(Insulin ~ GlucoseCluster, 
                           data = data, 
                           function(x) mean(as.numeric(x[x != 0]), na.rm = TRUE))

# Replace missing or zero 'Insulin' values
for (i in 1:nrow(data)) {
  if (is.na(data$Insulin[i]) || data$Insulin[i] == 0) {
    cluster <- data$GlucoseCluster[i]
    data$Insulin[i] <- unique(cluster_means$Insulin[cluster_means$GlucoseCluster == cluster])
  }
}

# Convert 'Outcome' to a factor for classification
data$Outcome <- as.factor(data$Outcome)

# Split data into training and test sets
set.seed(123)  # For reproducibility
data_split <- initial_split(data, prop = 0.7)
train_data <- training(data_split)
test_data <- testing(data_split)

# Remove any potential NAs in test data
# testData <- na.omit(testData)

# Define recipe
data_recipe <- recipe(Outcome ~ ., data = train_data) |>
  # step_rm() removes the features listed. "nominal" is another word for "categorical".
  step_rm(all_nominal_predictors()) |> 
  # Normalize the numerical features because SVM depends on distance
  step_normalize(all_numeric_predictors()) 

# Define model
linearSVM <- svm_linear(mode = "classification", 
  cost = 0.01, engine = "kernlab")

# Assemble workflow
data_workflow <- workflow() |>
  add_recipe(data_recipe) |>
  add_model(linearSVM)

# Fit the linear model
linearSVMFit <- fit(data_workflow, train_data)
linearSVMFit

# Predict values for the test set and add those values onto to the test set.
testPred <- augment(linearSVMFit, test_data) %>%
  select(Outcome, .pred_class, .pred_0, .pred_1)

# Evaluate the model
conf_matrix <- conf_mat(testPred, Outcome, .pred_class)

# Print accuracy and confusion matrix
testPred |> accuracy(Outcome, .pred_class)
testPred |> conf_mat(Outcome, .pred_class)
testPred |>
  conf_mat(Outcome, .pred_class) |>
  autoplot(type = "heatmap")

# Train the SVM model
svm_model <- svm(Outcome ~ ., data = trainData, kernel = "linear", cost = 1, scale = TRUE)

# Make predictions on the test set
predictions <- predict(svm_model, testData)

# Evaluate the model
conf_matrix <- confusionMatrix(predictions, testData$Outcome)
print(conf_matrix)

# Print model summary
print(summary(svm_model))
