# Author: Thomas D. Robertson II
# Load necessary libraries
if (!requireNamespace("tidyverse", quietly = TRUE)) install.packages("tidyverse")
if (!requireNamespace("mice", quietly = TRUE)) install.packages("mice")
if (!requireNamespace("caret", quietly = TRUE)) install.packages("caret")
if (!requireNamespace("corrplot", quietly = TRUE)) install.packages("corrplot")
if (!requireNamespace("ggplot2", quietly = TRUE)) install.packages("ggplot2")
if (!requireNamespace("glmnet", quietly = TRUE)) install.packages("glmnet")
if (!requireNamespace("themis", quietly = TRUE)) install.packages("themis")
if (!requireNamespace("pROC", quietly = TRUE)) install.packages("pROC")

library(tidyverse)
library(mice)
library(caret)
library(corrplot)
library(ggplot2)
library(glmnet)
library(themis)
library(pROC)

# Load the dataset
data <- read.csv(choose.files(), stringsAsFactors = FALSE)

# Replace zeros with NA for specific columns
columns_with_zeros <- c("Glucose", "BloodPressure", "SkinThickness", "Insulin", "BMI")
data[columns_with_zeros] <- lapply(data[columns_with_zeros], function(x) ifelse(x == 0, NA, x))

# Detect and count outliers using IQR method
detect_outliers <- function(column) {
  Q1 <- quantile(column, 0.25, na.rm = TRUE)
  Q3 <- quantile(column, 0.75, na.rm = TRUE)
  IQR_value <- Q3 - Q1
  lower_bound <- Q1 - 1.5 * IQR_value
  upper_bound <- Q3 + 1.5 * IQR_value
  sum(column < lower_bound | column > upper_bound, na.rm = TRUE) # Count of outliers
}

# Count outliers for each numeric column
outlier_counts <- sapply(data[columns_with_zeros], detect_outliers)
outlier_counts_df <- data.frame(Column = names(outlier_counts), Outliers = outlier_counts)
print(outlier_counts_df)

# Visualize the outlier counts
ggplot(outlier_counts_df, aes(x = Column, y = Outliers, fill = Column)) +
  geom_bar(stat = "identity") +
  labs(
    title = "Number of Outliers Per Column",
    x = "Column",
    y = "Outliers"
  ) +
  theme_minimal()

# Boxplots for visualization
boxplot_data <- data %>%
  select(all_of(columns_with_zeros)) %>%
  pivot_longer(cols = everything(), names_to = "Column", values_to = "Value")

ggplot(boxplot_data, aes(x = Column, y = Value, fill = Column)) +
  geom_boxplot(outlier.color = "red") +
  labs(
    title = "Boxplots of Numeric Columns with Outliers Highlighted",
    x = "Column",
    y = "Value"
  ) +
  theme_minimal()

# Histograms for visualization
ggplot(boxplot_data, aes(x = Value, fill = Column)) +
  geom_histogram(binwidth = 5, alpha = 0.7) +
  facet_wrap(~Column, scales = "free") +
  labs(
    title = "Histograms of Numeric Columns",
    x = "Value",
    y = "Frequency"
  ) +
  theme_minimal()

# Perform multiple imputation using MICE
set.seed(123)
imputed_data <- mice(data, method = 'pmm', m = 5, maxit = 50, seed = 500)
completed_data <- complete(imputed_data, 1)

# Validate imputations with visualizations
ggplot(data, aes(x = Glucose)) + 
  geom_histogram(fill = 'blue', alpha = 0.5, bins = 20) + 
  ggtitle("Glucose Distribution Before Imputation")

ggplot(completed_data, aes(x = Glucose)) + 
  geom_histogram(fill = 'green', alpha = 0.5, bins = 20) + 
  ggtitle("Glucose Distribution After Imputation")

# Check correlation matrix to ensure realistic relationships
corrplot(cor(completed_data, use = "complete.obs"), method = "number")

# Split data into training and test sets
set.seed(123)
trainIndex <- createDataPartition(completed_data$Outcome, p = 0.8, list = FALSE)
trainData <- completed_data[trainIndex, ]
testData <- completed_data[-trainIndex, ]

# Ensure the Outcome column is a factor for classification
trainData$Outcome <- as.factor(trainData$Outcome)
testData$Outcome <- as.factor(testData$Outcome)

# Apply SMOTE using themis
set.seed(123)
smote_recipe <- recipe(Outcome ~ ., data = trainData) %>%
  step_smote(Outcome, over_ratio = 1) %>%
  prep()

balanced_trainData <- bake(smote_recipe, new_data = NULL)

# Feature Engineering: Add Interaction and Polynomial Features
balanced_trainData$Glucose_BMI <- balanced_trainData$Glucose * balanced_trainData$BMI
testData$Glucose_BMI <- testData$Glucose * testData$BMI

balanced_trainData$Glucose2 <- balanced_trainData$Glucose^2
testData$Glucose2 <- testData$Glucose^2

# Fit a logistic regression model with regularization (Lasso)
x <- model.matrix(Outcome ~ ., balanced_trainData)[, -1]  # Exclude intercept
y <- balanced_trainData$Outcome

lasso_model <- cv.glmnet(x, as.numeric(as.character(y)), alpha = 1, family = "binomial")
best_lambda <- lasso_model$lambda.min
coef(lasso_model, s = best_lambda)

# Predict using the lasso-regularized model
x_test <- model.matrix(Outcome ~ ., testData)[, -1]
predictions <- predict(lasso_model, newx = x_test, s = best_lambda, type = "response")

# Store predicted probabilities and binary classifications
testData$predicted_prob <- as.numeric(predictions)
testData$Predicted <- ifelse(predictions > 0.5, 1, 0)

# Evaluate model performance
conf_matrix <- confusionMatrix(as.factor(testData$Predicted), as.factor(testData$Outcome), mode = "everything")
print(conf_matrix)

# Calculate ROC curve and AUC
roc_curve <- roc(testData$Outcome, testData$predicted_prob)
auc_value <- auc(roc_curve)
print(paste("AUC:", auc_value))

# Plot the ROC Curve
plot(roc_curve, main = "ROC Curve for Lasso-regularized Logistic Regression", col = "blue", lwd = 2)


# Save the cleaned and engineered dataset
# write.csv(completed_data, "cleaned_diabetes.csv", row.names = FALSE)
