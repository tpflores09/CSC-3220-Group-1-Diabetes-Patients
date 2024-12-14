# Classification tree written by Kristian Obrusanszki
# Load necessary libraries
if (!requireNamespace("mice", quietly = TRUE)) install.packages("mice")
if (!requireNamespace("caret", quietly = TRUE)) install.packages("caret")
if (!requireNamespace("corrplot", quietly = TRUE)) install.packages("corrplot")
if (!requireNamespace("ggplot2", quietly = TRUE)) install.packages("ggplot2")
if (!requireNamespace("tidyverse", quietly = TRUE)) install.packages("tidyverse")
if (!requireNamespace("tidymodels", quietly = TRUE)) install.packages("tidymodels")
if (!requireNamespace("rpart.plot", quietly = TRUE)) install.packages("rpart.plot")

library(rpart.plot)
library(tidyverse)
library(tidymodels)
library(mice)
library(caret)
library(corrplot)
library(ggplot2)

########################################################################## MICE DATA CLEANER
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

# Complete the dataset using the first imputed dataset
completed_data <- complete(imputed_data, 1)


################################################################################### Classification tree written by Kristian Obrusanszki

# Load necessary libraries
if (!requireNamespace("mice", quietly = TRUE)) install.packages("mice")
if (!requireNamespace("caret", quietly = TRUE)) install.packages("caret")
if (!requireNamespace("corrplot", quietly = TRUE)) install.packages("corrplot")
if (!requireNamespace("ggplot2", quietly = TRUE)) install.packages("ggplot2")
if (!requireNamespace("tidyverse", quietly = TRUE)) install.packages("tidyverse")
if (!requireNamespace("tidymodels", quietly = TRUE)) install.packages("tidymodels")
if (!requireNamespace("rpart.plot", quietly = TRUE)) install.packages("rpart.plot")

library(rpart.plot)
library(tidyverse)
library(tidymodels)
library(mice)
library(caret)
library(corrplot)
library(ggplot2)

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

# Complete the dataset using the first imputed dataset
completed_data <- complete(imputed_data, 1)

# Convert Outcome to factor
completed_data$Outcome <- factor(completed_data$Outcome, levels = c(0, 1))

# Split data into training and testing sets
set.seed(14092022)
data_split <- initial_split(completed_data, prop = 0.8)
trainData <- training(data_split)
testData <- testing(data_split)

# Define recipe
rec <- recipe(Outcome ~ Pregnancies + Glucose + BloodPressure + SkinThickness + Insulin + BMI + 
                DiabetesPedigreeFunction + Age, data = trainData)

# Classification decision tree
tree <- decision_tree(
  mode = "classification"
) %>% set_engine("rpart")

# Create a workflow and add recipe and model
tree_wflow <- workflow() |>
  add_recipe(rec) |>
  add_model(tree)

# Fit the model to the training data
fit_tree <- fit(tree_wflow, data = trainData)

# Print the fit object to inspect it
print(fit_tree)

# Generate predictions on the test set
predictions <- predict(fit_tree, new_data = testData, type = "prob") %>%
  bind_cols(testData)

# Convert predictions to binary class (threshold 0.5)
predictions <- predictions %>%
  mutate(Predicted = factor(ifelse(.pred_1 > 0.5, 1, 0), levels = c(0, 1)))

predictions <- predictions %>%
  mutate(.pred_1 = 1 - .pred_1)

# Ensure Outcome and Predicted columns are properly formatted
testData <- predictions %>%
  select(Predicted, Outcome)

# Check confusion matrix
conf_matrix <- confusionMatrix(testData$Predicted, testData$Outcome, mode = "everything")
print(conf_matrix)

# Calculate and plot the ROC curve
roc_data <- roc_curve(predictions, truth = Outcome, .pred_1)

autoplot(roc_data) +
  ggtitle("ROC Curve for Classification Tree Model") +
  theme_minimal()

# Plot the classification tree
rpart_model <- pull_workflow_fit(fit_tree)$fit
rpart.plot(rpart_model, type = 2, extra = 104, fallen.leaves = TRUE, main = "Classification Tree Diagram")
