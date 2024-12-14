# Author: Tania Perdomo Flores

# Load necessary libraries
if (!requireNamespace("tidyverse", quietly = TRUE)) install.packages("tidyverse")
if (!requireNamespace("readxl", quietly = TRUE)) install.packages("readxl")
if (!requireNamespace("caret", quietly = TRUE)) install.packages("caret")
if (!requireNamespace("pROC", quietly = TRUE)) install.packages("pROC")

library("tidyverse")
library("readxl")
library("caret")
library("pROC")

# Load the dataset
data <- read.csv(choose.files())

# Check for missing values
print("Missing values per column:")
print(colSums(is.na(data)))

# Impute missing values (NAs) for numeric columns, except 'Insulin' as it will be handled differently
numeric_cols <- which(sapply(data, is.numeric))  # Identify numeric column indices
for (col in numeric_cols) {
  col_name <- names(data)[col]
  if (col_name != 'Insulin') {
    data[[col_name]][is.na(data[[col_name]])] <- mean(data[[col_name]], na.rm = TRUE)
  }
}

# Define clusters for 'Glucose' to impute 'Insulin' values based on these clusters
data$GlucoseCluster <- cut(data$Glucose, breaks = c(0, 100, 140, 200, Inf), 
                           labels = c("Low", "Moderate", "High", "Very High"))

# Calculate the mean 'Insulin' for each 'GlucoseCluster', ignoring zero values in 'Insulin'
cluster_means <- aggregate(Insulin ~ GlucoseCluster, data = data, 
                           function(x) mean(as.numeric(x[x != 0]), na.rm = TRUE))

# Replace missing or zero 'Insulin' values with the mean of the corresponding 'GlucoseCluster'
for (cluster in levels(data$GlucoseCluster)) {
  cluster_mean <- cluster_means$Insulin[cluster_means$GlucoseCluster == cluster]
  if (length(cluster_mean) > 0 && !is.na(cluster_mean)) {
    data$Insulin[data$GlucoseCluster == cluster & (is.na(data$Insulin) | data$Insulin == 0)] <- cluster_mean
  }
}

# Calculate IQR for each numeric column, including 'Insulin', excluding 'Outcome'
Q1 <- apply(data[, numeric_cols, drop = FALSE], 2, function(x) quantile(as.numeric(x), 0.25, na.rm = TRUE))
Q3 <- apply(data[, numeric_cols, drop = FALSE], 2, function(x) quantile(as.numeric(x), 0.75, na.rm = TRUE))
IQR <- Q3 - Q1

# Detect and replace outliers in each numeric column (excluding 'Outcome')
for (col in numeric_cols) {
  col_name <- names(data)[col]
  if (col_name != 'Outcome') {
    lower_bound <- Q1[col_name] - 1.5 * IQR[col_name]
    upper_bound <- Q3[col_name] + 1.5 * IQR[col_name]
    
    if (col_name == 'Insulin') {
      # Remove rows where 'Insulin' is zero or an outlier
      data <- data[!(data$Insulin == 0 | data$Insulin < lower_bound | data$Insulin > upper_bound), ]
    } else {
      # Replace outliers with the column mean
      outliers <- data[[col_name]] < lower_bound | data[[col_name]] > upper_bound
      data[[col_name]][outliers] <- mean(as.numeric(data[[col_name]]), na.rm = TRUE)
    }
  }
}

# Print the final dataset after processing
print("Dataset after processing missing values, clustering, outlier replacement, and removing zero/outlier Insulin rows:")
print(data)

# Logistic Regression
# Fit the model
logit_model <- glm(Outcome ~ ., data = data, family = binomial)

# Summary of the logistic regression model
summary(logit_model)

# Predict probabilities on the training set
data$predicted_prob <- predict(logit_model, type = "response")

# Convert probabilities to binary outcome (threshold 0.5)
data$predicted_class <- ifelse(data$predicted_prob > 0.5, 1, 0)

# Confusion matrix
conf_matrix <- confusionMatrix(factor(data$predicted_class), factor(data$Outcome))
print(conf_matrix)

# Calculate AUC
roc_curve <- roc(data$Outcome, data$predicted_prob)
auc_value <- auc(roc_curve)
print(paste("AUC:", auc_value))

# Plot the ROC Curve
plot(roc_curve, main = "ROC Curve")


            
