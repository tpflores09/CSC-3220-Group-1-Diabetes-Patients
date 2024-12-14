Authors: Jun Han, Thomas Robertson.

This is a documentation on our processes while creating the Support Vector Machine Model.
```

# Load necessary libraries
library(e1071)  # For SVM model
library(caret)  # For data partitioning and evaluation

```
One mistake I made was forgetting to load those libraries above. It is absolutely crucial to making the SVM work properly.

```

# Load the dataset
data <- complete_data

# Check for missing values
print(colSums(is.na(data)))

# Impute missing values for numeric columns (except 'Insulin')
numeric_cols <- sapply(data, is.numeric)
for (col in names(data)[numeric_cols]) {
  if (col != 'Insulin') {
    data[[col]][is.na(data[[col]])] <- mean(data[[col]], na.rm = TRUE)
  }
}

```
Imputation needs to happen before cleaning outliers since missing values can mask outliers, missing values can effect those metrics that are used to calculate outliers. This was another challenge that we encountered during the coding of the SVM.
I did not think this mattered a lot at first, but once I realized that this is preventing the model from fitting, I understood the severity of it.

```

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
trainIndex <- createDataPartition(data$Outcome, p = 0.8, list = FALSE)
trainData <- data[trainIndex, ]
testData <- data[-trainIndex, ]

# Remove any potential NAs in test data
testData <- na.omit(testData)

# Train the SVM model
svm_model <- svm(Outcome ~ ., data = trainData, kernel = "linear", cost = 1, scale = TRUE)

# Make predictions on the test set
predictions <- predict(svm_model, testData)

# Evaluate the model
conf_matrix <- confusionMatrix(predictions, testData$Outcome)
print(conf_matrix)

# Print model summary
print(summary(svm_model))

```
At first I used a library called Kernlabs, it is actually not as versatile as Caret. There were issues with fitting the model, those problems were solved once Thomas switched to Caret and e1071.
