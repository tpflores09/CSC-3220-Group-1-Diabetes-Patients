#Authors: Logan Bolton, Jun Han


# Load the dataset
data <- read.csv(choose.files())

# Check for missing values
colSums(is.na(data))

# Impute missing values (NAs) for numeric columns, except 'Insulin' as it will be handled differently
numeric_cols <- sapply(data, is.numeric)  # Identify numeric columns
for (col in names(data)[numeric_cols]) {
  if (col != 'Insulin') {
    data[[col]][is.na(data[[col]])] <- mean(data[[col]], na.rm = TRUE)
  }
}

# Define clusters for 'Glucose' to impute 'Insulin' values based on these clusters
# Adjust breaks based on the distribution of Glucose
data$GlucoseCluster <- cut(data$Glucose, breaks = c(0, 100, 140, 200, Inf), 
                           labels = c("Low", "Moderate", "High", "Very High"))

# Calculate the mean 'Insulin' for each 'GlucoseCluster', ignoring zero values in 'Insulin'
cluster_means <- aggregate(Insulin ~ GlucoseCluster, data = data, function(x) mean(as.numeric(x[x != 0]), na.rm = TRUE))

# Replace missing or zero 'Insulin' values with the mean of the corresponding 'GlucoseCluster'
for (cluster in levels(data$GlucoseCluster)) {
  # Find the mean Insulin for the current cluster
  cluster_mean <- cluster_means$Insulin[cluster_means$GlucoseCluster == cluster]
  
  # Check if cluster_mean exists and has a valid value before proceeding
  if (length(cluster_mean) > 0 && !is.na(cluster_mean)) {
    data$Insulin[data$GlucoseCluster == cluster & (is.na(data$Insulin) | data$Insulin == 0)] <- cluster_mean
  }
}

# Calculate IQR for each numeric column, including 'Insulin', excluding 'Outcome'
Q1 <- apply(data[, numeric_cols], 2, function(x) quantile(as.numeric(x), 0.25, na.rm = TRUE))
Q3 <- apply(data[, numeric_cols], 2, function(x) quantile(as.numeric(x), 0.75, na.rm = TRUE))
IQR <- Q3 - Q1

# Detect and replace outliers in each numeric column (excluding 'Outcome')
for (col in names(data)[numeric_cols]) {
  if (col != 'Outcome') {
    # Define outlier thresholds
    lower_bound <- Q1[col] - 1.5 * IQR[col]
    upper_bound <- Q3[col] + 1.5 * IQR[col]
    
    # Identify outliers for each column
    if (col == 'Insulin') {
      # Filter out rows where 'Insulin' is zero or an outlier
      data <- data[!(data$Insulin == 0 | data$Insulin < lower_bound | data$Insulin > upper_bound), ]
    } else {
      # Replace outliers with the column mean
      outliers <- data[[col]] < lower_bound | data[[col]] > upper_bound
      data[[col]][outliers] <- mean(as.numeric(data[[col]]), na.rm = TRUE)
    }
  }
}

# Print the final dataset after processing
print("Dataset after processing missing values, clustering, outlier replacement, and removing zero/outlier Insulin rows:")
print(data)

