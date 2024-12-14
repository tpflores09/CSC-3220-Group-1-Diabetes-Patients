#Author: Logan Bolton

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

# Correctly construct selected_data
selected_data <- as.matrix(cbind(data$Glucose, data$Insulin))

k <- 3
max_iter <- 100
n <- nrow(selected_data)
centroids <- selected_data[sample(1:n, k), , drop = FALSE]

euclidean_distance <- function(a, b) {
  sqrt(sum((a - b)^2))
}

for (iter in 1:max_iter) {
  # Assignment step
  clusters <- sapply(1:n, function(j) {
    distances <- apply(centroids, 1, function(centroid) {
      sqrt(sum((selected_data[j, ] - centroid)^2))
    })
    which.min(distances)
  })
  
  # Update step
  new_centroids_list <- lapply(1:k, function(cluster) {
    members <- selected_data[clusters == cluster, , drop = FALSE]
    if (nrow(members) > 0) {
      colMeans(members)
    } else {
      # Reinitialize centroid to a random data point
      selected_data[sample(1:n, 1), ]
    }
  })
  
  # Combine list into a matrix
  new_centroids <- do.call(rbind, new_centroids_list)
  
  # Ensure centroids and new_centroids have the same dimensions
  if (!all(dim(centroids) == dim(new_centroids))) {
    stop("Dimensions of centroids do not match.")
  }
  
  # Check for convergence
  if (all(centroids == new_centroids, na.rm = TRUE)) {
    cat("Converged at iteration:", iter, "\n")
    break
  }
  
  centroids <- new_centroids
}

# Plotting the clusters
plot(selected_data[,1], selected_data[,2], 
     col = clusters, pch = 16, 
     xlab = "Glucose", ylab = "Insulin", 
     main = "Clusters Based on Glucose and Insulin Levels")
points(centroids[,1], centroids[,2], col = 1:k, pch = 8, cex = 2)

# Analyzing cluster characteristics
clustered_data <- data.frame(selected_data, cluster = clusters)
colnames(clustered_data)[1:2] <- c("Glucose", "Insulin")  # Name the columns
cluster_means <- aggregate(. ~ cluster, data = clustered_data, FUN = mean)
print(cluster_means)

# 
