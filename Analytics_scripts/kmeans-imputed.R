#Author: 
  #cleaner: Thomas Robertson
  #model: Logan Bolton

# Load necessary libraries
if (!requireNamespace("mice", quietly = TRUE)) install.packages("mice")
if (!requireNamespace("caret", quietly = TRUE)) install.packages("caret")
if (!requireNamespace("corrplot", quietly = TRUE)) install.packages("corrplot")
if (!requireNamespace("ggplot2", quietly = TRUE)) install.packages("ggplot2")

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

# Extract a complete, imputed dataset
complete_data <- complete(imputed_data, 1)  # Choose the first imputed dataset

# Select the Glucose and Insulin columns
selected_data <- as.matrix(complete_data[, c("Glucose", "Insulin")])


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
