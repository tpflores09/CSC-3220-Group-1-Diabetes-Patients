# Author: Thomas D. Robertson II
# Load necessary library
if (!requireNamespace("mice", quietly = TRUE)) install.packages("mice")
library("mice")

# Load the dataset
data <- read.csv(choose.files(), stringsAsFactors = FALSE)

# Replace zeros with NA for specific columns
columns_with_zeros <- c("Glucose", "BloodPressure", "SkinThickness", "Insulin", "BMI")
data[columns_with_zeros] <- lapply(data[columns_with_zeros], function(x) ifelse(x == 0, NA, x))

# Perform MICE Imputation
# Adjust m (number of imputations) or method (pmm, norm, etc.)
imputed_data <- mice(data, m = 5, method = 'pmm', seed = 123)

# Extract the first imputed dataset as a dataframe
complete_data <- complete(imputed_data, 1)

# Save the imputed dataset to a file
# write.csv(complete_data, "imputed_data.csv", row.names = FALSE)

# display imputed dataset
head(complete_data)  
