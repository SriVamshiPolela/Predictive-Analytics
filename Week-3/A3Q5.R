# Load the dataset file
Bank_data = read.csv("UniversalBank.csv")

# Explore the dataset
str(Bank_data)

# Income based on Age 

# Remove Zipcode 
Bank_data = Bank_data[,-c(1,5)]
dim(Bank_data)
colnames(Bank_data)

library(fastDummies)
Bank_data = dummy_cols(Bank_data,select_columns = "Education")

# Extract numeric columns for scaling
numeric_columns <- Bank_data[, sapply(Bank_data, is.numeric)]
#Z_Score or we can also use scale predifined function
zscore <- function(x) {
  return((x - mean(x)) / sd(x))
}
# Apply z-score to numeric columns (excluding 'Personal.Loan')
scaled_data <- as.data.frame(sapply(numeric_columns, zscore))

# Add the 'Personal.Loan' column back to the scaled data frame
scaled_data$Personal.Loan <- Bank_data$Personal.Loan

# Print the first few rows of the scaled data
head(scaled_data)
# Assuming scaled_data contains your dataset with scaled numeric columns
# Assuming 'Personal.Loan' is the target variable

library(class)
library(caret)

# Assuming 'Personal.Loan' is the target variable
target_variable <- scaled_data$Personal.Loan

# Partition the data into training and testing sets
set.seed(2024)
data_partition <- createDataPartition(target_variable, p = 0.6, list = FALSE)
train_data <- scaled_data[data_partition, ]
test_data <- scaled_data[-data_partition, ]
train_target <- target_variable[data_partition]
test_target <- target_variable[-data_partition]


# Calculate the percentage of customers who accepted the personal loan in the entire dataset
Accepted <- mean(scaled_data$Personal.Loan) * 100
# Calculate the percentage of customers who accepted the personal loan in the training partition
train_Accepted <- mean(train_target == 1) * 100

# Calculate the percentage of customers who accepted the personal loan in the testing partition
test_Accepted <- mean(test_target == 1) * 100

Accepted
train_Accepted
test_Accepted
# Train the kNN model
k <- 5  # You can adjust the value of k
kNN_Model <- knn(train = train_data[, -ncol(train_data)],
                 test = test_data[, -ncol(test_data)],
                 cl = train_target,
                 k = k)

# Evaluate model performance with a confusion matrix
conf_matrix_kNN <- confusionMatrix(factor(kNN_Model, levels = c('1', '0')),
                                   factor(test_target, levels = c('1', '0')),
                                   positive = '1')

# Print the confusion matrix
print(conf_matrix_kNN)

# Extract performance metrics
accuracy <- conf_matrix_kNN$overall["Accuracy"]
sensitivity <- conf_matrix_kNN$byClass["Sensitivity"]
specificity <- conf_matrix_kNN$byClass["Specificity"]

# Print performance metrics
cat("Accuracy:", accuracy, "\n")
cat("Sensitivity:", sensitivity, "\n")
cat("Specificity:", specificity, "\n")

# Tune kNN model
# Tune kNN model
knntuning <- tune.knn(x = train_data[, -ncol(train_data)],
                      y = as.factor(train_target),
                      k = 1:30)

# Display the tuning results
print(knntuning)

# Plot the tuning results
plot(knntuning)

# Summarize the tuning results
summary(knntuning)

kNN_Model <- knn(train = train_data[, -ncol(train_data)],
                 test = test_data[, -ncol(test_data)],
                 cl = train_target,
                 k = 1)
# Evaluate model performance with a confusion matrix
conf_matrix_kNN <- confusionMatrix(factor(kNN_Model, levels = c('1', '0')),
                                   factor(test_target, levels = c('1', '0')),
                                   positive = '1')

# Print the confusion matrix
print(conf_matrix_kNN)




