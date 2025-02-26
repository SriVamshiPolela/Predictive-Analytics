library(ROCR)
data(ROCR.simple)
df <- data.frame(ROCR.simple)
head(df)




# Load necessary library
library(ROCR)

# Load the dataset
data(ROCR.simple)
df <- data.frame(ROCR.simple)

# Define the function to calculate accuracy, sensitivity, and specificity
calculate_metrics <- function(predictions, actual, threshold) {
  # Convert probabilities to binary based on the threshold
  predicted_class <- ifelse(predictions >= threshold, 1, 0)
  
  # Confusion matrix components
  TP <- sum(predicted_class == 1 & actual == 1)  # True Positives
  TN <- sum(predicted_class == 0 & actual == 0)  # True Negatives
  FP <- sum(predicted_class == 1 & actual == 0)  # False Positives
  FN <- sum(predicted_class == 0 & actual == 1)  # False Negatives
  
  # Calculate accuracy, sensitivity (recall), and specificity
  accuracy <- (TP + TN) / (TP + TN + FP + FN)
  sensitivity <- TP / (TP + FN)  # True Positive Rate
  specificity <- TN / (TN + FP)  # True Negative Rate
  
  return(c(accuracy = accuracy, sensitivity = sensitivity, specificity = specificity))
}

# Define thresholds
thresholds <- c(0, 0.2, 0.3, 0.5, 0.7, 0.8, 1.0)

# Loop over thresholds and calculate metrics
results <- data.frame(Threshold = thresholds, Accuracy = NA, Sensitivity = NA, Specificity = NA)

for (i in 1:length(thresholds)) {
  metrics <- calculate_metrics(df$predictions, df$labels, thresholds[i])
  results[i, "Accuracy"] <- metrics["accuracy"]
  results[i, "Sensitivity"] <- metrics["sensitivity"]
  results[i, "Specificity"] <- metrics["specificity"]
}

# Print the results
print(results)