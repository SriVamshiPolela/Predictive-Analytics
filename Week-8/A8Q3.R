# Load the data
Owner.df <- read.csv("OwnerExample.csv")

# Function to calculate metrics
calculate_metrics <- function(pred, actual) {
  TP <- sum(pred == "owner" & actual == "owner")
  TN <- sum(pred == "nonowner" & actual == "nonowner")
  FP <- sum(pred == "owner" & actual == "nonowner")
  FN <- sum(pred == "nonowner" & actual == "owner")
  
  accuracy <- mean(pred == actual)
  sensitivity <- TP / sum(actual == "owner")
  specificity <- TN / sum(actual == "nonowner")
  
  c(Accuracy = accuracy, Sensitivity = sensitivity, Specificity = specificity)
}

# Set thresholds
thresholds <- c(0, 0.2, 0.3, 0.5,0.6, 0.7, 0.8, 1.0)

# Apply function for each threshold
results <- sapply(thresholds, function(threshold) {
  pred <- ifelse(Owner.df$Propensity >= threshold, "owner", "nonowner")
  calculate_metrics(pred, Owner.df$Actual)
})

# Combine results into a data frame
results_df <- data.frame(Threshold = thresholds, t(results))
print(results_df)




library(pROC)
r <- roc(Owner.df$Actual, Owner.df$Propensity)
# Add text with AUC value to the plot
plot.roc(r)
text(0.7, 0.2, paste("AUC =", round(auc(r), 4)), adj = 0, col = "blue", cex = 1.2)

auc(r)

r$sensitivities
r$specificities
r$thresholds 
r$direction
r$cases
r$controls
r$auc
r$original.predictor
r$original.response
r$levels
r$response
r$predictor
r$call




