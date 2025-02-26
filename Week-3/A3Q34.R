library(dplyr)
#Min-Max
normalize <- function(x) {
  return((x - min(x)) / (max(x) - min(x)))
}
#Z_Score or we can also use scale predifined function
zscore <- function(x) {
  return((x - mean(x)) / sd(x))
}

# Customer data
customer_data <- data.frame(
  Age = c(25, 56, 65, 32, 41, 49),
  Income = c(49000, 156000, 99000, 192000, 39000, 57000),
  Female = c(0, 1, 1, 0, 0, 0)
)

# Display original data
print("Original Data:")
print(customer_data)

# Rescale the data using min-max normalization
min_max_rescaled_data <- customer_data %>%
  mutate_at(vars(-contains("Record")), normalize)

# Rescale the data using z-score standardization
zscore_rescaled_data <- customer_data %>%
  mutate_at(vars(-contains("Record")), zscore)

# Display rescaled data
print("Min-Max Rescaled Data:")
print(min_max_rescaled_data)

print("Z-score Rescaled Data:")
print(zscore_rescaled_data)

# Compute distance matrix before rescaling
original_distance <- dist(customer_data[, -1])
# Compute distance matrix after min-max normalization
min_max_distance <- dist(min_max_rescaled_data)
# Compute distance matrix after z-score standardization
zscore_distance <- dist(zscore_rescaled_data)
original_distance
min_max_distance
zscore_distance

# Identify pairs with minimum and maximum distances
min_original <- which(original_distance == min(original_distance), arr.ind = TRUE)
max_original <- which(original_distance == max(original_distance), arr.ind = TRUE)

min_minmax <- which(min_max_distance == min(min_max_distance), arr.ind = TRUE)
max_minmax <- which(min_max_distance == max(min_max_distance), arr.ind = TRUE)

min_zscore <- which(zscore_distance == min(zscore_distance), arr.ind = TRUE)
max_zscore <- which(zscore_distance == max(zscore_distance), arr.ind = TRUE)
# Display the pairs
min_original
max_original
min_minmax
max_minmax
min_zscore
max_zscore






# ***************Question 4 ********************
library(e1071)
library(dplyr)
# Create the dataset
Online_Shopper.df <- read.csv("OnlineShopper.csv")
Online_Shopper.df <- Online_Shopper.df %>% mutate_at(vars(sports.com, gossip.com, finance.com, health.com, cooking.com, purchaser), as.factor)

# The naive Bayes model
naive_bayes_model <- naiveBayes(purchaser ~ ., data = Online_Shopper.df)
naive_bayes_model
# Create a new data frame for the new customer
new_cust <- data.frame(sports.com = "Yes", gossip.com = "No", finance.com = "No", health.com = "No", cooking.com = "No", purchaser = "No")
prediction_probs <- predict(naive_bayes_model, newdata = new_cust, type = "raw")
print(prediction_probs)
