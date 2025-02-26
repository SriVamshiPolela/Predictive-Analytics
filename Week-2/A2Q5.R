# Read the dataset file
ToyotaCorolla = read.csv("ToyotaCorolla.csv")
str(ToyotaCorolla)
dim(ToyotaCorolla)
ToyotaCorolla = ToyotaCorolla[1:1000,]
ToyotaCorolla = ToyotaCorolla[, c("Price", "Age_08_04", "KM", "Fuel_Type", "HP", "Met_Color", "Automatic", "Doors", "Quarterly_Tax", "Weight")]

# Convert the categorical data to factor
ToyotaCorolla[, c("Fuel_Type", "Met_Color", "Automatic")] <- lapply(ToyotaCorolla[, c("Fuel_Type", "Met_Color", "Automatic")], as.factor)


# Create a histogram for outcome variable
hist(ToyotaCorolla$Price, main = "Histogram for Prices")

# Exploring Relationships Among Features: The Correlation Matrix
correlation_matrix = cor(ToyotaCorolla[c("Price", "Age_08_04", "KM", "HP", "Quarterly_Tax", "Weight")])

library(corrplot)
# Set up the plot size
par(mar = c(2, 2, 2, 2))
# Create the correlation matrix plot
corrplot(correlation_matrix, method = "color", type = "upper", tl.col = "black", tl.srt = 45, addCoef.col = "black")
# Add a title
title(main = "Correlation Matrix", line = 0.5)

pairs(ToyotaCorolla[c("Price", "Age_08_04", "KM", "HP", "Quarterly_Tax", "Weight")])
pairs.panels(ToyotaCorolla[c("Price", "Age_08_04", "KM", "HP", "Quarterly_Tax", "Weight")])



# Model Fitting
library(caret)
set.seed(2023)
data_partition = createDataPartition(ToyotaCorolla$Price, p=0.6, list = FALSE)
train_data= ToyotaCorolla[data_partition,]
test_data= ToyotaCorolla[-data_partition,]

LM_Model = lm(Price~., data = ToyotaCorolla)
summary(LM_Model)

# Predict on test data
test_predict= predict(LM_Model, test_data)
summary(test_predict)

# Compare with observed values for outcome variable
summary(test_data$Price)

# Calculate correlation coefficient
correlation <- cor(test_predict, test_data$Price)

# Print the correlation coefficient
cat("Correlation Coefficient:", correlation)



# Use rpart for Increase the model performance 
library(rpart)
Rpart_Model<-rpart(Price~., data= train_data)
Rpart_Model

# Predict on test data
test_predict1 = predict(Rpart_Model, test_data)
summary(Rpart_Model)


# Evaluating Performance with Mean Absolute Error
MAE<-function(obs, pred){
  mean(abs(obs-pred))
}
MAE(test_data$Price, test_predict)
MAE(test_data$Price, test_predict1)

