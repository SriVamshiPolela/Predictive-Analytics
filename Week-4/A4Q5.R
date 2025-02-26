# Read the dataset file
ToyotaCorolla = read.csv("ToyotaCorolla.csv")
str(ToyotaCorolla)
dim(ToyotaCorolla)
ToyotaCorolla = ToyotaCorolla[1:1000,]
ToyotaCorolla = ToyotaCorolla[, c("Price", "Age_08_04", "KM", "Fuel_Type", "HP", "Met_Color", "Automatic", "Doors", "Quarterly_Tax", "Weight")]


# Creating dummies for fuel_type
library(fastDummies)

ToyotaCorolla = dummy_cols(ToyotaCorolla, select_columns = "Fuel_Type",remove_selected_columns = TRUE)

# Create training partition (60%) and testing partition (40%) with randomized partitioning
library(caret)
set.seed(24)

normalize <- function(x) {
  return((x - min(x)) / (max(x) - min(x)))
}
Toyota_norm<-as.data.frame(lapply(ToyotaCorolla, normalize))
# View(Toyota_norm)
set.seed(24)
Data_partition = createDataPartition(Toyota_norm$Price, p = 0.6, list = FALSE)
train.df = Toyota_norm[Data_partition,]
test.df = Toyota_norm[-Data_partition,] 

dim(train.df)
dim(test.df)

#install.packages('Deriv')

#install.packages("neuralnet")
library(neuralnet)
colnames(train.df)

# Training a Model on the Data
NeuralNetwork_Model = neuralnet(Price ~., data = train.df)
plot(NeuralNetwork_Model)

# Predict using the test data
Toyato_pred = predict(NeuralNetwork_Model, newdata = test.df)
# For unnormalized test data
correlation = cor(Toyato_pred, ToyotaCorolla$Price[-Data_partition])
# After normalized test data
correlation_norm = cor(Toyato_pred, test.df$Price)

correlation
correlation_norm

unnormalize = function(x, minv, maxv) {
  return( x * (maxv - minv) + minv )
}

UnNormialize_results<-unnormalize(Toyato_pred, min(ToyotaCorolla$Price), max(ToyotaCorolla$Price))

# After unnormalizing the predicted results
# Calculate the correlation between predicted and actual prices
correlation_unnorm = cor(UnNormialize_results, ToyotaCorolla$Price[-Data_partition])
correlation_unnorm

#Calculate the mean absolute error for the original model
mean_absolute_error = mean(abs(UnNormialize_results - ToyotaCorolla$Price[-Data_partition]))
mean_absolute_error
# Print the results
cat("Original Model - Correlation:", correlation, " Mean Absolute Error:", mean_absolute_error, "\n")

# Improve model performance
# Train the neural network model with 4 hidden nodes
Improved_NeuralNetwork_Model = neuralnet(
  Price ~ Age_08_04 + KM + HP + Met_Color + Automatic + Doors + Quarterly_Tax + Weight + Fuel_Type_CNG + Fuel_Type_Diesel + Fuel_Type_Petrol,
  data = train.df, 
  hidden = 4  # Specify 4 hidden nodes
)
plot(Improved_NeuralNetwork_Model)
# Make predictions on the testing data with the improved model
improved_predictions = predict(Improved_NeuralNetwork_Model, newdata = test.df)

UnNormialize_Improved_results = unnormalize(improved_predictions, min(ToyotaCorolla$Price), max(ToyotaCorolla$Price))

# Evaluate the improved model's performance
correlation_improved = cor(UnNormialize_Improved_results, ToyotaCorolla$Price[-Data_partition])
mean_absolute_error_improved = mean(abs(UnNormialize_Improved_results - ToyotaCorolla$Price[-Data_partition]))

# Print the results
cat("Improved Model - Correlation:", correlation_improved, " Mean Absolute Error:", mean_absolute_error_improved, "\n")

