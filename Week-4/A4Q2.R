# Load the universalbank dataset
Bank_data = read.csv("UniversalBank.csv")

str(Bank_data)
library(caret)
# Partition the data
set.seed(2)
UB_DataPartition = createDataPartition(Bank_data$Personal.Loan, p=0.6, list = FALSE)
train_data = Bank_data[UB_DataPartition,]
test_data = Bank_data[-UB_DataPartition,]

# build the model with two predictors Income and Age
Logit_Model = glm(Personal.Loan ~ Income + Age, data = train_data, family = "binomial")
Logit_Model

