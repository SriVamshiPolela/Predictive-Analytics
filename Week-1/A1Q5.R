# Libraries used
library(caret)
library(C50)
library(ROSE)
# Read the UniversalBank dataset csv file
UniversalBank = read.csv("UniversalBank.csv")

# Explore the dataset
colnames(UniversalBank)
head(UniversalBank)

str(UniversalBank)

# Removing the Irrelevant attributes (Zipcode)
UniversalBank = UniversalBank[, !(names(UniversalBank) %in% c("ZIP.Code"))]
colnames(UniversalBank) # make sure whether the Zipcode attribute removed or not.

# Check the Education attruibute
unique(UniversalBank$Education)
# Education attribute as to be factored
UniversalBank$Education = factor(UniversalBank$Education, levels = c(1,2,3), labels = c("Undergrad","Graduate","Advanced/Professional"))
levels(UniversalBank$Education)

# Check the target attribute personal Loan
unique(UniversalBank$Personal.Loan) 
# convert the personal loan attribute into categorical format
UniversalBank$Personal.Loan = factor(UniversalBank$Personal.Loan, c(1,0), labels = c("Acceptor","Non-Acceptor"))
levels(UniversalBank$Personal.Loan)

# Check the	Securities Account,	CD Account,	Online,	CreditCard attributes
lapply(UniversalBank[c("Securities.Account", "CD.Account", "Online", "CreditCard")], unique)
# These are binary variables
# Convert specified columns to factors
lapply(UniversalBank[c("Securities.Account", "CD.Account", "Online", "CreditCard")], factor)


# The percentage of customers who accepted the personal loan
table(UniversalBank$Personal.Loan)
prop.table(table(UniversalBank$Personal.Loan))

# Partition the data
set.seed(1999)
UB_DataPartition = createDataPartition(UniversalBank$Personal.Loan, p=0.6, list = FALSE)
train_data = UniversalBank[UB_DataPartition,]
test_data = UniversalBank[-UB_DataPartition,]

# Check class distribution in train data
table(train_data$Personal.Loan)
prop.table(table(train_data$Personal.Loan))

# check class distribution in test data
table(test_data$Personal.Loan)
prop.table(table(test_data$Personal.Loan))

# Using the c5.0 model to train a model
UB_Model = C5.0(train_data$Personal.Loan~., data = train_data[,-1])
UB_Model
# Tree size
# Plot the tree
plot(UB_Model)


# Evaluate the performance
Predict_test1 = predict(UB_Model,test_data)
confusionMatrix(table(Predict_test1, test_data$Personal.Loan))

# Optimize the model performance with Trial Option (trials =6)

UB_Model_boost = C5.0(train_data$Personal.Loan~.,data = train_data[,-1], trials = 6)
UB_Model_boost

Predict_test3 = predict(UB_Model_boost, test_data)
confusionMatrix(table(Predict_test3, test_data$Personal.Loan))

# Check with Trial Option (trials =10)

UB_Model_boost10 = C5.0(train_data$Personal.Loan~., data = train_data[,-1], trials = 10)
UB_Model_boost10

Predict_test4 = predict(UB_Model_boost10, test_data)
confusionMatrix(table(Predict_test4, test_data$Personal.Loan))



# Parameter tuning 
library(rpart)
UB_Model_rpart = rpart(Personal.Loan~., data = train_data[,-1], method = "class", minsplit=5,xval=5)
UB_Model_rpart
printcp(UB_Model_rpart)
library(rpart.plot)
# prune by lower cp
pruned.ct <- prune(UB_Model_rpart,
                   cp=UB_Model_rpart$cptable[which.min(UB_Model_rpart$cptable[,"xerror"]),"CP"])
sum(pruned.ct$frame$var == "<leaf>")
rpart.plot(pruned.ct, extra=1, fallen.leaves=FALSE)

