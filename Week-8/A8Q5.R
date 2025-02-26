
# Step 1: Collecting data 
Bank_data = read.csv("UniversalBank.csv")

# Step 2: Exploring data and preparing data (Remove zipcode, making sure categorical data are factored correctly. 
str(Bank_data)
head(Bank_data)
Bank_data = Bank_data[,-c(1,5)]
colnames(Bank_data)
lapply(Bank_data, unique)

library(fastDummies)
Bank_data = dummy_cols(Bank_data, select_columns = "Education", remove_selected_columns = TRUE)

# convert Personal.Loan to a factor with labels Yes and No
Bank_data$Personal.Loan = factor(Bank_data$Personal.Loan, levels=c(1, 0), labels=c("Yes", "No"))
                       
table(Bank_data$Personal.Loan)

# Create training partition (60%) and testing partition (40%) with randomized partitioning. Set a seed so the results can be reproduced.)
library(caret)
set.seed(2024)
Data_partition = createDataPartition(Bank_data$Personal.Loan, p=0.6, list = FALSE)
train.df = Bank_data[Data_partition,]
test.df = Bank_data[-Data_partition,]
dim(train.df)
dim(test.df)

library(rpart)
library(rpart.plot)

# train model 
Class.Model = rpart(Personal.Loan~., train.df)
Class.Predict = predict(Class.Model, test.df, type = "class")
rpart.plot(Class.Model)
confusionMatrix(Class.Predict, test.df$Personal.Loan, positive = "Yes")

#library(gmodels)
#CrossTable(Class.Predict, test.df$Personal.Loan)

# Step 5: Improving model performance with meta-leaning 

# bagging default nbagg=25

bag = ipred::bagging(Personal.Loan ~ ., data = train.df)
bag_pred = predict(bag, test.df, type = "class")
confusionMatrix(bag_pred, test.df$Personal.Loan, positive = "Yes")


# Random Forest
library(randomForest)
rf <- randomForest(Personal.Loan ~ ., data = train.df)  
rf_pred <- predict(rf, test.df, type = "class")
confusionMatrix(rf_pred, test.df$Personal.Loan)

# boosting default: coeflearn = 'Breiman'

install.packages("ada", repos = "https://cran.r-project.org")
library(ada)
install.packages("rlist", repos = "https://cran.r-project.org")
library(rlist)

boost <- adabag::boosting(Personal.Loan ~ ., data = train.df)
boost_pred <- predict(boost, test.df, type = "class")
# Check the length of both vectors
length(boost_pred$class)
length(test.df$Personal.Loan)
confusionMatrix(factor(boost_pred$class, levels = c("Yes", "No")), test.df$Personal.Loan)







