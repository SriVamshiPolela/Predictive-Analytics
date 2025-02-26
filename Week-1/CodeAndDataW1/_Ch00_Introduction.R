# The Steps in Data Analytics

# Predicting home values in the West Roxbury neighborhood
# Steps typical to many data analytics tasks using a familiar procedure: multiple linear regression

# 1. determine the purpose
# predict the value of homes in West Roxbury

# 2. obtain the data 
housing.df <- read.csv("WestRoxbury.csv", header = TRUE)  # load data
#housing.df <- mlba::WestRoxbury # load data from mlba package

# 3. explore, clean and proprocess the data, reduce data dimention if needed
dim(housing.df)  # find the dimension of data frame
head(housing.df)  # show the first six rows
housing.df$REMODEL <- as.factor(housing.df$REMODEL)
class(housing.df)
str(housing.df)

housing.df$TOTAL.VALUE  # a different way to show the whole first column
length(housing.df$TOTAL.VALUE)  # find the length of the first column
mean(housing.df$TOTAL.VALUE)  # find the mean of the first column
summary(housing.df)  # find summary statistics for each column

names(housing.df)  # print a list of variables to the screen.
t(t(names(housing.df)))  # print the list in a useful column format
class(housing.df$REMODEL) # REMODEL is a factor variable
levels(housing.df[, 14])  # It can take one of three levels
class(housing.df$BEDROOMS)  # BEDROOMS is an integer variable
class(housing.df[, 1])  # Total_Value is a numeric variable

library(fastDummies)
housing.dummy.df <- dummy_cols(housing.df, select_columns = c("REMODEL"), 
                               remove_first_dummy = FALSE, remove_selected_columns = TRUE)

names(housing.dummy.df)

# 4. Reduce the data dimension
# removed variables such as bldg_type, roof_type, and ext_fin

# 5. determine machine learning task
# supervised prediction task, outcome is TOTAL_VALUE

# 6. partition the data if necessary
set.seed(1)
## partitioning into training (60%) and validation (40%)
train.rows <- sample(rownames(housing.df), dim(housing.df)[1]*0.6)
holdout.rows <- setdiff(rownames(housing.df), train.rows) 
train.df <- housing.df[train.rows, ]
holdout.df <- housing.df[holdout.rows, ]

## partitioning into training (60%) and holdout (40%) using caret
set.seed(1)
idx <- caret::createDataPartition(housing.df$TOTAL.VALUE, p=0.6, list=FALSE)
train.df <- housing.df[idx, ]
holdout.df <- housing.df[-idx, ]

# 7. choose the technique
# it is multiple linear regression

# 8. use the algorithm to perform the task
reg <- lm(TOTAL.VALUE ~ .-TAX, data = train.df) # remove variable "TAX"
options(scipen=999)
#summary(reg)
tr.res <- data.frame(train.df$TOTAL.VALUE, reg$fitted.values, reg$residuals)
head(tr.res)

pred <- predict(reg, newdata = holdout.df)
vl.res <- data.frame(holdout.df$TOTAL.VALUE, pred, residuals = holdout.df$TOTAL.VALUE - pred)
head(vl.res)

library(forecast)
# compute accuracy on training set
accuracy(reg$fitted.values, train.df$TOTAL.VALUE)

# compute accuracy on prediction set
pred <- predict(reg, newdata = holdout.df)
accuracy(pred, holdout.df$TOTAL.VALUE)

# 9. interpret the results
# try other algorithms and settings 

# 10 deploy the model
# predict TOTAL VALUE for homes where this value is unknown
new.data <- read.csv("WestRoxbury_new_data.csv", header = TRUE)
new.data

pred <- predict(reg, newdata = new.data)
pred
