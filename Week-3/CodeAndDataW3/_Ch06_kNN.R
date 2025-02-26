# 3 Case Study: Youth Development

# 3.1 Step 1: Collecting Data
## datafile: CaseStudy02_Boystown_Data.csv

# 3.2 Step 2: Exploring and Preparing the Data
library(class)
library(gmodels)
boystown<-read.csv("https://umich.instructure.com/files/399119/download?download_frd=1", sep=" ")
boystown$sex<-boystown$sex-1
boystown$dadjob<--1*(boystown$dadjob-2)
boystown$momjob<--1*(boystown$momjob-2)
str(boystown)

# First explore the data by running a PCA
rawData <- boystown[ , -1] 
head(rawData)
pca1 <- prcomp(as.matrix(rawData), center = T)
summary(pca1)
pca1$rotation

# more than 1 vandalism or larceny conviction
Y <- ifelse (boystown$vandalism + boystown$larceny > 1, "Recidivism", "Control")  
# covariate set excludes these 3 columns index/ID, vandalism, and larceny
X <- boystown[, -c(1, 10, 11)]  

boystown_z <- as.data.frame(lapply(X, scale))
bt_train <- boystown_z[1:150, ]
bt_test  <- boystown_z[151:200, ]
bt_train_labels <- Y[1:150]  
bt_test_labels  <- Y[151:200]
table(bt_train_labels)

round(prop.table(table(bt_train_labels)), digits=2)
round(prop.table(table(bt_test_labels)), digits=2)

# 3.3 Normalizing Data
normalize<-function(x){
  return((x-min(x))/(max(x)-min(x)))
}
# some test examples:
normalize(c(1, 2, 3, 4, 5))
normalize(c(1, 3, 6, 7, 9))

# apply the normalization transformation to predictors
boystown_n<-as.data.frame(lapply(X, normalize))

# alternatively we can use "scale", as we showed above
#boystown_z <- as.data.frame(lapply(X, scale))

summary(boystown_n$Alcoholuse)

summary(boystown_z$Alcoholuse)

# 3.4 Data preparation - creating training and test datasets
# may want to use random split of the raw data into training and testing
# Note that the object boystown_n/boystown_z already excludes the outcome variable
bt_train <- boystown_z[1:150, ]
bt_test  <- boystown_z[151:200, ]

bt_train_labels <- Y[1:150]  
bt_test_labels  <- Y[151:200]

# 3.5 Step 3: Training a Model On the Data
#install.packages('class', repos = "http://cran.us.r-project.org")
library(class)
bt_test_pred<-knn(train=bt_train, test=bt_test, cl=bt_train_labels, k=7)

# 3.6 Step 4: Evaluating model performance
# install.packages("gmodels", repos="http://cran.us.r-project.org")
library(gmodels)
CrossTable(x=bt_test_labels, y=bt_test_pred, prop.chisq = F)

# 3.7 Step 5: Improving Model Performance
# The normalization strategy may play a role in the classification performance
bt_train_n <- boystown_n[1:150, ]
bt_test_n  <- boystown_n[151:200, ]
bt_test_pred<-knn(train=bt_train_n, test=bt_test_n, cl=bt_train_labels, k=7)
CrossTable(x=bt_test_labels, y=bt_test_pred, prop.chisq = F)

# retrieve Probabilities
bt_test_pred <- knn(train=bt_train, test=bt_test, cl=bt_train_labels, prob=T, k=14) 
#bt_test_pred <- knn(train=bt_train_n, test=bt_test_n, cl=bt_train_labels, prob=T, k=14) # with normalization

# Binarize the probabilities
bt_test_predBin <- ifelse(attributes(bt_test_pred)$prob > 0.6 & bt_test_pred == "Recidivism", "Recidivism", "Control")          
CT <- CrossTable(x=bt_test_labels, y=bt_test_predBin, prop.chisq = F)
print(paste0("Prediction accuracy of model 'bt_test_pred' is ", (CT$prop.tbl[1,1]+CT$prop.tbl[2,2]) ))

# 3.8 Testing Alternative Values of k
bt_train <- boystown_z[1:150, ]
bt_test  <- boystown_z[151:200, ]
bt_train_labels <- Y[1:150]
bt_test_labels  <- Y[151:200]
bt_test_pred1   <- knn(train=bt_train, test=bt_test, cl=bt_train_labels, k=1)
bt_test_pred9   <- knn(train=bt_train, test=bt_test, cl=bt_train_labels, k=9)
bt_test_pred11  <- knn(train=bt_train, test=bt_test,  cl=bt_train_labels, k=11)
bt_test_pred21  <- knn(train=bt_train, test=bt_test,  cl=bt_train_labels, k=21)
bt_test_pred27  <- knn(train=bt_train, test=bt_test, cl=bt_train_labels, k=27)
ct_1  <- CrossTable(x=bt_test_labels, y=bt_test_pred1, prop.chisq = F)
ct_9  <- CrossTable(x=bt_test_labels, y=bt_test_pred9, prop.chisq = F)
ct_11 <- CrossTable(x=bt_test_labels, y=bt_test_pred11, prop.chisq = F)
ct_21 <- CrossTable(x=bt_test_labels, y=bt_test_pred21, prop.chisq = F)
ct_27 <- CrossTable(x=bt_test_labels, y=bt_test_pred27, prop.chisq = F)

bt_test_pred <- knn(train=bt_train, test=bt_test,  cl=bt_train_labels, prob=T, k=18)        # retrieve Probabilities
bt_test_predBin <- ifelse(attributes(bt_test_pred)$prob > 0.6 & bt_test_pred == "Recidivism", "Recidivism", "Control")     # Binarize the probabilities
CT <- CrossTable(x=bt_test_labels, y=bt_test_predBin, prop.chisq = F)
# CT 
print(paste0("Prediction accuracy of model 'bt_test_pred' (k=18) is ", (CT$prop.tbl[1,1]+CT$prop.tbl[2,2]) ))

# install.packages("e1071")
library(e1071)
set.seed(121)
knntuning = tune.knn(x= bt_train, y = as.factor(bt_train_labels), k = 1:30)
knntuning
plot(knntuning)
summary(knntuning)

# library(caret)
# knnControl <- trainControl(
#   method = "cv", ## cross validation
#   number = 10,   ## 10-fold
#   summaryFunction = twoClassSummary,
#   classProbs = TRUE,
#   verboseIter = FALSE
# )
# # bt_train_stringLabels <- ifelse (bt_train_labels==1, "Recidivism", "Control")
# knn_model <- train(x=bt_train, y=bt_train_labels , metric = "ROC", method = "knn", tuneLength = 20, trControl = knnControl)
# print(knn_model)

# Here we are providing a cutoff of 60% probability for derived class label=Recidivism
# library(dplyr)
# summaryPredictions <- predict(knn_model, newdata = bt_test, type = "prob") # %>% mutate('knnPredClass'=names(.)[apply(., 1, which.max)])
# summaryPredictionsLabel <- ifelse (summaryPredictions$Recidivism > 0.6, "Recidivism", "Control")
# testDataPredSummary <- as.data.frame(cbind(trueLabels=bt_test_labels, controlProb=summaryPredictions$Control, 
#                                            recidivismProb=summaryPredictions$Recidivism, knnPredLabel=summaryPredictionsLabel))
# print(paste0("Accuracy = ", 2*as.numeric(table(testDataPredSummary$trueLabels == testDataPredSummary $knnPredLabel)[2]), "%"))

library(class)
library(ggplot2)
library(reshape2)
# define a function that generates CV folds
cv_partition <- function(y, num_folds = 10, seed = NULL) {
  if(!is.null(seed)) {
    set.seed(seed)
  }
  n <- length(y)
  
  # split() divides the data into the folds defined by gl().
  # gl() generates factors according to the pattern of their levels
  
  folds <- split(sample(seq_len(n), n), gl(n = num_folds, k = 1, length = n))
  folds <- lapply(folds, function(fold) {
    list(
      training = which(!seq_along(y) %in% fold),
      test = fold
    )
  })
  names(folds) <- paste0("Fold", names(folds))
  return(folds)
}

# Generate 10-folds of the data
folds = cv_partition(bt_train_labels, num_folds = 10)

# Define a training set_CV_error calculation function
train_cv_error = function(K) {
  #Train error
  knnbt = knn(train = bt_train, test = bt_train, 
              cl = bt_train_labels, k = K)
  train_error = mean(knnbt != bt_train_labels)
  
  #CV error
  cverrbt = sapply(folds, function(fold) {
    mean(bt_train_labels[fold$test] != knn(train = bt_train[fold$training,], cl = bt_train_labels[fold$training], test = bt_train[fold$test,], k=K))
  }
  )
  
  cv_error = mean(cverrbt)
  
  #Test error
  knn.test = knn(train = bt_train, test = bt_test, 
                 cl = bt_train_labels, k = K)
  test_error = mean(knn.test != bt_test_labels)
  return(c(train_error, cv_error, test_error))
}

set.seed(121)
k_err = sapply(1:30, function(k) train_cv_error(k))
df_errs = data.frame(t(k_err), 1:30)
colnames(df_errs) = c('Train', 'CV', 'Test', 'K')
dataL <- melt(df_errs, id="K")

# require(ggplot2)
# library(reshape2)
ggplot(dataL, aes_string(x="K", y="value", colour="variable",
   group="variable", linetype="variable", shape="variable")) +
   geom_line(size=0.8) + labs(x = "Number of nearest neighbors (k)",
           y = "Classification error",
           colour="", group="",
           linetype="", shape="") +
  geom_point(size=2.8) +
  geom_vline(xintercept=9:10, colour = "pink")+
  geom_text(aes(9,0,label = "9", vjust = 1)) +
  geom_text(aes(10,0,label = "10", vjust = 1))

library(plotly)
plot_ly(dataL, x = ~K, y = ~value, color = ~variable, type = "scatter", mode = "markers+lines") %>% 
  add_segments(x=25, xend=25, y=0.0, yend=0.33, type = "scatter", name="k=9",
               line=list(color="darkgray", width = 2, dash = 'dot'), mode = "lines", showlegend=FALSE) %>% 
  add_segments(x=14, xend=14, y=0.0, yend=0.33, type = "scatter", name="k=14",
               line=list(color="lightgray", width = 2, dash = 'dot'), mode = "lines", showlegend=FALSE) %>% 
  add_segments(x=18, xend=18, y=0.0, yend=0.36, type = "scatter",  name="k=18",
               line=list(color="gray", width = 2, dash = 'dot'), mode = "lines", showlegend=FALSE) %>% 
  layout(title='K-NN Training, CV, and Testing Error Rates against k', 
         legend=list(title=list(text='<b> Samples </b>')), 
         xaxis=list(title='Number of nearest neighbors (k)'), yaxis=list(title='Classification error'))

# 3.9 Quantitative Assessment
bt_test_pred <- knn(train=bt_train, test=bt_test,  cl=bt_train_labels, prob=T, k=12) # retrieve Probabilities
bt_test_predBin <- ifelse(attributes(bt_test_pred)$prob > 0.6 & bt_test_pred == "Recidivism", "Recidivism", "Control") # Binarize the probabilities
CT <- CrossTable(x=bt_test_labels, y=bt_test_predBin, prop.chisq = F)

mod12_TN <- CT$prop.row[1, 1]  
mod12_FP <- CT$prop.row[1, 2]
mod12_FN <- CT$prop.row[2, 1]
mod12_TP <- CT$prop.row[2, 2]

mod12_sensi <- mod12_TN/(mod12_TN+mod12_FP) 
mod12_speci <- mod12_TP/(mod12_TP+mod12_FN)
print(paste0("kNN model k=12 Sensitivity=", mod12_sensi))
print(paste0("kNN model k=12 Specificity=", mod12_speci))

table(bt_test_labels, bt_test_predBin)

# install.packages("caret")
library("caret")
# Model 12: bt_test_predBin
confusionMatrix(as.factor(bt_test_predBin), as.factor(bt_test_labels))

# install.packages("scatterplot3d")
library(scatterplot3d)
grid_xy <- matrix(c(0, 1, 1, 0), nrow=2, ncol=2)
intensity <- matrix(c(mod12_TN, mod12_FN, mod12_FP, mod12_TP), nrow=2, ncol=2)

# scatterplot3d(grid_xy, intensity, pch=16, highlight.3d=TRUE, type="h", main="3D Scatterplot") 

s3d.dat <- data.frame(cols=as.vector(col(grid_xy)), 
                      rows=as.vector(row(grid_xy)), 
                      value=as.vector(intensity))
scatterplot3d(s3d.dat, pch=16, highlight.3d=TRUE, type="h", xlab="real", ylab="predicted", zlab="Agreement", 
              main="3D Scatterplot: Model12 Results (FP, FN, TP, TN)") 

# scatterplot3d(s3d.dat, type="h", lwd=5, pch=" ", xlab="real", ylab="predicted", zlab="Agreement", main="Model9 Results (FP, FN, TP, TN)")

plot_ly(x = c("TN", "FN", "FP", "TP"),
        y = c(mod12_TN, mod12_FN, mod12_FP, mod12_TP),
        name = c("TN", "FN", "FP", "TP"), type = "bar", color=c("TN", "FN", "FP", "TP")) %>% 
  layout(title="Confusion Matrix", 
         legend=list(title=list(text='<b> Model k=12; Performance Metrics </b>')), 
         xaxis=list(title='Metrics'), yaxis=list(title='Probability'))
