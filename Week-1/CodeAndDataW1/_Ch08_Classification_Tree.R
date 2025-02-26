# 2 Hands-on Example: Iris Data

install.packages("party")
library("party")
str(iris)
head(iris)
table(iris$Species)
iris_ctree <- ctree(Species ~ Sepal.Length + Sepal.Width + Petal.Length + Petal.Width, data=iris)
print(iris_ctree)
plot(iris_ctree, cex=2)

head(iris)
tail(iris)

library(rpart)
iris_rpart = rpart(Species~., data=iris)
print(iris_rpart)
# Use the `rattle::fancyRpartPlot` to generates an elegant plot
install.packages("rattle")
library(rattle)
fancyRpartPlot(iris_rpart, cex = 1.5, caption = "rattle::fancyRpartPlot (Iris flowers)")

# 3.4 C5.0 Decision Tree Algorithm
N <- 100
set.seed(1234)
x <- runif(N)
curve(-x*log2(x)-(1-x)*log2(1-x), col="red", main="Entropy for Different Proportions", xlab = "x (proportion for class 1)", ylab = "Entropy", lwd=3)
segments(0.8, 0, 0.8, 0.7219281, lwd=2, col="blue", lty = 2)
points(0.8, 0.7219281, col = "blue", pch=21, cex=2, lwd=2)
text(-0.02, 0.3, pos=4, srt = 90, "High-Structure/Low-Entropy")
text(0.48, 0.3, pos=4, srt = 90, "Low-Structure/High-Entropy")
text(0.96, 0.3, pos=4, srt = 90, "High-Structure/Low-Entropy")
text(0.65, 0.8, pos=4, "(x=0.8,Entr=0.72)")

install.packages("plotly")
library(plotly)
x_ind <- sort(x)
y <- -x_ind*log2(x_ind)-(1-x_ind)*log2(1-x_ind)
modelLabels <- c('High-Structure/Low-Entropy', 'Low-Structure/High-Entropy', 'High-Structure/Low-Entropy')
modelLabels.x <- c(0.03, 0.5, 0.97)
modelLabels.y <- c(0.5, 0.7, 0.5)
modelLabels.col <- c("blue", "red", "blue")
plot_ly() %>%
  add_lines(x = ~x_ind, y = ~y, name="Entropy", line = list(width = 4)) %>%
  add_segments(x=0.8, xend=0.8, y=0.07, yend = 0.7219281, showlegend=F) %>%
  # add_segments(x=-0.2, xend=1.1, y=0, yend = 0, showlegend=F) %>%
  # add_markers(x = ~x, y = ~y, name="Sample Simulated Data") %>%
  #add_lines(x = ~c(point1$x,point2$x), y = ~c(point1$y,point2$y), name="Second PC, Orthogonal to lm(Y ~ X)", 
  #          line = list(width = 4))  %>% 
  add_markers(x = 0.8, y = 0.7219281, name="(x=0.8,Entropy=0.72)", marker = list(size = 20,
                                                                                 color = 'green', line = list(color = 'yellow', width = 2))) %>%
  layout(title="Binary Class Entropy",
         xaxis=list(title="Proportion of Class 1 Obsevations", scaleanchor="y"),  # control the y:x axes aspect ratio
         yaxis = list(title="Entropy", scaleanchor  = "x", range=c(-0.1, 1.1)), legend = list(orientation = 'h'),
         annotations = list(text=modelLabels,  x=modelLabels.x, y=modelLabels.y, textangle=90,
                            font=list(size=15, color=modelLabels.col), showarrow=FALSE))

# 4 Case Study 1: Quality of Life and Chronic Disease

# 4.1 Step 1: Collecting Data
# data file: Case06_QoL_Symptom_ChronicIllness.csv

# 4.2 Step 2: exploring and preparing the data
qol<-read.csv("https://umich.instructure.com/files/481332/download?download_frd=1")
str(qol)
table(qol$QOL_Q_01)
qol<-qol[!qol$CHRONICDISEASESCORE==-9, ]
summary(qol$CHRONICDISEASESCORE)
qol$cd<-qol$CHRONICDISEASESCORE>1.497
# qol$cd<-factor(qol$cd, levels=c(F, T), labels = c("minor_disease", "severe_disease"))
# assuming it's more important to identify "severe disease"
qol$cd<-factor(qol$cd, levels=c(T, F), labels = c("severe_disease", "minor_disease"))

# 4.2.1 Data preparation: creating random training and test datasets
qol<-qol[order(qol$ID), ]
# Remove ID (col=1) # the clinical Diagnosis (col=41) will be handled later
qol <- qol[ , -1]
#qol_train<-qol[1:2114, ]
#qol_test<-qol[2115:2214, ]
set.seed(1234)
train_index <- sample(seq_len(nrow(qol)), size = 0.8*nrow(qol))
qol_train<-qol[train_index, ]
qol_test<-qol[-train_index, ]
prop.table(table(qol_train$cd))
prop.table(table(qol_test$cd))

# 4.3 Step 3: Training a Model On the Data
install.packages("C50")
library(C50)
summary(qol_train[,-c(40, 41)])
set.seed(1234)
qol_model<-C5.0(qol_train[,-c(40, 41)], qol_train$cd)
qol_model
summary(qol_model)
plot(qol_model)
# plot(qol_model, type="simple") #simple vs. extended 

# 4.4 Step 4: Evaluating Model Performance
# See docs for predict # ?C50::predict.C5.0
qol_pred<-predict(qol_model, qol_test[ ,-c(40, 41)])  # removing the last 2 columns CHRONICDISEASESCORE and cd, whjich represent the clinical outcomes we are predicting!
# install.packages("caret")
library(caret)
confusionMatrix(table(qol_pred, qol_test$cd))

# 4.5 Step 5: Trial Option
set.seed(1234)
qol_boost6<-C5.0(qol_train[ , -c(40, 41)], qol_train$cd, trials=6) # try alternative values for the trials option
qol_boost6
summary(qol_model)
plot(qol_boost6, type="simple")
qol_boost_pred6 <- predict(qol_boost6, qol_test[ ,-c(40, 41)])
confusionMatrix(table(qol_boost_pred6, qol_test$cd))

# 4.6 Loading the Misclassification Error Matrix
error_cost<-matrix(c(0, 4, 1, 0), nrow = 2)
error_cost
set.seed(1234)
qol_cost<-C5.0(qol_train[-c(40, 41)], qol_train$cd, costs=error_cost)
qol_cost_pred<-predict(qol_cost, qol_test)
confusionMatrix(table(qol_cost_pred, qol_test$cd))

# 4.7 Parameter Tuning
library("rpart") 
# remove CHRONICDISEASESCORE, but keep *cd* label
set.seed(1234)
qol_model<-rpart(cd~., data=qol_train[, -40], cp=0.01) 
# here we use rpart::cp = *complexity parameter* = 0.01
qol_model
library(rpart.plot)
rpart.plot(qol_model, type = 4,extra = 1,clip.right.labs = F)
library("rattle")
fancyRpartPlot(qol_model, cex = 1, caption = "rattle::fancyRpartPlot (QoL Data)")
qol_pred<-predict(qol_model, qol_test,type = 'class')
confusionMatrix(table(qol_pred, qol_test$cd))

set.seed(1234)
control = rpart.control(cp = 0.000, xval = 10, minsplit = 2)
qol_model= rpart(cd ~ ., data = qol_train[ , -40], control = control)

plotcp(qol_model)
printcp(qol_model)

set.seed(1234)
selected_tr <- prune(qol_model, cp= qol_model$cptable[which.min(qol_model$cptable[,"xerror"]),"CP"])
fancyRpartPlot(selected_tr, cex = 1, caption = "rattle::fancyRpartPlot (QoL Data)")
qol_pred_tune<-predict(selected_tr, qol_test,type = 'class')
confusionMatrix(table(qol_pred_tune, qol_test$cd))
#fancyRpartPlot(qol_model, cex = 0.1, caption = "rattle::fancyRpartPlot (QoL Data)")

# 5 Compare Different Impurity Indices
set.seed(1234)
qol_model = rpart(cd ~ ., data=qol_train[ , -40], parms = list(split = "entropy"))
fancyRpartPlot(qol_model, cex = 1, caption = "rattle::fancyRpartPlot (QoL data)")
# Modify and test using "error" and "gini"
# qol_pred<-predict(qol_model, qol_test,type = 'class')
# confusionMatrix(table(qol_pred, qol_test$cd))

# 7 Case Study 2: QoL in Chronic Disease (Take 2)
# 7.1 Step 3: Training a Model on the Data
# install.packages("RWeka")
library(RWeka)
# just remove the CHRONICDISEASESCORE but keep cd
set.seed(1234)
qol_1R<-OneR(cd~., data=qol[ , -40])
qol_1R

# 7.2 Step 4: Evaluating Model Performance
summary(qol_1R)

# 7.3 Step 5: Alternative Model1
set.seed(1234)
qol_jrip1<-JRip(cd~., data=qol[ , -40])
qol_jrip1
summary(qol_jrip1)

# 7.4 Step 5: Alternative Model2
# install.packages("randomForest")
require(randomForest)
set.seed(12)
# rf.fit <- tuneRF(qol_train[ , -40], qol_train[ , 40], stepFactor=1.5)
rf.fit <- randomForest(cd~. , data=qol_train[ , -40],importance=TRUE,ntree=2000,mtry=26)
varImpPlot(rf.fit, cex=0.5); print(rf.fit)
plot(rf.fit,main="MSE Errors vs. Iterations")

#rf.fit1 <- randomForest(cd~. , data=qol_train[ , -40],importance=TRUE,ntree=2000,mtry=26)
rf.fit2 <- randomForest(cd~. , data=qol_train[ , -40], importance=TRUE, nodesize=5, ntree=5000, mtry=26)

#plot(rf.fit,log="x",main="MSE Errors vs. Iterations")
#points(1:5000, rf.fit1$mse, col="red", type="l")
#points(1:5000, rf.fit2$mse, col="green", type="l")
#legend("topright", col=c("black", "red", "green"), lwd=c(2, 2, 2), legend=c("rf.fit", "rf.fit1", "rf.fit2"))

qol_pred2<-predict(rf.fit2, qol_test, type = 'class')
confusionMatrix(table(qol_pred2, qol_test$cd))
