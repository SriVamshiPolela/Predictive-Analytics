# 1 Measuring the Performance of Classification Methods
hn_med<-read.csv("https://umich.instructure.com/files/1614350/download?download_frd=1", stringsAsFactors = FALSE)
hn_med$seer_stage<-factor(hn_med$seer_stage)
require(tm)
hn_med_corpus<-Corpus(VectorSource(hn_med$MEDICATION_SUMMARY))
corpus_clean<-tm_map(hn_med_corpus, tolower)
corpus_clean<-tm_map(corpus_clean, removePunctuation)
corpus_clean <- tm_map(corpus_clean, stripWhitespace)
corpus_clean <-tm_map(corpus_clean, removeNumbers)
hn_med_dtm<-DocumentTermMatrix(corpus_clean)
hn_med_train<-hn_med[1:562, ]
hn_med_test<-hn_med[563:662, ]
hn_med_dtm_train<-hn_med_dtm[1:562, ]
hn_med_dtm_test<-hn_med_dtm[563:662, ]
corpus_train<-corpus_clean[1:562]
corpus_test<-corpus_clean[563:662]
hn_med_train$stage<-hn_med_train$seer_stage %in% c(4, 5, 7)
hn_med_train$stage<-factor(hn_med_train$stage, levels=c(F, T), labels = c("early_stage", "later_stage"))
hn_med_test$stage<-hn_med_test$seer_stage %in% c(4, 5, 7)
hn_med_test$stage<-factor(hn_med_test$stage, levels=c(F, T), labels = c("early_stage", "later_stage"))

convert_counts <- function(x) {
  x <- ifelse(x > 0, 1, 0)
  x <- factor(x, levels = c(0, 1), labels = c("No", "Yes"))
  return(x)
}

hn_med_dict<-as.character(findFreqTerms(hn_med_dtm_train, 5))
hn_train<-DocumentTermMatrix(corpus_train, list(dictionary=hn_med_dict))
hn_test<-DocumentTermMatrix(corpus_test, list(dictionary=hn_med_dict))
hn_train <- apply(hn_train, MARGIN = 2, convert_counts)
hn_test <- apply(hn_test, MARGIN = 2, convert_counts)
library(e1071)
hn_classifier <- naiveBayes(hn_train, hn_med_train$stage)

pred_raw<-predict(hn_classifier, hn_test, type="raw")
head(pred_raw)

pred_nb<-predict(hn_classifier, hn_test)
head(stats::ftable(pred_nb))
#head(pred_nb)
#table(pred_nb)

# Back in Chapter 8 where we discussed the C5.0 and the randomForest classifiers to predict the chronic disease score in a another case-study Quality of Life (QoL).
qol<-read.csv("https://umich.instructure.com/files/481332/download?download_frd=1")	
qol<-qol[!qol$CHRONICDISEASESCORE==-9, ]	
qol$cd<-qol$CHRONICDISEASESCORE>1.497	
qol$cd<-factor(qol$cd, levels=c(F, T), labels = c("minor_disease", "severe_disease"))	
qol<-qol[order(qol$ID), ]	

# Remove ID (col=1) # the clinical Diagnosis (col=41) will be handled later	
qol <- qol[ , -1]	

# 80-20%  training-testing data split	
set.seed(1234)	
train_index <- sample(seq_len(nrow(qol)), size = 0.8*nrow(qol))	
qol_train<-qol[train_index, ]	
qol_test<-qol[-train_index, ]	

library(C50)	
set.seed(1234)	
qol_model <- C5.0(qol_train[,-c(40, 41)], qol_train$cd)	

# Below are the (probability) results of the C5.0 classification tree model prediction:
pred_prob<-predict(qol_model, qol_test, type="prob")
head(pred_prob)
pred_tree<-predict(qol_model, qol_test)
head(pred_tree)
head(stats::ftable(pred_tree))
#table(pred_tree)

# 2 Evaluation strategies
# 2.1 Binary outcomes
#        actual Positive Negative
# pred Positive	TP		   FP(err1)
#      Negative	FN(err2) TN

# sensitivity = TP/(TP+FN)
# specificity = TN/(TN+FP)

# precision = TP/(TP+FP)
# recall 	  = TP/(TP+FN)

# t1_err = 1-precision = 1-TP/(TP+FP) = FP/(TP+FP)
# t2_err = 1-recall = 1-TP/(TP+FN) = FN/(TP+FN)

# 2.2 Confusion Matrices
hn_test_pred<-predict(hn_classifier, hn_test)
table(hn_test_pred, hn_med_test$stage)
library(gmodels)
CrossTable(hn_test_pred, hn_med_test$stage)

# Using either table (CrossTable, confusionMatrix), we can calculate accuracy and error rate by hand.
accuracy<-(73+0)/100
accuracy
error_rate<-(23+4)/100
error_rate
1-accuracy

# 2.3 Other Measures of Performance Beyond Accuracy
library(caret)
qol_pred<-predict(qol_model, qol_test)
confusionMatrix(table(qol_pred, qol_test$cd), positive="severe_disease")
confusionMatrix(factor(qol_pred, levels = c("severe_disease", "minor_disease")), 
                 factor(qol_test$cd, levels = c("severe_disease", "minor_disease")),
                 positive = "severe_disease")


# 2.3.2 The Kappa (k) Statistic
table(qol_pred, qol_test$cd)
A=143; B=71; C=72; D=157
# A+B+ C+D # 443
# ((A+B)*(A+C)+(C+D)*(B+D))/(A+B+C+D)   # 221.7201
EA=((A+B)*(A+C)+(C+D)*(B+D))/(A+B+C+D) # Expected accuracy
OA=A+D; OA    # Observed accuracy
k=(OA-EA)/(A+B+C+D - EA); k # 0.3537597
# Compare against the official kappa score
confusionMatrix(table(qol_pred, qol_test$cd), positive="severe_disease")$overall[1] # report official Accuracy
round(confusionMatrix(table(qol_pred, qol_test$cd), positive="severe_disease")$overall[2], 2) # report official Kappa

# install.packages(vcd)
library(vcd)
Kappa(table(qol_pred, qol_test$cd))
Kappa(table(qol_pred, qol_test$cd),weights = matrix(c(1,10,1,10),nrow=2))
#table(qol_pred, qol_test$cd)
table(factor(qol_pred, levels = c("severe_disease", "minor_disease")), 
     factor(qol_test$cd, levels = c("severe_disease", "minor_disease")))

# 2.3.3 Sensitivity and Specificity
sens<-157/(157+71)
sens
spec<-143/(143+72)
spec
library(caret)
sensitivity(qol_pred, qol_test$cd, positive="severe_disease")
sensitivity(factor(qol_pred, levels = c("severe_disease", "minor_disease")),
            factor(qol_test$cd,levels = c("severe_disease", "minor_disease")))
specificity(factor(qol_pred, levels = c("severe_disease", "minor_disease")),
            factor(qol_test$cd,levels = c("severe_disease", "minor_disease")))
confusionMatrix(table(qol_pred, qol_test$cd), positive="severe_disease")$byClass[1] # another way to report the sensitivity
confusionMatrix(table(qol_pred, qol_test$cd), positive="severe_disease")$byClass[2] # another way to report the specificity

# 2.3.4 Precision and Recall
prec<-157/(157+72)
prec
recall<-157/(157+71)
recall

library (ROCR)
library(plotly)
qol_pred <- predict(qol_model, qol_test)
qol_pred <- predict(qol_model, qol_test, type = 'prob')

pred <- prediction(qol_pred[,2], qol_test$cd)
PrecRec <- performance(pred, "prec", "rec")
plot(PrecRec)

plot_ly(x = ~PrecRec@x.values[[1]][2:length(PrecRec@x.values[[1]])], 
        y = ~PrecRec@y.values[[1]][2:length(PrecRec@y.values[[1]])], 
        name = 'Recall-Precision relation', type='scatter', mode='markers+lines') %>%
  layout(title=paste0("Precision-Recall Plot, AUC=",
                      round(as.numeric(PrecRec@y.values[[1]]), 2)),
         xaxis=list(title="Recall"), yaxis=list(title="Precision"))

PrecRec <- performance(pred, "auc")
paste0("AUC=", round(as.numeric(PrecRec@y.values), 2))

# Another way to obtain precision would be posPredValue() under caret package. 
# Remember to specify which one is the “success” class.
qol_pred<-predict(qol_model, qol_test)
posPredValue(qol_pred, qol_test$cd, positive="severe_disease")

error1<-1-prec
error1
error2<-1-recall
error2

# t1_err = 1-precision = 1-TP/(TP+FP) = FP/(TP+FP)
# err1 = 72/(157+72)
# t2_err = 1-recall = 1-TP/(TP+FN) = FN/(TP+FN)
# err2 = 71/(157+71)

# 2.3.5 The F-Measure
f1<-(2*prec*recall)/(prec+recall)
f1
precision <- posPredValue(qol_pred, qol_test$cd, positive="severe_disease")
recall <- sensitivity(qol_pred, qol_test$cd, positive="severe_disease")
F1 <- (2 * precision * recall) / (precision + recall)
F1

# 3 Visualizing Performance Tradeoffs (ROC Curve)
# install.packages("ROCR")
library(ROCR)
pred<-ROCR::prediction(predictions=pred_prob[, 2], labels=qol_test$cd) 
# avoid naming collision (ROCR::prediction), as
# there is another prediction function in the neuralnet package.
# curve(log(x), from=0, to=100, xlab="False Positive Rate", ylab="True Positive Rate", main="ROC curve", col="green", lwd=3, axes=F)
# Axis(side=1, at=c(0, 20, 40, 60, 80, 100), labels = c("0%", "20%", "40%", "60%", "80%", "100%"))
# Axis(side=2, at=0:5, labels = c("0%", "20%", "40%", "60%", "80%", "100%"))
# segments(0, 0, 110, 5, lty=2, lwd=3)
# segments(0, 0, 0, 4.7, lty=2, lwd=3, col="blue")
# segments(0, 4.7, 107, 4.7, lty=2, lwd=3, col="blue")
# text(20, 4, col="blue", labels = "Perfect Classifier")
# text(40, 3, col="green", labels = "Test Classifier")
# text(70, 2, col="black", labels= "Classifier with no predictive value")

roc<-performance(pred, measure="tpr", x.measure="fpr")
plot(roc, main="ROC curve for Quality of Life Model", col="blue", lwd=3)
segments(0, 0, 1, 1, lty=2)
roc_auc<-performance(pred, measure="auc")
str(roc_auc)
roc_auc@y.values

plot_ly(x = ~roc@x.values[[1]], y = ~roc@y.values[[1]], 
        name = 'ROC Curve', type='scatter', mode='markers+lines') %>%
  add_lines(x=c(0,1), y=c(0,1), line=list(color="black", dash='dash'),
            name="Classifier with no predictive value") %>%
  layout(title="ROC Curve for Quality of Life C5.0 classification Tree Model", 
         legend = list(orientation = 'h'),
         xaxis=list(title="False Positive Rate", scaleanchor="y", range=c(0,1)), 
         yaxis=list(title="True Positive Rate", scaleanchor="x"),
         annotations = list(text=paste0("AUC=",
                                        round(as.numeric(performance(pred, "auc")@y.values[[1]]), 2)),  
                            x=0.6, y=0.4, textangle=0,
                            font=list(size=15, color="blue", showarrow=FALSE)))


library(pROC)
qol_pred <- predict(qol_model, qol_test, type = 'prob')
r <- roc(qol_test$cd, qol_pred[,2])
plot.roc(r)
auc(r)

r$sensitivities
r$specificities
r$thresholds
#cbind(r$sensitivities, r$specificities, r$thresholds)
plot(1-r$specificities, r$sensitivities)

 #compare with
 #roc@y.name
# roc@y.values
# roc@x.name
# roc@x.values
# roc@alpha.name
# roc@alpha.values

# 4 Estimating future performance (internal statistical validation)
# 4.1 The Holdout Method
google<-read.csv("https://umich.instructure.com/files/416274/download?download_frd=1", stringsAsFactors = F)	
google<-google[, -c(1, 2)]	
normalize <- function(x) {	
  return((x - min(x)) / (max(x) - min(x)))	
}	
google_norm<-as.data.frame(lapply(google, normalize))	

sub<-caret::createDataPartition(google_norm$RealEstate, p=0.75, list = F)
google_train<-google_norm[sub, ]
google_test<-google_norm[-sub, ]

sub<-sample(nrow(google_norm), floor(nrow(google_norm)*0.50))
google_train<-google_norm[sub, ]
google_test<-google_norm[-sub, ]
sub1<-sample(nrow(google_test), floor(nrow(google_test)*0.5))
google_test1<-google_test[sub1, ]
google_test2<-google_test[-sub1, ]
nrow(google_norm)
nrow(google_train) # training
nrow(google_test1) # testing: internal cross validation
nrow(google_test2) # testing: out of bag validation

# 4.2 Cross-Validation
library("caret")
set.seed(1234)
folds<-createFolds(google_norm$RealEstate, k=10)
str(folds)

# install.packages("sparsediscrim")
library(sparsediscrim)
folds2 = cv_partition(1:nrow(google_norm), num_folds=10)
str(folds2)

library(neuralnet)
fold_cv<-lapply(folds, function(x){
  google_train<-google_norm[-x, ]
  google_test<-google_norm[x, ]
  google_model<-neuralnet(RealEstate~Unemployment+Rental+Mortgage+Jobs+Investing+DJI_Index+StdDJI, data=google_train)
  google_pred<-compute(google_model, google_test[, c(1:2, 4:8)])
  pred_results<-google_pred$net.result
  pred_cor<-cor(google_test$RealEstate, pred_results)
  return(pred_cor)
})
str(fold_cv)
mean(unlist(fold_cv))

# 4.3 Bootstrap Sampling
# define the total data size
n <- 500
# define number of Bootstrap iterations
N=2000
#define the resampling function and compute the proportion of uniquely selected elements
uniqueProportions <- function(myDataSet, sampleSize){
  indices <- sample(1:sampleSize, sampleSize, replace=TRUE) # sample With Replacement
  length(unique(indices))/sampleSize
}
# compute the N proportions of unique elements (could also use a for loop)
proportionsVector <- c(lapply(1:N, uniqueProportions, sampleSize=n), recursive=TRUE)
# report the Expected (mean) proportion of unique elements in the bootstraping samples of n
mean(proportionsVector)

# Recall: qol_model <- C5.0(qol_train[,-c(40, 41)], qol_train$cd)
# predict lables of testing data
qol_pred<-predict(qol_model, qol_test)
# compute matches and mismatches of Predicted and Observed class labels
predObsEqual <- qol_pred == qol_test$cd
predObsTF <- c(table(predObsEqual)[1], table(predObsEqual)[2]) 
predObsTF

# training error rate
train.err <- as.numeric(predObsTF[1]/(predObsTF[1]+predObsTF[2]))

# testing error rate, leave-one-out Bootstrap (LOOB) Cross-Validation
B <- 10 
loob.err <- NULL
N <- dim(qol_test)[1]   # size of test-dataset
for (b in 1:B) {
  bootIndices <- sample(1:N, N*0.9, replace=T)
  train <- qol_test[bootIndices, ]
  qol_modelBS <- C5.0(train[,-c(40, 41)], train$cd)
  inner.err <- NULL
  
  # for current iteration extract the apporopriate tetsing cases for testing
  i <- (1:length(bootIndices))
  i <- i[is.na(match(i, bootIndices))]
  test <- qol_test[i, ]
  
  # predict using model at current iteration
  qol_modelBS_pred <- predict(qol_modelBS, test)
  
  predObsEqual <- qol_modelBS_pred == test$cd
  predObsTF <- c(table(predObsEqual)[1], table(predObsEqual)[2]); predObsTF
  # training error rate
  inner.err <- as.numeric(predObsTF[1]/(predObsTF[1]+predObsTF[2]))
  loob.err <- c(loob.err, mean(inner.err))
}

test.err <- ifelse(is.null(loob.err), NA, mean(loob.err))

# 0.632 Bootstrap error
boot.632 <- 0.368 * train.err + 0.632 * test.err; boot.632
