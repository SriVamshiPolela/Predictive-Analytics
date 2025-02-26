# 3 Case Study - ALS
# 3.1 Step 1: Collecting Data
# datafile: ALS_TrainingData_2223.csv
# 3.2 Step 2: Exploring and preparing the data
ALS.train<-read.csv("https://umich.instructure.com/files/1789624/download?download_frd=1")
summary(ALS.train)

# 3.3 Step 3 - training a model on the data
# install.packages("Boruta")
library(Boruta)
set.seed(123)
# !!!!!!!takes around 3 mins!!!!!!
als<-Boruta(ALSFRS_slope~.-ID, data=ALS.train, doTrace=0)
print(als)

als$ImpHistory[1:6, 1:10]

plot(als, xlab="", xaxt="n")
lz<-lapply(1:ncol(als$ImpHistory), function(i)
  als$ImpHistory[is.finite(als$ImpHistory[, i]), i])
names(lz)<-colnames(als$ImpHistory)
lb<-sort(sapply(lz, median))
axis(side=1, las=2, labels=names(lb), at=1:ncol(als$ImpHistory), cex.axis=0.5, font = 4)

library(plotly)
df_long <- tidyr::gather(as.data.frame(als$ImpHistory), feature, measurement)

plot_ly(df_long, y = ~measurement, color = ~feature, type = "box") %>%
  layout(title="Box-and-whisker Plots across all 102 Features (ALS Data)",
         xaxis = list(title="Features"),
         yaxis = list(title="Importance"),
         showlegend=F)

final.als<-TentativeRoughFix(als)
print(final.als)
final.als$finalDecision

getConfirmedFormula(final.als)

# report the Boruta "Confirmed" & "Tentative" features, removing the "Rejected" ones
print(final.als$finalDecision[final.als$finalDecision %in% c("Confirmed", "Tentative")])

# how many are actually "confirmed" as important/salient?
impBoruta <- final.als$finalDecision[final.als$finalDecision %in% c("Confirmed")]; length(impBoruta)

# 3.4 Step 4 - evaluating model performance
# 3.4.1 Comparing with RFE
library(caret)
library(randomForest)
set.seed(123)
control<-rfeControl(functions = rfFuncs, method = "cv", number=10)

# !!!!!!!takes around 10 mins!!!!!!
rf.train<-rfe(ALS.train[, -c(1, 7)], ALS.train[, 7], sizes=c(10, 20, 30, 40), rfeControl=control)
rf.train

plot(rf.train, type=c("g", "o"), cex=1, col=1:5)

predRFE <- predictors(rf.train)
predBoruta <- getSelectedAttributes(final.als, withTentative = F)
intersect(predBoruta, predRFE)

# 3.4.2 Comparing with stepwise feature selection
data2 <- ALS.train[, -1]
# Define a base model - intercept only
base.mod <- lm(ALSFRS_slope ~ 1 , data= data2)
# Define the full model - including all predictors
all.mod <- lm(ALSFRS_slope ~ . , data= data2)
# ols_step <- lm(ALSFRS_slope ~ ., data=data2)
ols_step <- step(base.mod, scope = list(lower = base.mod, upper = all.mod), direction = 'both', k=2, trace = F)
summary(ols_step) 
#ols_step

# get the shortlisted variable
stepwiseConfirmedVars <- names(unlist(ols_step[[1]]))
# remove the intercept 
stepwiseConfirmedVars <- stepwiseConfirmedVars[!stepwiseConfirmedVars %in% "(Intercept)"]
print(stepwiseConfirmedVars)

library(mlbench)
library(caret)

# estimate variable importance
predStepwise <- varImp(ols_step, scale=FALSE)
# summarize importance
print(predStepwise)

# plot predStepwise
# plot(predStepwise)

# Boruta vs. Stepwise feataure selection
intersect(predBoruta, stepwiseConfirmedVars) 
