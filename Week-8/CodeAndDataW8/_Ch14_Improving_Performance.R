# 1 Improving model performance by parameter tuning

# 2 Using caret for Automated Parameter Tuning
boystown<-read.csv("https://umich.instructure.com/files/399119/download?download_frd=1", sep=" ")
boystown$sex<-boystown$sex-1
boystown$dadjob<--1*(boystown$dadjob-2)
boystown$momjob<--1*(boystown$momjob-2)
boystown<-boystown[, -1]
table(boystown$gpa)

boystown$grade<-boystown$gpa %in% c(3, 4, 5)
boystown$grade<-factor(boystown$grade, levels=c(F, T), labels = c("above_avg", "avg_or_below"))
normalize<-function(x){
  return((x-min(x))/(max(x)-min(x)))
}
boystown_n<-as.data.frame(lapply(boystown[, -11], normalize))

str(boystown_n)
boystown_n<-cbind(boystown_n, boystown[, 11])
str(boystown_n)
colnames(boystown_n)[11]<-"grade"

library(caret)
set.seed(123)
kNN_mod <- train(grade~., data=boystown_n, method="knn")
kNN_mod
summary(kNN_mod)

set.seed(1234)
pred <- predict(kNN_mod, boystown_n)
table(pred, boystown_n$grade)
head(predict(kNN_mod, boystown_n, type = "prob"))

# 3 Customizing the Tuning Process
ctrl<-trainControl(method = "boot632", number=25, selectionFunction = "oneSE")
grid<-expand.grid(k=c(1, 3, 5, 7, 9)) 
# Creates a data frame from all combinations of the supplied factors

set.seed(123)
kNN_mod2 <-train(grade ~ ., data=boystown_n, method="knn", 
                 metric="Kappa", 
                 trControl=ctrl, 
                 tuneGrid=grid)
kNN_mod2

# 4 Improving model performance with meta-learning
# 4.1 Bagging
qol<-read.csv("https://umich.instructure.com/files/481332/download?download_frd=1")
qol<-qol[!qol$CHARLSONSCORE==-9 , -c(1, 2)]
qol$CHARLSONSCORE<-as.factor(qol$CHARLSONSCORE)

# install.packages("ipred")
library(ipred)
set.seed(123)
mybag<-bagging(CHARLSONSCORE ~ ., data=qol, nbagg=25) # default may be 25

bt_pred<-predict(mybag, qol)
agreement<-bt_pred==qol$CHARLSONSCORE
prop.table(table(agreement))
caret::confusionMatrix(bt_pred,qol$CHARLSONSCORE)

library(caret)
set.seed(123)
ctrl<-trainControl(method="repeatedcv", number = 10, repeats = 10)
train(CHARLSONSCORE~., data=as.data.frame(qol), method="treebag", trControl=ctrl)

set.seed(123)
ctrl<-trainControl(method="repeatedcv", number = 10, repeats = 10)
train(as.factor(QOL_Q_01) ~ . , data=as.data.frame(qol), method="treebag", trControl=ctrl)

#str(svmBag)
#svmBag$fit

# 4.2 Boosting

# 4.3 Random Forests
# 4.3.1 Training Random Forests
# install.packages("randomForest")
library(randomForest)
set.seed(123)
rf<-randomForest(as.factor(QOL_Q_01) ~ . , data=qol)
rf

# 4.3.2 Evaluating Random Forest Performance
library(caret)
ctrl<-trainControl(method="cv", number=10)
grid_rf<-expand.grid(mtry=c(2, 4, 8, 16))

set.seed(123)
m_rf <- train(as.factor(QOL_Q_01) ~ ., data = qol, method = "rf", 
              metric = "Kappa", trControl = ctrl, tuneGrid = grid_rf)
m_rf

# 4.4 Adaptive Boosting
# Prep the data
qol <- read.csv("https://umich.instructure.com/files/481332/download?download_frd=1")
qol<-qol[!qol$CHARLSONSCORE==-9 , -c(1, 2)]
qol$CHARLSONSCORE <- as.factor(qol$CHARLSONSCORE)
#qol$QOL_Q_01 <- as.factor(qol$QOL_Q_01)
qol<-qol[!qol$CHARLSONSCORE==-9 , -c(1, 2)]
qol$cd<-qol$CHRONICDISEASESCORE>1.497
qol$cd<-factor(qol$cd, levels=c(F, T), labels = c("minor_disease", "severe_disease"))
qol<-qol[!qol$CHRONICDISEASESCORE==-9, ]

# install.packages("ada"); install.packages("adabag")
library("ada"); library("adabag")
set.seed(123)
# qol$QOL_Q_01 <- as.factor(qol$QOL_Q_01)
# qol_boost <- boosting(QOL_Q_01 ~ . , data=qol, mfinal = 100, coeflearn = 'Breiman')
# mean(qol_boost$class==qol$QOL_Q_01)
qol_boost <- boosting(cd ~ . , data=qol[, -37], mfinal = 100, coeflearn = 'Breiman')
mean(qol_boost$class==qol$cd)

set.seed(123)
# qol_boost <- boosting(QOL_Q_01 ~ . , data=qol, mfinal = 100, coeflearn = 'Freund')
# mean(qol_boost$class==qol$QOL_Q_01)
qol_boost <- boosting(cd ~ . , data=qol[, -37], mfinal = 100, coeflearn = 'Freund')
mean(qol_boost$class==qol$cd)

set.seed(1234)
# qol_boost <- boosting(QOL_Q_01 ~ ., data=qol, mfinal = 100, coeflearn = 'Zhu')
# mean(qol_boost$class==qol$QOL_Q_01)
qol_boost <- boosting(cd ~ . , data=qol[, -37], mfinal = 100, coeflearn = 'Zhu')
mean(qol_boost$class==qol$cd)


# install.packages("ada"); install.packages("adabag")
library("ada"); library("adabag")
qol_boost <- boosting(CHARLSONSCORE~.,data=qol, mfinal = 100, coeflearn = 'Breiman')
mean(qol_boost$class==qol$CHARLSONSCORE)
qol_boost <- boosting(CHARLSONSCORE~.,data=qol, mfinal = 100, coeflearn = 'Freund')
mean(qol_boost$class==qol$CHARLSONSCORE)
qol_boost <- boosting(CHARLSONSCORE~.,data=qol, mfinal = 100, coeflearn = 'Zhu')
mean(qol_boost$class==qol$CHARLSONSCORE)


# 5 Comparing the performance of several alternative models
# install.packages("fastAdaboost")
#library(fastAdaboost)
library(caret)    # for modeling
library(lattice)  # for plotting
control <- trainControl(method="repeatedcv", number=10, repeats=3)

## Run all subsequent models in parallel
library(doParallel)
cl <- makePSOCKcluster(5)
registerDoParallel(cl)
system.time({
  rpart.fit     <- train(cd~., data=qol[, -37], method="rpart", trControl=control);
  rf.fit        <- train(cd~., data=qol[, -37], method="rf", trControl=control);
  knn.fit       <- train(cd~., data=qol[, -37], method="knn", trControl=control);
  adabag.fit    <- train(cd~., data=qol[, -37], method="AdaBag", trControl=control);
  adaboost.fit  <- train(cd~., data=qol[, -37], method="AdaBoost.M1", trControl=control)
})

stopCluster(cl) # close multi-core cluster
rm(cl)

results <- resamples(list(rpart=rpart.fit, RF=rf.fit, kNN=knn.fit, Bag=adabag.fit, Boost=adaboost.fit))

# summary of model differences
summary(results)

# Plot Accuracy Summaries
scales <- list(x=list(relation="free"), y=list(relation="free"))
bwplot(results, scales=scales)                 # Box plots of accuracy

# Convert (results) data-frame from wide to long format
# The arguments to gather():
# - data: Data object
# - key: Name of new key column (made from names of data columns)
# - value: Name of new value column
# - ...: Names of source columns that contain values
# - factor_key: Treat the new key column as a factor (instead of character vector)
library(tidyr)
results_long <- gather(results$values[, -1], method, measurement, factor_key=TRUE) %>%
  separate(method, c("Technique", "Metric"), sep = "~")

# Compare original wide format to transformed long format
results$values[, -1]
head(results_long)

library(plotly)
plot_ly(results_long, x=~Technique, y = ~measurement, color = ~Metric, type = "box")

densityplot(results, scales=scales, pch = "|") # Density plots of accuracy

densityModels <- with(results_long[which(results_long$Metric=='Accuracy'), ],
                      tapply(measurement, INDEX = Technique, density))
df <- data.frame(
  x = unlist(lapply(densityModels, "[[", "x")),
  y = unlist(lapply(densityModels, "[[", "y")),
  method = rep(names(densityModels), each = length(densityModels[[1]]$x))
)

plot_ly(df, x = ~x, y = ~y, color = ~method) %>% add_lines() %>%
  layout(title="Performance Density Plots (Accuracy)", legend = list(orientation='h'),
         xaxis=list(title="Accuracy"), yaxis=list(title="Density"))

densityModels <- with(results_long[which(results_long$Metric=='Kappa'), ],
                      tapply(measurement, INDEX = Technique, density))
df <- data.frame(
  x = unlist(lapply(densityModels, "[[", "x")),
  y = unlist(lapply(densityModels, "[[", "y")),
  method = rep(names(densityModels), each = length(densityModels[[1]]$x))
)

plot_ly(df, x = ~x, y = ~y, color = ~method) %>% add_lines() %>%
  layout(title="Performance Density Plots (Kappa)", legend = list(orientation='h'),
         xaxis=list(title="Kappa"), yaxis=list(title="Density"))

dotplot(results, scales=scales)                # Dot plots of Accuracy & Kappa

splom(results)      # contrast pair-wise model scatterplots of prediction accuracy (Trellis Scatterplot matrices)

# Pairs - Accuracy
results_wide <- results_long[which(results_long$Metric=='Accuracy'), -2] %>%
  pivot_wider(names_from = Technique, values_from = measurement)

df = data.frame(cbind(rpart=results_wide$rpart[[1]],
                      RF=results_wide$RF[[1]], 
                      kNN=results_wide$kNN[[1]], 
                      Bag=results_wide$Bag[[1]], 
                      Boost=results_wide$Boost[[1]]))

dims <- dplyr::select_if(df, is.numeric)
dims <- purrr::map2(dims, names(dims), ~list(values=.x, label=.y))
plot_ly(type = "splom", dimensions = setNames(dims, NULL), 
        showupperhalf = FALSE, diagonal = list(visible = FALSE)) %>%
  layout(title="Performance Pairs Plot (Accuracy)")

# Pairs - Kappa
results_wide <- results_long[which(results_long$Metric=='Kappa'), -2] %>%
  pivot_wider(names_from = Technique, values_from = measurement)

df = data.frame(cbind(rpart=results_wide$rpart[[1]],
                      RF=results_wide$RF[[1]],
                      kNN=results_wide$kNN[[1]], 
                      Bag=results_wide$Bag[[1]], 
                      Boost=results_wide$Boost[[1]]))

dims <- dplyr::select_if(df, is.numeric)
dims <- purrr::map2(dims, names(dims), ~list(values=.x, label=.y))
plot_ly(type = "splom", dimensions = setNames(dims, NULL), 
        showupperhalf = FALSE, diagonal = list(visible = FALSE)) %>%
  layout(title="Performance Pairs Plot (Kappa)")
