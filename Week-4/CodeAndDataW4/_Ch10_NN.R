# 2 Case Study 1: Google Trends and the Stock Market: Regression
# 2.1 Step 1: Collecting Data
# data file: CaseStudy13_GoogleTrends_Markets_Data.csv

# 2.2 Step 2: Exploring and Preparing the Data
google<-read.csv("https://umich.instructure.com/files/416274/download?download_frd=1", stringsAsFactors = F)
google<-google[, -c(1, 2)]
str(google)

normalize <- function(x) {
  return((x - min(x)) / (max(x) - min(x)))
}
google_norm<-as.data.frame(lapply(google, normalize))
summary(google_norm$RealEstate)

sub<-sample(nrow(google_norm), floor(nrow(google_norm)*0.75))
google_train<-google_norm[sub, ]
google_test<-google_norm[-sub, ]

# 2.3 Step 3: Training a Model on the Data
# install.packages("neuralnet")
library(neuralnet)
google_model<-neuralnet(RealEstate~Unemployment+Rental+Mortgage+Jobs+Investing+DJI_Index+StdDJI, data=google_train)
plot(google_model)

# 2.4 Step 4: Evaluating Model Performance
google_pred<-compute(google_model, google_test[, c(1:2, 4:8)])
pred_results<-google_pred$net.result
cor(pred_results, google_test$RealEstate)

# 2.5 Step 5: Improving Model Performance
google_model2<-neuralnet(RealEstate~Unemployment+Rental+Mortgage+Jobs+Investing+DJI_Index+StdDJI, data=google_train, hidden = 4)
plot(google_model2)

google_pred2<-compute(google_model2, google_test[, c(1:2, 4:8)])
pred_results2<-google_pred2$net.result
cor(pred_results2, google_test$RealEstate)

library(plotly)
plot_ly() %>%
  add_markers(x=pred_results2, y=google_test$RealEstate, 
              name="Data Scatter", type="scatter", mode="markers") %>%
  add_trace(x = c(0,1), y = c(0,1), type="scatter", mode="lines",
            line = list(width = 4), name="Ideal Agreement") %>%
  layout(title=paste0('Scatterplot (Normalized) Observed vs. Predicted Real Estate Values, Cor(Obs,Pred)=',
                      round(cor(pred_results2, google_test$RealEstate), 2)),
         xaxis = list(title="NN (hidden=4) Real Estate Predictions"),
         yaxis = list(title="(Normalized) Observed Real Estate"),
         legend = list(orientation = 'h'))

# 2.6 Step 6: Adding Additional Layers
google_model2<-neuralnet(RealEstate~Unemployment+Rental+Mortgage+Jobs+Investing+DJI_Index+StdDJI, data=google_train, hidden = c(4,3,3))
google_pred2<-compute(google_model2, google_test[, c(1:2, 4:8)])
pred_results2<-google_pred2$net.result
cor(pred_results2, google_test$RealEstate)

# plot(google_model2)
plot_ly() %>%
  add_markers(x=pred_results2, y=google_test$RealEstate, 
              name="Data Scatter", type="scatter", mode="markers") %>%
  add_trace(x = c(0,1), y = c(0,1), type="scatter", mode="lines",
            line = list(width = 4), name="Ideal Agreement") %>%
  layout(title=paste0('Scatterplot (Normalized) Observed vs. Predicted Real Estate Values, Cor(Obs,Pred)=',
                      round(cor(pred_results2, google_test$RealEstate), 2)),
         xaxis = list(title="NN (hidden=(4,3,3)) Real Estate Predictions"),
         yaxis = list(title="(Normalized) Observed Real Estate"),
         legend = list(orientation = 'h'))

# 4 Case Study 2: Google Trends and the Stock Market - Classification
google_class = google_norm
id1 = which(google_class$RealEstate>quantile(google_class$RealEstate,0.75))
id2 = which(google_class$RealEstate<quantile(google_class$RealEstate,0.25))
id3 = setdiff(1:nrow(google_class),union(id1,id2))
google_class$RealEstate[id1]=0
google_class$RealEstate[id2]=2
google_class$RealEstate[id3]=1
summary(as.factor(google_class$RealEstate))

set.seed(2017)
train = sample(1:nrow(google_class),0.7*nrow(google_class))
google_tr = google_class[train,]
google_ts = google_class[-train,]
train_x = google_tr[,c(1:2,4:8)]
train_y = google_tr[,3]
colnames(train_x)
test_x = google_ts[,c(1:2,4:8)]
test_y = google_ts[3]
train_y_ind = model.matrix(~factor(train_y)-1)
colnames(train_y_ind) = c("High","Median","Low")
train = cbind(train_x, train_y_ind)

set.seed(2017)
nn_single = neuralnet(High+Median+Low~Unemployment+Rental+Mortgage+Jobs+Investing+DJI_Index+StdDJI,
            data = train,
            hidden=4,
            linear.output=FALSE,
            lifesign='full', lifesign.step=5000)
pred = function(nn, dat) {
  # compute uses the trained neural net (nn=nn_single), and 
  # new testing data (dat=google_ts) to generate predictions (y_hat)
  # compute returns a list containing: 
  #     (1) neurons: a list of the neurons' output for each layer of the neural network, and
  #     (2) net.result: a matrix containing the overall result of the neural network.
  yhat = compute(nn, dat)$net.result
  
  # find the maximum in each row (1) in the net.result matrix
  # to determine the first occurrence of a specific element in each row (1)
  # we can use the apply function with which.max
  yhat = apply(yhat, 1, which.max)-1
  return(yhat)
}

mean(pred(nn_single, google_ts[,c(1:2,4:8)]) != as.factor(google_ts[,3]))
table(pred(nn_single, google_ts[,c(1:2,4:8)]), as.factor(google_ts[,3]))
plot(nn_single)

# multiple hidden layers
nn_single = neuralnet(High+Median+Low~Unemployment+Rental+Mortgage+Jobs+Investing+DJI_Index+StdDJI,
            data = train,
            hidden=c(4,5),
            linear.output=FALSE,
            lifesign='full', lifesign.step=5000)
mean(pred(nn_single, google_ts[,c(1:2,4:8)]) != as.factor(google_ts[,3]))

