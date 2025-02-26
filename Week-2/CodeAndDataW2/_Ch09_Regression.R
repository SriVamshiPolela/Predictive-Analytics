# 2.1 Simple Linear Regression
library(plotly)
heart_attack<-read.csv("https://umich.instructure.com/files/1644953/download?download_frd=1", stringsAsFactors = F)
heart_attack$CHARGES<-as.numeric(heart_attack$CHARGES)
heart_attack<-heart_attack[complete.cases(heart_attack), ]

fit1<-lm(CHARGES~LOS, data=heart_attack)
par(cex=.8)
plot(heart_attack$LOS, heart_attack$CHARGES, xlab="LOS", ylab = "CHARGES")
abline(fit1, lwd=2, col="red")

plot_ly(heart_attack, x = ~LOS, y = ~CHARGES, type = 'scatter', mode = "markers", name="Data") %>%
  add_trace(x=~mean(LOS), y=~mean(CHARGES), type="scatter", mode="markers",
            name="(mean(LOS), mean(Charge))", marker=list(size=20, color='blue', line=list(color='yellow', width=2))) %>%
  add_lines(x = ~LOS, y = fit1$fitted.values, mode = "lines", name="Linear Model") %>%
  layout(title=paste0("lm(CHARGES ~ LOS), Cor(LOS,CHARGES) = ", 
                      round(cor(heart_attack$LOS, heart_attack$CHARGES),3)))

# 2.2 Ordinary Least Squares Estimation
b<-cov(heart_attack$LOS, heart_attack$CHARGES)/var(heart_attack$LOS)
b
a<-mean(heart_attack$CHARGES)-b*mean(heart_attack$LOS)
a
# compare to the lm() estimate:
fit1$coefficients[1]
# we can do the same for the slope parameter b==
fit1$coefficients[2]

# 2.4 Correlations
r<-cov(heart_attack$LOS, heart_attack$CHARGES)/(sd(heart_attack$LOS)*sd(heart_attack$CHARGES))
r
cor(heart_attack$LOS, heart_attack$CHARGES)

# 2.5 Multiple Linear Regression
reg<-function(y, x){
  x<-as.matrix(x)
  x<-cbind(Intercept=1, x)
  solve(t(x)%*%x)%*%t(x)%*%y
}
reg(y=heart_attack$CHARGES, x=heart_attack$LOS)
# and compare the result to lm()
fit1

str(heart_attack)
reg(y=heart_attack$CHARGES, x=heart_attack[, c(7, 8)])
# and compare the result to lm()
fit2<-lm(CHARGES ~ LOS+AGE, data=heart_attack)
fit2

# 3 Case Study 1: Baseball Players
# 3.1 Step 1: Collecting Data
# data file: 01a_data.txt

# 3.2 Step 2: Exploring and Preparing the Data
mlb<- read.table('https://umich.instructure.com/files/330381/download?download_frd=1', as.is=T, header=T)
str(mlb)
mlb<-mlb[, -1]
mlb$Team<-as.factor(mlb$Team)
mlb$Position<-as.factor(mlb$Position)

summary(mlb$Weight)
hist(mlb$Weight, main = "Histogram for Weights")
plot_ly(x = mlb$Weight, type = "histogram", name= "Histogram for Weights") %>%
  layout(title="Baseball Players' Weight Histogram", bargap=0.1,
         xaxis=list(title="Weight"),  # control the y:x axes aspect ratio
         yaxis = list(title="Frequency"))

# require(GGally)
# mlb_binary = mlb
# mlb_binary$bi_weight = as.factor(ifelse(mlb_binary$Weight>median(mlb_binary$Weight),1,0))
# g_weight <- ggpairs(data=mlb_binary[-1], title="MLB Light/Heavy Weights",
#             mapping=ggplot2::aes(colour = bi_weight),
#             lower=list(combo=wrap("facethist",binwidth=1)))
#             # upper = list(continuous = wrap("cor", size = 4.75, alignPercent = 1))
# g_weight

plot_ly(mlb) %>%
  add_trace(type = 'splom', dimensions = list( list(label='Position', values=~Position), 
                                               list(label='Height', values=~Height), list(label='Weight', values=~Weight), 
                                               list(label='Age', values=~Age), list(label='Team', values=~Team)),
            text=~Team,
            marker = list(color = as.integer(mlb$Team),
                          size = 7, line = list(width = 1, color = 'rgb(230,230,230)')
            )
  ) %>%
  layout(title= 'MLB Pairs Plot', hovermode='closest', dragmode= 'select',
         plot_bgcolor='rgba(240,240,240, 0.95)')

# g_position <- ggpairs(data=mlb[-1], title="MLB by Position",
#               mapping=ggplot2::aes(colour = Position),
#               lower=list(combo=wrap("facethist",binwidth=1)))
# g_position

table(mlb$Team)
table(mlb$Position)
summary(mlb$Height)
summary(mlb$Age)

# 3.3 Exploring Relationships Among Features: The Correlation Matrix
cor(mlb[c("Weight", "Height", "Age")])
car::vif(lm(Weight ~ Height + Age, data=mlb))

# 3.5 Visualizing Relationships Among Features: The Scatterplot Matrix
pairs(mlb[c("Weight", "Height", "Age")])

plot_ly(mlb) %>%
  add_trace(type = 'splom', dimensions = list( list(label='Height', values=~Height),
                                               list(label='Weight', values=~Weight), list(label='Age', values=~Age)),
            text=~Position,
            marker = list(color = as.integer(mlb$Team),
                          size = 7, line = list(width = 1, color = 'rgb(230,230,230)')
            )
  ) %>%
  layout(title= 'MLB Pairs Plot', hovermode='closest', dragmode= 'select',
         plot_bgcolor='rgba(240,240,240, 0.95)')

# install.packages("psych")
library(psych)
pairs.panels(mlb[, c("Weight", "Height", "Age")])

# 3.6 Step 3: Training a Model on the Data
fit<-lm(Weight~., data=mlb)
fit

# 3.7 Step 4: Evaluating Model Performance
summary(fit)
#plot(fit, which = 1:2)

plot_ly(x=fit$fitted.values, y=fit$residuals, type="scatter", mode="markers") %>%
  layout(title="LM: Fitted-values vs. Model-Residuals",
         xaxis=list(title="Fitted"), 
         yaxis = list(title="Residuals"))

# 3.8 Step 5: Improving Model Performance
step(fit,direction = "backward")
step(fit,direction = "forward") # this is incorrect
step(fit,direction = "both")

# step forward from a model with no predictor to a model with all predictors
fit.null <- lm(Weight~1, data=mlb)
fit.full <- lm(Weight~., data=mlb)
step(fit.null, scope=list(lower=fit.null, upper=fit.full), direction = "forward")

step(fit,k=2)
step(fit,k=log(nrow(mlb)))
fit2 = step(fit,k=2,direction = "backward")
summary(fit2)

#plot(fit2, which = 1:2)
plot_ly(x=fit2$fitted.values, y=fit2$residuals, type="scatter", mode="markers") %>%
  layout(title="LM: Fitted-values vs. Model-Residuals",
         xaxis=list(title="Fitted"), 
         yaxis = list(title="Residuals"))

# 3.8.1 Model Specification: Adding Non-linear Relationships
mlb$age2<-(mlb$Age)^2
fit2<-lm(Weight~., data=mlb)
#fit2<-lm(Weight~Team+Position+Height+Age+I(Age^2), data=mlb)
summary(fit2)

# 3.9 Transformation: Converting a Numeric Variable to a Binary Indicator
mlb$age30<-ifelse(mlb$Age>=30, 1, 0)
fit3<-lm(Weight~Team+Position+Age+age30+Height, data=mlb)
summary(fit3)

# 3.10 Model Specification: Adding Interaction Effects
fit4<-lm(Weight~Team+Height+Age*Position+age2, data=mlb)
summary(fit4)

# 4 Understanding Regression Trees and Model Trees
# 4.2 Adding Regression to Trees
ori<-c(1, 2, 3, 3, 4, 5, 6, 6, 7, 8)
at1<-c(1, 2, 3)
at2<-c(3, 4, 5, 6, 6, 7, 8)
bt1<-c(1, 2, 3, 3, 4, 5)
bt2<-c(6, 6, 7, 8)
sdr_a<-sd(ori)-(length(at1)/length(ori)*sd(at1)+length(at2)/length(ori)*sd(at2))
sdr_b<-sd(ori)-(length(bt1)/length(ori)*sd(bt1)+length(bt2)/length(ori)*sd(bt2))
sdr_a
sdr_b

# Using bt1 and bt2 as terminal nodes
mean(bt1);mean(bt2)

# 6 Case Study 2: Baseball Players (Take 2)
# 6.1 Step 2: Exploring and Preparing the Data
set.seed(1234)
train_index <- sample(seq_len(nrow(mlb)), size = 0.75*nrow(mlb))
mlb_train<-mlb[train_index, ]
mlb_test<-mlb[-train_index, ]

# 6.2 Step 3: Training a Model On the Data
#install.packages("rpart")
library(rpart)
mlb.rpart<-rpart(Weight~Height+Age, data=mlb_train)
mlb.rpart

# 6.3 Visualizing Decision Trees
# install.packages("rpart.plot")
library(rpart.plot)
rpart.plot(mlb.rpart, digits=3)
rpart.plot(mlb.rpart, digits = 4, fallen.leaves = T, type=3, extra=101)
library(rattle)
fancyRpartPlot(mlb.rpart, cex = 0.8)

# 6.4 Step 4: Evaluating Model Performance
mlb.p<-predict(mlb.rpart, mlb_test)
summary(mlb.p)
summary(mlb_test$Weight)
cor(mlb.p, mlb_test$Weight)

# 6.5 Measuring Performance with Mean Absolute Error
MAE<-function(obs, pred){
  mean(abs(obs-pred))
}
MAE(mlb_test$Weight, mlb.p)
mean(mlb_test$Weight)
MAE(mlb_test$Weight, mean(mlb_test$Weight))

# 6.6 Step 5: Improving Model Performance
#install.packages("RWeka")

# Sometimes RWeka installations may be off a bit, see:
# http://stackoverflow.com/questions/41878226/using-rweka-m5p-in-rstudio-yields-java-lang-noclassdeffounderror-no-uib-cipr-ma

Sys.getenv("WEKA_HOME") # where does it point to? Maybe some obscure path? 
# if yes, correct the variable:
Sys.setenv(WEKA_HOME="C:\\MY\\PATH\\WEKA_WPM")
library(RWeka)
# WPM("list-packages", "installed")

mlb.m5 <- M5P(Weight~Height+Age, data=mlb_train)
mlb.m5
summary(mlb.m5)
mlb.p.m5<-predict(mlb.m5, mlb_test)
summary(mlb.p.m5)
cor(mlb.p.m5, mlb_test$Weight)
MAE(mlb_test$Weight, mlb.p.m5)

