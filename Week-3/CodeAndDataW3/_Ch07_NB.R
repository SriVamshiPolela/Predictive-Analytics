# Play golf

#############
# load data
library(e1071)
weather.df <- read.csv("weather.csv")
weather_test.df <- read.csv("weather_test.csv")

# run naive bayes without laplace smoothing
weather.nb <- naiveBayes(Play.Golf ~ ., data = weather.df)
weather.nb
weather_pred.prob <- predict(weather.nb, newdata = weather_test.df, type = "raw")
weather_pred.class <- predict(weather.nb, newdata = weather_test.df)


# 5 Case Study: Head and Neck Cancer Medication

# 5.1 Step 1: Collecting Data
## datafile: CaseStudy14_HeadNeck_Cancer_Medication.csv

# 5.2 Step 2: Exploring and preparing the data
hn_med<-read.csv("https://umich.instructure.com/files/1614350/download?download_frd=1", stringsAsFactors = FALSE)
str(hn_med)
hn_med$seer_stage <- factor(hn_med$seer_stage)
str(hn_med$seer_stage)
table(hn_med$seer_stage)

# 5.2.1 Data preparation - processing text data for analysis
# install.packages("tm", repos = "http://cran.us.r-project.org")
# requires R V.3.3.1 +
library(tm)
hn_med_corpus<-Corpus(VectorSource(hn_med$MEDICATION_SUMMARY))
print(hn_med_corpus)
inspect(hn_med_corpus[1:3])
hn_med_corpus[[1]]$content
hn_med_corpus[[2]]$content
hn_med_corpus[[3]]$content
corpus_clean<-tm_map(hn_med_corpus, tolower)
corpus_clean<-tm_map(corpus_clean, removePunctuation)
#corpus_clean <- tm_map(corpus_clean, stripWhitespace)
corpus_clean <- tm_map(corpus_clean, removeNumbers)
corpus_clean <- tm_map(corpus_clean, stripWhitespace)
# corpus_clean <- tm_map(corpus_clean, PlainTextDocument)
inspect(corpus_clean[1:3])
corpus_clean[[1]]$content
corpus_clean[[2]]$content
corpus_clean[[3]]$content
hn_med_dtm<-DocumentTermMatrix(corpus_clean)

# 5.2.2 Data preparation - creating training and test datasets
set.seed(12345) 
subset_int <- sample(nrow(hn_med),floor(nrow(hn_med)*0.8)) # 80% training + 20% testing
hn_med_train<-hn_med[subset_int, ]
hn_med_test<-hn_med[-subset_int, ]
hn_med_dtm_train<-hn_med_dtm[subset_int, ]
hn_med_dtm_test<-hn_med_dtm[-subset_int, ]
corpus_train<-corpus_clean[subset_int]
corpus_test<-corpus_clean[-subset_int]
# hn_med_train<-hn_med[1:562, ]
# hn_med_test<-hn_med[563:662, ]
# hn_med_dtm_train<-hn_med_dtm[1:562, ]
# hn_med_dtm_test<-hn_med_dtm[563:662, ]
# corpus_train<-corpus_clean[1:562]
# corpus_test<-corpus_clean[563:662]

prop.table(table(hn_med_train$seer_stage))
prop.table(table(hn_med_test$seer_stage))

hn_med_train$stage<-hn_med_train$seer_stage %in% c(5:9)
hn_med_train$stage<-factor(hn_med_train$stage, levels=c(F, T), labels = c("early_stage", "later_stage"))
hn_med_test$stage<-hn_med_test$seer_stage %in% c(5:9)
hn_med_test$stage<-factor(hn_med_test$stage, levels=c(F, T), labels = c("early_stage", "later_stage"))
prop.table(table(hn_med_train$stage))
prop.table(table(hn_med_test$stage))

# 5.2.3 Visualizing text data - word clouds
# install.packages("wordcloud", repos = "http://cran.us.r-project.org")
library(wordcloud)
wordcloud(corpus_train, min.freq = 40, random.order = FALSE, colors=brewer.pal(5, "Dark2"))
early<-subset(hn_med_train, stage=="early_stage")
later<-subset(hn_med_train, stage=="later_stage")
wordcloud(early$MEDICATION_SUMMARY, max.words = 20, colors=brewer.pal(3, "Dark2"))
wordcloud(later$MEDICATION_SUMMARY, max.words = 20, colors=brewer.pal(3, "Dark2"))

# 5.2.4 Data preparation - creating indicator features for frequent words
summary(findFreqTerms(hn_med_dtm_train, 5))
hn_med_dict<-as.character(findFreqTerms(hn_med_dtm_train, 5))
hn_train<-DocumentTermMatrix(corpus_train, list(dictionary=hn_med_dict))
hn_test<-DocumentTermMatrix(corpus_test, list(dictionary=hn_med_dict))
convert_counts <- function(wordFreq) {
  wordFreq <- ifelse(wordFreq > 0, 1, 0)
  wordFreq <- factor(wordFreq, levels = c(0, 1), labels = c("No", "Yes"))
  return(wordFreq)
}

hn_train <- apply(hn_train, MARGIN = 2, convert_counts)
hn_test <- apply(hn_test, MARGIN = 2, convert_counts)

# Check the structure of hn_train and hn_train:
# head(hn_train); dim(hn_train)

# 5.3 Step 3 - training a model on the data
# install.packages("e1071", repos = "http://cran.us.r-project.org")
library(e1071)
hn_classifier <- naiveBayes(hn_train, hn_med_train$stage)
hn_test_pred<-predict(hn_classifier, hn_test)

# 5.4 Step 4 - evaluating model performance
library(gmodels)
CT <- CrossTable(hn_test_pred, hn_med_test$stage)
CT
mod_TN <- CT$prop.row[1, 1]  
mod_FP <- CT$prop.row[1, 2]
mod_FN <- CT$prop.row[2, 1]
mod_TP <- CT$prop.row[2, 2]

# library(caret)
# confusionMatrix(hn_test_pred, hn_med_test$stage)

# CT$prop.row
library(plotly)
plot_ly(x = c("TN", "FN", "FP", "TP"),
        y = c(mod_TN, mod_FN, mod_FP, mod_TP),
        name = c("TN", "FN", "FP", "TP"), type = "bar", color=c("TN", "FN", "FP", "TP")) %>% 
  layout(title="Consusion Matrix", 
         legend=list(title=list(text='<b> Metrics </b>')), 
         xaxis=list(title='Metrics'), yaxis=list(title='Probability'))

# 5.5 Step 5 - improving model performance
set.seed(1234)
hn_classifier1 <- naiveBayes(hn_train, hn_med_train$stage, laplace = 1, type = "class")
hn_test_pred1 <- predict(hn_classifier1, hn_test)
CrossTable(hn_test_pred1, hn_med_test$stage)

# 5.6 Step 6 - compare Naive Bayesian vs. LDA
library(MASS)
# df_hn_train = data.frame(lapply(as.data.frame(hn_train),as.numeric), stage = hn_med_train$stage)
# df_hn_test = data.frame(lapply(as.data.frame(hn_test),as.numeric), stage = hn_med_test$stage)

library("dplyr")
binarizeFunction <- function(x) { ifelse(x=="Yes", 1,0) }

#  A function to Convert Categorical variables to numeric
cat2Numeric <- function (dfInput) {
  df = as.data.frame(lapply( as.data.frame(dfInput), factor)) %>%
    mutate_all(binarizeFunction)
  return(df)
}

# define the numeric DF of predictors (X) and outcome (Y=stage)
df_hn_train = data.frame(cat2Numeric(hn_train), stage = as.numeric(hn_med_train$stage))
df_hn_test = data.frame(cat2Numeric(hn_test), stage = as.numeric(hn_med_test$stage))

# Remove the multicollinearity - this should be done via VIF assessment, 
#      but for now, just take the first few predictors
df_hn_train <- df_hn_train[ , c(1:34, 40:50, 60:70, 109)]

# Fit LDA
set.seed(1234)
hn_lda <- lda(data=df_hn_train, stage~.)
# hn_pred = predict(hn_lda, df_hn_test[,-104])
hn_pred = predict(hn_lda, df_hn_test)
CrossTable(hn_pred$class, df_hn_test$stage)

