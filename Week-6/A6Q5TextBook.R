library(tm)
# step 1: import and label records
# read zip file into a corpus
corp <- Corpus(ZipSource(mlba::AutosElectronics, recursive = T))

# create an array of records labels
label <- c(rep(1, 1000), rep(0, 1000))

# step 2: text preprocessing
# tokenization
corp <- tm_map(corp, stripWhitespace)
corp <- tm_map(corp, removePunctuation)
corp <- tm_map(corp, removeNumbers)

# stopwords
corp <- tm_map(corp, removeWords, stopwords("english"))

# stemming
corp <- tm_map(corp, stemDocument)
corp
# step 3: TF-IDF and latent semantic analysis
# compute TF-IDF
tdm <- TermDocumentMatrix(corp)
tdm
tdm_dtm = DocumentTermMatrix(corp)
tdm_dtm
tfidf <- weightTfIdf(tdm)
tfidf
# extract (20) concepts
library(lsa)
lsa.tfidf <- lsa(tfidf, dim = 20)

# convert to data frame
words.df <- as.data.frame(as.matrix(lsa.tfidf$dk))
words.df
library(caret)

# prepare training and holdout sets
set.seed(1)
df <- cbind(label=factor(label), words.df)
head(df)
idx <- caret::createDataPartition(df$label, p=0.6, list=FALSE)
train.df <- df[idx,]
holdout.df <- df[-idx,]

# fit logistic regression
logit.reg <- train(label ~., data=train.df,
                   trControl=trainControl(method="none"),
                   method="glm", family="binomial")

# compute accuracy on holdout set
pred <- predict(logit.reg, newdata=holdout.df)
confusionMatrix(pred, holdout.df$label)
 