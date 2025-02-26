
# Load required libraries
library(tm)
library(plotly)
library(wordcloud)
# Step:1 read zip file into a corpus
corpus <- Corpus(ZipSource("AutoAndElectronics1.zip", recursive = T))

str(corpus)
class(corpus)
corpus[[1]]$content

# Step 2: Clean the VCorpus Object
# Text Pre-processing
corpus <- tm_map(corpus, tolower) # To-Lower Case Transformation
corpus <- tm_map(corpus, removeWords, stopwords("english")) # Remove Stopwords
corpus <- tm_map(corpus, stripWhitespace) # Remove the extra white space
corpus <- tm_map(corpus, removeNumbers) # Remove numbers
corpus <- tm_map(corpus, removePunctuation) 
corpus <- tm_map(corpus, PlainTextDocument)

# Stemming: Removal of Plurals and Action Suffixes
corpus <- tm_map(corpus, stemDocument, language = "english")
corpus # check corpus


# Step 3:build document-term matrix used 
dtm_corpus = DocumentTermMatrix(corpus)
dtm_corpus
dtm_corpus$dimnames$Docs = as.character(1:1000)
inspect(dtm_corpus)

# Step 4: Identify auto-related and electronics-related documents

# Check the frequent terms
findFreqTerms(dtm_corpus, lowfreq = 1000)

# Find correlated words for car 
findAssocs(dtm_corpus, "car", corlimit = 0.6)


# Create DTM objects for each group (auto-related (top 500) and electronics-related (bottom 500) 

dtm_top500 = dtm_corpus[1:500,]
dtm_bot500 = dtm_corpus[501:1000,]

dtm_top500
dtm_bot500

# Remove sparse terms
dtm_top500 = removeSparseTerms(dtm_top500, 0.90)
dtm_bot500 =removeSparseTerms(dtm_bot500, 0.90)

# Frequent words for auto-related
findFreqTerms(dtm_top500, lowfreq = 100)

# Frequent words for Electronics-related
findFreqTerms(dtm_bot500, lowfreq = 100)


# calculate the cumulative frequencies of words across documents and sort:

# find frequencies For Auto-related 
Freq_Auto = sort(colSums(as.matrix(dtm_top500)), decreasing = T)
Freq_Auto

# find frequencies for Electronics-related 
Freq_Electronic = sort(colSums(as.matrix(dtm_bot500)), decreasing = T)
Freq_Electronic

# plot frequent words for auto-related
WF1=data.frame(term=names(Freq_Auto), occurrences=Freq_Auto)

df.Freq_Auto <- subset(WF1, Freq_Auto>2)
plot_ly(data=df.Freq_Auto, x=~term, y=~occurrences, type="bar") %>%
  layout(title="Top 500 Auto-Related (Frequent Terms)")


# plot frequent words for auto-related
WF2 = data.frame(term=names(Freq_Electronic), occurrences=Freq_Electronic)

df.Freq_Electronic <- subset(WF2, Freq_Electronic>2)
plot_ly(data=df.Freq_Electronic, x=~term, y=~occurrences, type="bar") %>%
  layout(title="Top 500 Electronic-Related (Frequent Terms)")


# Word cloud for auto related
wordcloud(names(Freq_Auto), Freq_Auto, min.freq=5, colors=brewer.pal(6, "Spectral"))
# Word cloud for Electronics related
wordcloud(names(Freq_Electronic), Freq_Electronic, min.freq = 5, colors = brewer.pal(6, "Spectral"))

# what are common (frequently occuring words) in both auto-related and electronic-related?
common_terms = intersect(subset(WF1, Freq_Auto>2)$term, subset(WF2, Freq_Electronic>2)$term)
print(common_terms)

#build term-document matrix
tdm_corpus = TermDocumentMatrix(corpus)
tdm_corpus

# Combining TF and IDF
tdm_tfidf = weightTfIdf(tdm_corpus)
tdm_tfidf

d = weightTfIdf(dtm_corpus)
d
# Perform LSA (with dim=5) to receive a LSA object from the TF-IDF matrix.
#install.packages("lsa")
library(lsa)
lsa.tfidf <- lsa(tdm_tfidf, dim = 5)
# convert to data frame
words.df <- as.data.frame(as.matrix(lsa.tfidf$dk))
head(words.df)
# create an array of records labels
label <- c(rep(1, 500), rep(0, 500))

library(caret)

# prepare training and holdout sets
set.seed(1234)
df <- cbind(label=factor(label), words.df)
head(df)
# Data partitioning : train data =60% and test data=40%
idx <- caret::createDataPartition(df$label, p=0.6, list=FALSE)
train.df <- df[idx,]
holdout.df <- df[-idx,]

dim(train.df)
dim(holdout.df)

# fit logistic regression
Logit.model = glm(label~., data = train.df, family = "binomial")
# predict 
Logit.Pred = predict(Logit.model, newdata = holdout.df, type = "response")
Logit.Pred = ifelse(Logit.Pred >= 0.5, 1, 0)

# Logit.Pred is a numeric vector of predictions (0 and 1)
Logit.Pred = factor(Logit.Pred, levels = c("0", "1"))

confusionMatrix(Logit.Pred, holdout.df$label)

# Improving and comparing model performance (Use any other classification algorithm(s) for modeling. Compare and comment on results.)
library(rpart)
Classification.Model = rpart(label~., data = train.df)

Class_Pred = predict(Classification.Model, newdata = holdout.df,type = "class")
confusionMatrix(Class_Pred, holdout.df$label)

