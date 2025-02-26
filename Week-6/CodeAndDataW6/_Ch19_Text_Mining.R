# 1 A Simple NLP/TM Example
# 1.1 Define and Load the Unstructured-Text Documents
doc1 <- "HS650: The Data Science and Predictive Analytics (DSPA) course (offered as a massive open online course, MOOC, as well as a traditional University of Michigan class) aims to build computational abilities, inferential thinking, and practical skills for tackling core data scientific challenges. It explores foundational concepts in data management, processing, statistical computing, and dynamic visualization using modern programming tools and agile web-services. Concepts, ideas, and protocols are illustrated through examples of real observational, simulated and research-derived datasets. Some prior quantitative experience in programming, calculus, statistics, mathematical models, or linear algebra will be necessary. This open graduate course will provide a general overview of the principles, concepts, techniques, tools and services for managing, harmonizing, aggregating, preprocessing, modeling, analyzing and interpreting large, multi-source, incomplete, incongruent, and heterogeneous data (Big Data). The focus will be to expose students to common challenges related to handling Big Data and present the enormous opportunities and power associated with our ability to interrogate such complex datasets, extract useful information, derive knowledge, and provide actionable forecasting. Biomedical, healthcare, and social datasets will provide context for addressing specific driving challenges. Students will learn about modern data analytic techniques and develop skills for importing and exporting, cleaning and fusing, modeling and visualizing, analyzing and synthesizing complex datasets. The collaborative design, implementation, sharing and community validation of high-throughput analytic workflows will be emphasized throughout the course."
doc2 <- "Bootcamp: A week-long intensive Bootcamp focused on methods, techniques, tools, services and resources for big healthcare and biomedical data analytics using the open-source statistical computing software R. Morning sessions (3 hrs) will be dedicated to methods and technologies and applications. Afternoon sessions (3 hrs) will be for group-based hands-on practice and team work. Commitment to attend the full week of instruction (morning sessions) and self-guided work (afternoon sessions) is required. Certificates of completion will be issued only to trainees with perfect attendance that complete all work. This hands-on intensive graduate course (Bootcamp) will provide a general overview of the principles, concepts, techniques, tools and services for managing, harmonizing, aggregating, preprocessing, modeling, analyzing and interpreting large, multi-source, incomplete, incongruent, and heterogeneous data (Big Data). The focus will be to expose students to common challenges related to handling Big Data and present the enormous opportunities and power associated with our ability to interrogate such complex datasets, extract useful information, derive knowledge, and provide actionable forecasting. Biomedical, healthcare, and social datasets will provide context for addressing specific driving challenges. Students will learn about modern data analytic techniques and develop skills for importing and exporting, cleaning and fusing, modeling and visualizing, analyzing and synthesizing complex datasets. The collaborative design, implementation, sharing and community validation of high-throughput analytic workflows will be emphasized throughout the course."
doc3 <- "HS 853: This course covers a number of modern analytical methods for advanced healthcare research. Specific focus will be on reviewing and using innovative modeling, computational, analytic and visualization techniques to address concrete driving biomedical and healthcare applications. The course will cover the 5 dimensions of Big-Data (volume, complexity, multiple scales, multiple sources, and incompleteness). HS853 is a 4 credit hour course (3 lectures + 1 lab/discussion). Students will learn how to conduct research, employ and report on recent advanced health sciences analytical methods; read, comprehend and present recent reports of innovative scientific methods; apply a broad range of health problems; experiment with real Big-Data. Topics Covered include: Foundations of R, Scientific Visualization, Review of Multivariate and Mixed Linear Models, Causality/Causal Inference and Structural Equation Models, Generalized Estimating Equations, PCOR/CER methods Heterogeneity of Treatment Effects, Big-Data, Big-Science, Internal statistical cross-validation, Missing data, Genotype-Environment-Phenotype, associations, Variable selection (regularized regression and controlled/knockoff filtering), medical imaging, Databases/registries, Meta-analyses, classification methods, Longitudinal data and time-series analysis, Geographic Information Systems (GIS), Psychometrics and Rasch measurement model analysis, MCMC sampling for Bayesian inference, and Network Analysis"
doc4 <- "HS 851: This course introduces students to applied inference methods in studies involving multiple variables. Specific methods that will be discussed include linear regression, analysis of variance, and different regression models. This course will emphasize the scientific formulation, analytical modeling, computational tools and applied statistical inference in diverse health-sciences problems. Data interrogation, modeling approaches, rigorous interpretation and inference will be emphasized throughout. HS851 is a 4 credit hour course (3 lectures + 1 lab/discussion).  Students will learn how to: ,  Understand the commonly used statistical methods of published scientific papers , Conduct statistical calculations/analyses on available data , Use software tools to analyze specific case-studies data , Communicate advanced statistical concepts/techniques , Determine, explain and interpret assumptions and limitations. Topics Covered  include   Epidemiology , Correlation/SLR , and slope inference, 1-2 samples , ROC Curve , ANOVA , Non-parametric inference , Cronbach's $\alpha$, Measurement Reliability/Validity , Survival Analysis , Decision theory , CLT/LLNs - limiting results and misconceptions , Association Tests , Bayesian Inference , PCA/ICA/Factor Analysis , Point/Interval Estimation (CI) - MoM, MLE , Instrument performance Evaluation , Study/Research Critiques , Common mistakes and misconceptions in using probability and statistics, identifying potential assumption violations, and avoiding them."
doc5 <- "HS550: This course provides students with an introduction to probability reasoning and statistical inference. Students will learn theoretical concepts and apply analytic skills for collecting, managing, modeling, processing, interpreting and visualizing (mostly univariate) data. Students will learn the basic probability modeling and statistical analysis methods and acquire knowledge to read recently published health research publications. HS550 is a 4 credit hour course (3 lectures + 1 lab/discussion).  Students will learn how to:  Apply data management strategies to sample data files , Carry out statistical tests to answer common healthcare research questions using appropriate methods and software tools , Understand the core analytical data modeling techniques and their appropriate use  Examples of Topics Covered ,  EDA/Charts , Ubiquitous variation , Parametric inference , Probability Theory , Odds Ratio/Relative Risk , Distributions , Exploratory data analysis , Resampling/Simulation , Design of Experiments , Intro to Epidemiology , Estimation , Hypothesis testing , Experiments vs. Observational studies , Data management (tables, streams, cloud, warehouses, DBs, arrays, binary, ASCII, handling, mechanics) , Power, sample-size, effect-size, sensitivity, specificity , Bias/Precision , Association vs. Causality , Rate-of-change , Clinical vs. Stat significance , Statistical Independence Bayesian Rule."

# 1.2 Create a New VCorpus Object
docs<-c(doc1, doc2, doc3, doc4, doc5)
class(docs)
install.packages("tm")
install.packages("tm", repos = "https://cran.r-project.org")


library(tm)
doc_corpus<-VCorpus(VectorSource(docs))
doc_corpus
doc_corpus[[1]]$content
View(doc_corpus)
# 1.3 To-Lower Case Transformation
doc_corpus<-tm_map(doc_corpus, tolower)
# corpus <- tm_map(corpus, PlainTextDocument) which beats it right back into the correct data type.
doc_corpus <- tm_map(doc_corpus, PlainTextDocument)
#doc_corpus<-tm_map(doc_corpus, content_transformer(tolower))
#doc_corpus <- tm_map(doc_corpus, content_transformer(tolower), mc.cores = 1)

doc_corpus[[1]]

class(doc_corpus)
str(doc_corpus)
# 1.4 Text Pre-processing
# 1.4.1 Remove Stopwords
stopwords("english")
doc_corpus<-tm_map(doc_corpus, removeWords, stopwords("english"))
doc_corpus[[1]]

doc_corpus<-tm_map(doc_corpus, stripWhitespace)
doc_corpus[[1]]

# 1.4.2 Remove Punctuation
doc_corpus<-tm_map(doc_corpus, removePunctuation)
doc_corpus[[2]]

doc_corpus<-tm_map(doc_corpus, PlainTextDocument)

# 1.4.3 Stemming: Removal of Plurals and Action Suffixes
doc_corpus[[1]]$content
doc_corpus[[2]]$content
doc_corpus[[3]]$content

install.packages("SnowballC", repos = "https://cran.r-project.org")
library(SnowballC)
doc_corpus<-tm_map(doc_corpus, stemDocument)
doc_corpus[[1]]$content

# 1.5 Bags of Words
# 1.6 Document Term Matrix
#doc_dtm1<-TermDocumentMatrix(doc_corpus)
#doc_dtm1
doc_dtm<-DocumentTermMatrix(doc_corpus)
doc_dtm

doc_dtm$dimnames$Docs<-as.character(1:5)
inspect(doc_dtm)
findFreqTerms(doc_dtm, lowfreq = 2)
findFreqTerms(doc_dtm, lowfreq = 5)
findAssocs(doc_dtm, "statist", corlimit = 0.8)

# 2 Case-Study: Job Ranking
library(rvest)
wiki_url <- read_html("https://wiki.socr.umich.edu/index.php/SOCR_Data_2011_US_JobsRanking")
html_nodes(wiki_url, "#content")
job <- html_table(html_nodes(wiki_url, "table")[[1]])
head(job)

# 2.1 Step 1: Make a VCorpus Object
#jobCorpus<-VCorpus(VectorSource(job[, 10]))
#library(dplyr)
#jobCorpus<-VCorpus(VectorSource(pull(job[, 10])))

jobs <- as.list(job$Description)
jobCorpus <- VCorpus(VectorSource(jobs))

# 2.2 Step 2: Clean the VCorpus Object
jobCorpus<-tm_map(jobCorpus, tolower)
#jobCorpus<-tm_map(jobCorpus, content_transformer(tolower))
for(j in seq(jobCorpus)){
  jobCorpus[[j]] <- gsub("_", " ", jobCorpus[[j]])
}
jobCorpus<-tm_map(jobCorpus, removeWords, stopwords("english"))
jobCorpus<-tm_map(jobCorpus, removePunctuation)
jobCorpus<-tm_map(jobCorpus, stripWhitespace)
jobCorpus<-tm_map(jobCorpus, PlainTextDocument)
jobCorpus<-tm_map(jobCorpus, stemDocument)

# 2.3 Step 3: build document-term matrix
jobCorpus[[1]]$content
dtm<-DocumentTermMatrix(jobCorpus)
dtm
dtm$dimnames$Docs<-as.character(1:200)
inspect(dtm[1:10, 1:10])
dtm_top30<-dtm[1:30, ]
dtm_bot100<-dtm[101:200, ]
dtm_top30
dtm_bot100
dtms_top30<-removeSparseTerms(dtm_top30, 0.90)
dtms_top30
dtms_bot100<-removeSparseTerms(dtm_bot100, 0.94)
dtms_bot100

# Let's calculate the cumulative frequencies of words across documents and sort:
freq1<-sort(colSums(as.matrix(dtms_top30)), decreasing=T)
freq1
freq2<-sort(colSums(as.matrix(dtms_bot100)), decreasing=T)
freq2

# Plot frequent words (for bottom 100 jobs)
wf2=data.frame(term=names(freq2), occurrences=freq2)
library(ggplot2)
p <- ggplot(subset(wf2, freq2>2), aes(term, occurrences))
p <- p + geom_bar(stat="identity")
p <- p + theme(axis.text.x=element_text(angle=45, hjust=1))
p

df.freq2 <- subset(wf2, freq2>2)
library(plotly)
plot_ly(data=df.freq2, x=~term, y=~occurrences, type="bar") %>%
  layout(title="Bottom 100 Job Descriptions (Frequent Terms)")

# Plot frequent words (for top 30 jobs)
wf1=data.frame(term=names(freq1), occurrences=freq1)
p2 <- ggplot(subset(wf1, freq1>2), aes(term, occurrences, fill = freq1))
p2 <- p2 + geom_bar(stat="identity")
p2 <- p2 + theme(axis.text.x=element_text(angle=45, hjust=1))
p2

df.freq1 <- subset(wf1, freq1>2)
plot_ly(data=df.freq1, x=~term, y=~occurrences, type="bar") %>%
  layout(title="Top 30 Job Descriptions (Frequent Terms)")

# what is common (frequently occuring words)
# in the description of the top 30 and the bottom 100 jobs?
intersect(subset(wf1, freq1>2)$term, subset(wf2, freq2>2)$term)

library(wordcloud)
set.seed(123)
wordcloud(names(freq1), freq1)

# Color code the frequencies using an appropriate color map:

# Sequential palettes names include: 
# Blues BuGn BuPu GnBu Greens Greys Oranges OrRd PuBu PuBuGn PuRd Purples RdPu Reds YlGn YlGnBu YlOrBr YlOrRd

# Diverging palettes include  
# BrBG PiYG PRGn PuOr RdBu RdGy RdYlBu RdYlGn Spectral
wordcloud(names(freq2), freq2, min.freq=5, colors=brewer.pal(6, "Spectral"))


# 80-20%  training-testing data split	
set.seed(1234)	
job$highrank<-ifelse(job$Index<30, 1, 0)
train_index <- sample(seq_len(nrow(job)), size = 0.8*nrow(job))	
job_train<-job[train_index, ]	
job_test<-job[-train_index, ]	
dtm_train<-dtm[train_index, ]	
dtm_test<-dtm[-train_index, ]	
job_corpus_train<-jobCorpus[train_index]
job_corpus_test<-jobCorpus[-train_index]

###################### from chapter 7 NB
summary(findFreqTerms(dtm_train, 5))
job_dict<-as.character(findFreqTerms(dtm_train, 5))
job_dic_train<-DocumentTermMatrix(job_corpus_train, list(dictionary=job_dict))
job_dic_test<-DocumentTermMatrix(job_corpus_test, list(dictionary=job_dict))

convert_counts <- function(wordFreq) {
  wordFreq <- ifelse(wordFreq > 0, 1, 0)
  wordFreq <- factor(wordFreq, levels = c(0, 1), labels = c("No", "Yes"))
  return(wordFreq)
}

job_dtm_train <- apply(job_dic_train, MARGIN = 2, convert_counts)
job_dtm_test <- apply(job_dic_test, MARGIN = 2, convert_counts)
library(e1071)
job_classifier <- naiveBayes(job_dtm_train, job_train$highrank)
job_test_pred<-predict(job_classifier, job_dtm_test)
caret::confusionMatrix(as.factor(job_test_pred), as.factor(job_test$highrank), positive = "1")

