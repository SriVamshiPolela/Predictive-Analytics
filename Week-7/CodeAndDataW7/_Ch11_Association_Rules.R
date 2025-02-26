# 6 A Toy Example
require(knitr)
item_table = as.data.frame(t(c("{1,2,3,4}","{1,2,4}","{1,2}","{2,3,4}","{2,3}","{3,4}","{2,4}")))
colnames(item_table) <- c("choice1","choice2","choice3","choice4","choice5","choice6","choice7")
kable(item_table, caption = "Item table")

item_table = as.data.frame(t(c(3,6,4,5)))
colnames(item_table) <- c("items: {1}","{2}","{3}","{4}")
rownames(item_table) <- "(N=7)*support"
kable(item_table,caption = "Size 1 Support")

item_table = as.data.frame(t(c(3,1,2,3,4,3)))
colnames(item_table) <- c("{1,2}","{1,3}","{1,4}","{2,3}","{2,4}","{3,4}")
rownames(item_table) <- "N*support"
kable(item_table,caption = "Size 2 Support")

item_table = as.data.frame(t(c(1,2,1,2)))
colnames(item_table) <- c("{1,2,3}", "{1,2,4}", "{1,3,4}","{2,3,4}")
rownames(item_table) <- "N*support"
kable(item_table,caption = "Size 3 Support")

item_table = as.data.frame(t(c(1)))
colnames(item_table) <- c("{1,2,3,4}")
rownames(item_table) <- "N*support"
kable(item_table,caption = "Size 4 Support")


# 7 Case Study 1: Head and Neck Cancer Medications
# 7.1 Step 1: Collecting Data
# dataf file: 10_medication_descriptions.csv
# 7.2 Step 2: Exploring and Preparing the Data
med<-read.csv("https://umich.instructure.com/files/1678540/download?download_frd=1", stringsAsFactors = FALSE)
med<-med[, -1]
write.csv(med, "medication.csv", row.names=F)
library(knitr)
kable(med[1:5, ])

# install.packages("arules")
library(arules)
med<-read.transactions("medication.csv", sep = ",", skip = 1, rm.duplicates=TRUE)
summary(med)

# 7.2.1 Visualizing Item Support: Item Frequency Plots
inspect(med[1:5,])
itemFrequency(med[, 1:5])
itemFrequencyPlot(med, topN=20)
itemFrequencyPlot(med, support=0.1)

# 7.2.2 Visualizing Transaction Data: Plotting the Sparse Matrix
image(med[1:5, ])
subset_int <- sample(nrow(med), 100, replace = F)  
image(med[subset_int, ])

# 7.3 Step 3: Training a Model on the Data
apriori(med) #default setting support=0.1, confidence=0.8
med_rule<-apriori(med, parameter=list(support=0.01, confidence=0.25, minlen=2))
med_rule

# 7.4 Step 4: Evaluating Model Performance
summary(med_rule)
# install.packages("arulesViz")
library(arulesViz)
plot(sort(med_rule))
inspect(med_rule[1:3])

# 7.5 Step 5: Sorting the Set of Association Rules
inspect(sort(med_rule, by="lift")[1:3])

# 7.6 Taking Subsets of Association Rules
fi_rules<-subset(med_rule, items %in% "fentanyl injection uh")
inspect(fi_rules)

# 7.7 Graphical depiction of association rules
# ?arulesViz::plot()
# plot(sort(fi_rules, by="lift"), method="grouped", control=list(type="items"), 
#      main = "Grouped Matrix for the 14 Fentanyl-associated Rules")
subrules2 <- sample(subset(fi_rules, lift > 2), 5)
#plot(sort(subrules2, by="lift"), method="grouped", control=list(type="items"), engine = "htmlwidget")
plot(fi_rules, method="graph", measure = "support", engine="htmlwidget", # nodeCol=rainbow(14),
     shading = "lift", control = list(verbose = TRUE))

# 7.8 Saving association rules to a file or data frame
write(med_rule, file = "medrule.csv", sep=",", row.names=F)
med_df<-as(med_rule, "data.frame")
str(med_df)

# 8 Practice Problems
# 8.1 Groceries
library(arules)
data("Groceries")
summary(Groceries)

itemFrequencyPlot(Groceries, topN=10)
#itemFrequencyPlotly(Groceries, 10, "grocieries")

groceryrules <- apriori(Groceries, parameter = list(support =	0.006, confidence = 0.25, minlen = 2))	
groceryrules	
inspect(sort(groceryrules, by = "lift")[1:3])	

groceryrules <- apriori(Groceries, parameter = list(support = 0.006, confidence = 0.6, minlen = 2))	
groceryrules	
inspect(sort(groceryrules, by = "lift")[1:3])	
