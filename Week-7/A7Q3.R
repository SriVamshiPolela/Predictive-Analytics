# load dataset using arules
# install.packages("arules", repos = "https://cran.r-project.org/")
library(arules)
basket <- read.transactions('MarketBasket.csv', sep = ',', rm.duplicates = TRUE)

# Number of transactions and items in the dataset
basket

'b)Show the first five transactions.'
inspect(basket[1:5])

'c)Find out the top frequent grocery items that have minimum support 0.1 and plot them. Which item has the highest support?'
# Plot frequent items
itemFrequencyPlot(basket, support=0.1 ,topN=10)


'd)Use support = 0.003, confidence = 0.4, minlen = 2 to generate the grocery association rules. Sort the rules with highest lift. 
Translate two of the rules into “if-then” statements. Interpret the various measures (such as support, confidence, and lift).'
# Generate association rules
rules <- apriori(basket, parameter = list(support = 0.003, confidence = 0.4, minlen = 2))
rules <- apriori(basket, support = 0.003, confidence = 0.4, minlen = 2)

inspect(sort(rules, by="lift")[1:2])




