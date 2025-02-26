# Read the csv file
# Step 1: Collecting data 
WestRoxbury.df = read.csv("WestRoxbury.csv")
str(WestRoxbury.df)
head(WestRoxbury.df)


'Step 2: Exploring data and preparing data (Use only the first 1000 rows of data and 6 variables. 
Explore data by creating a couple of plots of your interested measurements. Explain your findings. 
Create dummies for categorical variable and rescale the data so distance can be calculated properly.)
'
WestRoxbury.df = WestRoxbury.df[1:1000, c(1,3,4,12,13,14)]
head(WestRoxbury.df)
# Set up a 2x3 grid
par(mfrow = c(2, 3))
# Check the distribution of TOTAL_VALUE
hist(WestRoxbury.df$TOTAL_VALUE,   main = "Distribution of Total_Value of property", xlab = "Total value of property") # Right Skewed
hist(WestRoxbury.df$LOT.SQFT, main = "Distribution of Total Lot Size in sq.ft", xlab = "Total Lot Size") # Right Skewed
hist(WestRoxbury.df$YR.BUILT, main = "Distribution  of Year property was built", xlab = "Year") # left Skewed

# create distribution / frequency plot for Kitchen, Fireplace and REMODEL 
kitchen_Freq = table(WestRoxbury.df$KITCHEN)
barplot(kitchen_Freq, main = "Distribution of Kitchen", xlab = "Kitchen", col = "skyblue")

Firplace_Freq = table(WestRoxbury.df$FIREPLACE)
barplot(Firplace_Freq, main = "Distribution of Fireplace", xlab = "Fireplace", col ="skyblue")

Remodel_Freq = table(WestRoxbury.df$REMODEL)
barplot(Remodel_Freq, main = "Distribution of House Remodel", xlab = "Remodel", col = "skyblue")
# Reset the plotting layout to default (1x1)
par(mfrow = c(1, 1))

# Create a scatter plot with a smooth line
ggplot(WestRoxbury.df, aes(x = LOT.SQFT, y = TOTAL_VALUE)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "blue") +  # Add a linear regression line
  labs(title = "Scatter Plot of Lot Size vs Total Values with Regression Line",
       x = "Lot Size (sqft)",
       y = "Total Value")

summary(WestRoxbury.df)

# Data preprocessing
unique_w = lapply(WestRoxbury.df, unique)
unique_w

# Check missing values for each variable
missing_values <- lapply(WestRoxbury.df, function(x) sum(is.na(x)))
missing_values

# Create dummy variables for REMODEL
library(fastDummies)
WestRoxbury.df = dummy_cols(WestRoxbury.df, select_columns = "REMODEL",remove_selected_columns = TRUE)

# Standardize the data
WestRoxbury.df_Z = as.data.frame(lapply(WestRoxbury.df, scale))
str(WestRoxbury.df_Z)

'Step 3: Training a model on the data (Use the function kmeans() for the k-means clustering algorithm to create 4 clusters. 
Set a seed so the results can be reproduced.)'

library(stats)
set.seed(2024)
K_Means = kmeans(WestRoxbury.df_Z, 4)
K_Means$size
# Display cluster centers
K_Means$centers
require(cluster)

# Plot visual presentation (profile line plot or bar plot) of cluster centroids
par(mfrow = c(2, 4), mar = c(4, 4, 2, 1))

# Loop through each variable and create bar plots for each cluster
for (i in 1:8) {
  barplot(K_Means$centers[, i], names.arg = 1:4, main = colnames(K_Means$centers)[i], col = 1:4, ylim = range(K_Means$centers))
}
par(mfrow = c(1, 1))

euclidean <- function(a, b){ 
  sqrt(sum((a-b)^2))
}

distance = dist(WestRoxbury.df_Z)
print(as.matrix(distance))
sil = silhouette(K_Means$cluster, distance)
summary(sil)

# This plot is not suitable for large data, if you remove border for each bar it will display it has like density plot
# plot(sil, col=c(1:length(K_Means$size)))
plot(sil, col=c(1:length(K_Means$size)), border = NA)

library(factoextra)
# use this to display
factoextra::fviz_silhouette(sil, label=T, palette = "jco", ggtheme = theme_classic())

library(plotly)
df <- as.data.frame(t(K_Means$centers))
rowNames <- rownames(df)
colnames(df) <- paste0("Cluster",c(1:4))
plot_ly(df, x = rownames(df), y = ~Cluster1, type = 'bar', name = 'Cluster1') %>% 
  add_trace(y = ~Cluster2, name = 'Cluster2') %>% 
  add_trace(y = ~Cluster3, name = 'Cluster3') %>% 
  add_trace(y = ~Cluster4, name = 'Cluster4') %>%
  layout(title="Explicating Derived Cluster Labels",
         yaxis = list(title = 'Cluster Centers'), barmode = 'group')


# Step: 5 usage of cluster information
K_Means$centers
K_Means$size
WestRoxbury.df$Cluster = K_Means$cluster
WestRoxbury.df[1:5,]

library(plotly)
  clusterNames <- paste0("Cluster ", WestRoxbury.df$Cluster)
plot_ly(data = WestRoxbury.df, x = ~TOTAL_VALUE, y = ~LOT.SQFT, type="scatter", mode="markers",
        color = ~Cluster, marker = list(size = 30), name=clusterNames) %>%
  hide_colorbar()


