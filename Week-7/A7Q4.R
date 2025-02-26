library(rvest)	
wiki_url <- read_html("https://wiki.socr.umich.edu/index.php/SOCR_012708_ID_Data_HotDogs")	
html_nodes(wiki_url, "#content")	
hotdog<- html_table(html_nodes(wiki_url, "table")[[1]])	
plot(hotdog$Calories, hotdog$Sodium, main = "Hotdogs", xlab="Calories", ylab="Sodium")	
segments(120, 280, 120, 570, lty=2)	
segments(120, 280, 30, 280, lty=2)	
segments(120, 570, 30, 570, lty=2)	
segments(125, 250, 125, 600, lty=2)	
segments(125, 250, 165, 250, lty=2)	
segments(165, 250, 165, 600, lty=2)	
segments(125, 600, 165, 600, lty=2)	
segments(170, 400, 170, 700, lty=2)	
segments(170, 400, 250, 400, lty=2)	
text(100, 220, "cluster 1")	
text(140, 200, "cluster 2")	
text(185, 350, "cluster 3")	

hotdog.df <- hotdog[,-1]
hotdog.df.norm <- sapply(hotdog.df, scale)
set.seed(123)
km <- kmeans(hotdog.df.norm, 3)
km$cluster
km$centers
km$withinss
km$size

library(ggplot2)
km.Lable <- as.factor(km$cluster)
ggplot(hotdog.df, aes(Calories, Sodium, color = km.Lable)) + geom_point()
mydata <- hotdog.df.norm
wss <- (nrow(mydata)-1)*mean(apply(mydata,2,var))
for (i in 1:6) wss[i] <- mean(kmeans(mydata, centers=i)$withinss)
plot(1:6, wss, type="b", xlab="Number of Clusters (k)",
     ylab="Average Within−Cluster Squared Distance")


# Hierarchical clustering  
distance_HC <- dist(hotdog.df.norm, method = "euclidean")
hc <- hclust(distance_HC, method = "average")
# Plot the dendrogram

plot(hc, hang = -1, ann = FALSE)

# Choose the number of clusters (in this case, 3)
num_clusters <- 3

# Extract height values from the dendrogram
heights <- as.vector(hc$height)

# Sort unique height values in ascending order
sorted_heights <- unique(sort(heights))

# Select the cutoff height corresponding to the desired number of clusters
cutoff_height <- sorted_heights[length(sorted_heights) - (num_clusters - 1)]

# Print the cutoff height
print(cutoff_height)

# here the cutoff height value is 1.92004 so i took 2 
# Draw a horizontal line at the cutoff height
abline(h = 2, col = "red", lty = 2)


# Cut the dendrogram at the chosen height to obtain clusters
clusters <- cutree(hc, h = 2)

# Print the sizes of each cluster
table(clusters)



