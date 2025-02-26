# 1 Clustering as a machine learning task

#install.packages("rvest", repos = "https://cran.r-project.org/")	
library(rvest)	
# Hot Dog Calorie and Sodium Dataset
# Number of cases: 54
# Variable Names
#   Type: Type of hotdog (beef, meat, or poultry)
#   Calories: Calories per hot dog
#   Sodium: Milligrams of sodium per hot dog
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

#install.packages("calibrate",repos = "https://cran.r-project.org/")
library(calibrate)	
plot(hotdog$Calories, hotdog$Sodium)	
text(hotdog$Calories, hotdog$Sodium, labels=hotdog$Type, pos=3)	

library(ggplot2)	
gg1_plot = ggplot(hotdog, aes(x=Calories, y=Sodium)) + geom_point(aes(color=Type, shape = Type, stroke = 5),alpha=1) + theme_bw(base_size=15) +	
  guides(colour = guide_legend(override.aes = list(size=5))) +	
  theme(legend.position="top")	
gg1_plot	

modelLabels <- c('cluster 1', 'cluster 2', 'cluster 3')	
modelLabels.x <- c(100, 145, 185)	
modelLabels.y <- c(220, 200, 350)	
modelLabels.col <- c("blue", "red", "green")	
rect.x <- c(120, 80, 165, 125, 200, 170)	
rect.y <- c(130, 570, 250, 600, 400, 700)	

library(plotly)
plot_ly(hotdog, x = ~Calories, y = ~Sodium,  type="scatter",	
        mode="markers", symbol = "na", symbols = "circle-open") %>%	
  layout(title="2D Hotdogs Scatterplot (Calories vs. Sodium)",	
         xaxis=list(title="Calories"),     # , scaleanchor="y"),  # control the y:x axes aspect ratio	
         yaxis = list(title="Sodium"),     # , scaleanchor  = "x"), 	
         legend = list(title=list(text='<b> Meat Type</b>'), orientation = 'h'),	
         annotations = list(text=modelLabels,  x=modelLabels.x, y=modelLabels.y, textangle=c(90,90,90),	
                            font=list(size=15, color=modelLabels.col), showarrow=FALSE),	
         shapes = list(   # draw the rectangular boxes	
           list(type="rect", fillcolor="lightblue", line=list(color="blue"), opacity=0.2,	
                x0=rect.x[1], x1=rect.x[2], xref="x", y0=rect.y[1], y1=rect.y[2], yref="y"),	
           list(type="rect", fillcolor= "lightgreen", line=list(color="green"), opacity=0.2,	
                x0=rect.x[3], x1=rect.x[4], xref="x", y0=rect.y[3], y1=rect.y[4], yref = "y"),	
           list(type="rect", fillcolor= "lightgray", line=list(color="orange"), opacity=0.2,	
                x0=rect.x[5], x1=rect.x[6], xref="x", y0=rect.y[5], y1=rect.y[6], yref = "y"))	
  )

plot_ly(hotdog, x = ~Calories, y = ~Sodium, color=~Type, type="scatter",	
        mode="markers", name=~Type, symbol=~Type, marker = list(size = 15)) %>%	
  layout(title="2D Hotdogs Scatterplot (Calories vs. Sodium)",	
         xaxis=list(title="Calories"),     # , scaleanchor="y"),  # control the y:x axes aspect ratio	
         yaxis = list(title="Sodium"),     # , scaleanchor  = "x"), 	
         legend = list(title=list(text='<b> Meat Type</b>'), orientation = 'h'),	
         annotations = list(text=modelLabels,  x=modelLabels.x, y=modelLabels.y, textangle=c(90,90,90),	
                            font=list(size=15, color=modelLabels.col), showarrow=FALSE),	
         shapes = list(   # draw the rectangular boxes	
           list(type="rect", fillcolor="lightblue", line=list(color="blue"), opacity=0.2,	
                x0=rect.x[1], x1=rect.x[2], xref="x", y0=rect.y[1], y1=rect.y[2], yref="y"),	
           list(type="rect", fillcolor= "lightgreen", line=list(color="green"), opacity=0.2,	
                x0=rect.x[3], x1=rect.x[4], xref="x", y0=rect.y[3], y1=rect.y[4], yref = "y"),	
           list(type="rect", fillcolor= "lightgray", line=list(color="orange"), opacity=0.2,	
                x0=rect.x[5], x1=rect.x[6], xref="x", y0=rect.y[5], y1=rect.y[6], yref = "y"))	
  )	

# 3 The k-Means Clustering Algorithm
# 3.2 Choosing the appropriate number of clusters
require(graphics)	
x<-c(30, 200, 500, 1096.663, 3000, 5000, 7000, 10000)	
y<-function(x){	
  y=log(x)	
}	
curve(log(x), 30, 10000, xlab="k", ylab="Within-group Homogeneity", axes=F, main="Elbow Method")	
Axis(side=1, at=c(0, 2000, 4000, 6000, 8000, 10000), labels = c(rep("", 6)))	
Axis(side=2, at=4:9, labels = c(rep("", 6)))	
points(x, y(x))	
text(1000, 8, "elbow point")	
segments(1096.663, 7.3, 1000, 7.7)	

library(plotly)
elbowAnnot <- list(x = x[4], y = y(x[4]), text = "Elbow Point", xref = "x", yref = "y",	
                   showarrow = TRUE, arrowhead = 1)	
plot_ly(x = ~x, y = ~y(x), type = 'scatter', mode = 'markers+lines', marker=list(size=15)) %>%	
  # add_annotation(x=1, y=7, text="Elbow Point", showarrow=True, arrowhead=1) %>%	
  layout(title="Elbow Method", annotations = elbowAnnot,	
         xaxis=list(title="Number of clusters (k)"),	
         yaxis = list(title="Within-group Homogeneity"))	

# 4 Case Study 1: Divorce and Consequences on Young Adults
# 4.1 Step 1: Collecting Data
# data file: CaseStudy01_Divorce_YoungAdults_Data.csv

# 4.2 Step 2: Exploring and Preparing the Data
divorce<-read.csv("https://umich.instructure.com/files/399118/download?download_frd=1")
summary(divorce)
divorce$DIVYEAR<-ifelse(divorce$DIVYEAR==89, 0, 1)
table(divorce$livewithmom)
divorce[divorce$livewithmom==9, ]
divorce[45, 6]<-2
divorce[45, ]

# 4.3 Step 3: Training a Model on the Data
di_z<- as.data.frame(lapply(divorce, scale))
str(di_z)
library(stats)
set.seed(321)
diz_clussters<-kmeans(di_z, 3)

# 4.4 Step 4 - evaluating model performance
diz_clussters$size
require(cluster)
dis = dist(di_z)
sil = silhouette(diz_clussters$cluster, dis)
summary(sil)
plot(sil, col=c(1:length(diz_clussters$size)))
#plot(sil, col=c(1:length(diz_clussters$size)), border = NA)
#install.packages("factoextra", repos = "https://cran.r-project.org/")
library(factoextra)

factoextra::fviz_silhouette(sil, label=T, palette = "jco", ggtheme = theme_classic())
diz_clussters$centers

par(mfrow=c(1, 1), mar=c(4, 4, 4, 2))
myColors <- c("darkblue", "red", "green", "brown", "pink", "purple", "yellow")
barplot(t(diz_clussters$centers), beside = TRUE, xlab="cluster", 
        ylab="value", col = myColors)
legend("top", ncol=2, legend = c("DIVYEAR", "momint", "dadint", "momclose", "depression", "livewithmom", "gethitched"), fill = myColors)


df <- as.data.frame(t(diz_clussters$centers))
rowNames <- rownames(df)
colnames(df) <- paste0("Cluster",c(1:3))
library(plotly)
plot_ly(df, x = rownames(df), y = ~Cluster1, type = 'bar', name = 'Cluster1') %>% 
  add_trace(y = ~Cluster2, name = 'Cluster2') %>% 
  add_trace(y = ~Cluster3, name = 'Cluster3') %>% 
  layout(title="Explicating Derived Cluster Labels",
         yaxis = list(title = 'Cluster Centers'), barmode = 'group')

# 4.5 Step 5 - usage of cluster information
divorce$clusters<-diz_clussters$cluster
divorce[1:5, ]
require(ggplot2)
ggplot(divorce, aes(livewithmom, momint), main="Scatterplot Live with mom vs feel close to mom") +
  geom_point(aes(colour = factor(clusters), shape=factor(clusters), stroke = 8), alpha=1) + 
  theme_bw(base_size=25) +
  geom_text(aes(label=ifelse(clusters%in%1, as.character(clusters), ''), hjust=2, vjust=2, colour = factor(clusters)))+
  geom_text(aes(label=ifelse(clusters%in%2, as.character(clusters), ''), hjust=-2, vjust=2, colour = factor(clusters)))+
  geom_text(aes(label=ifelse(clusters%in%3, as.character(clusters), ''), hjust=2, vjust=-1, colour = factor(clusters))) + 
  guides(colour = guide_legend(override.aes = list(size=8))) +
  theme(legend.position="top")

clusterNames <- paste0("Cluster ", divorce$clusters)
plot_ly(data = divorce, x = ~livewithmom, y = ~momint, type="scatter", mode="markers",
        color = ~clusters, marker = list(size = 30), name=clusterNames) %>%
  hide_colorbar()

# 5 Model improvement
# install.packages("matrixStats")
library(matrixStats)
kpp_init = function(dat, K) {
  x = as.matrix(dat)
  n = nrow(x)
  # Randomly choose a first center
  centers = matrix(NA, nrow=K, ncol=ncol(x))
  set.seed(123)
  centers[1,] = as.matrix(x[sample(1:n, 1),])
  for (k in 2:K) {
    # Calculate dist^2 to closest center for each point
    dists = matrix(NA, nrow=n, ncol=k-1)
    for (j in 1:(k-1)) {
      temp = sweep(x, 2, centers[j,], '-')
      dists[,j] = rowSums(temp^2)
    }
    dists = rowMins(dists)
    # Draw next center with probability proportional to dist^2
    cumdists = cumsum(dists)
    prop = runif(1, min=0, max=cumdists[n])
    centers[k,] = as.matrix(x[min(which(cumdists > prop)),])
  }
  return(centers)
}

clust_kpp = kmeans(di_z, kpp_init(di_z, 3), iter.max=100, algorithm='Lloyd')
clust_kpp$centers
sil2 = silhouette(clust_kpp$cluster, dis)
summary(sil2)
plot(sil2, col=1:length(diz_clussters$size), border=NA)

# 5.1 Tuning the parameter k
n_rows <- 21
mat = matrix(0,nrow = n_rows)
for (i in 2:n_rows){
  set.seed(321)
  clust_kpp = kmeans(di_z, kpp_init(di_z, i), iter.max=100, algorithm='Lloyd')
  sil = silhouette(clust_kpp$cluster, dis)
  mat[i] = mean(as.matrix(sil)[,3])
}
colnames(mat) <- c("Avg_Silhouette_Value")
mat
ggplot(data.frame(k=2:n_rows,sil=mat[2:n_rows]),aes(x=k,y=sil))+
  geom_line()+
  scale_x_continuous(breaks = 2:n_rows)

df <- data.frame(k=2:n_rows,sil=mat[2:n_rows])
plot_ly(df, x = ~k, y = ~sil, type = 'scatter', mode = 'lines', name='Silhouette') %>%
  layout(title="Average Silhouette Graph")

k <- 3
set.seed(31)
clust_kpp = kmeans(di_z, kpp_init(di_z, k), iter.max=200, algorithm="MacQueen")
sil3 = silhouette(clust_kpp$cluster, dis)
summary(sil3)
plot(sil3, col=1:length(clust_kpp$size))

# 9 Hierarchical Clustering
library(cluster)
divorce_sing = agnes(di_z, diss=FALSE, method='single')
divorce_comp = agnes(di_z, diss=FALSE, method='complete')
divorce_ward = agnes(di_z, diss=FALSE, method='ward')
sil_sing = silhouette(cutree(divorce_sing, k=3), dis)
sil_comp = silhouette(cutree(divorce_comp, k=3), dis)
# try 8 clusters
sil_ward = silhouette(cutree(divorce_ward, k=8), dis)
plot(sil_ward)
cutree(divorce_ward, k=8)

# install.packages("ggdendro")
library(ggdendro)
ggdendrogram(as.dendrogram(divorce_ward), leaf_labels=FALSE, labels=FALSE)
mean(sil_ward[,"sil_width"])
ggdendrogram(as.dendrogram(divorce_ward), leaf_labels=TRUE, labels=T, size=10)
summary(sil_ward)
plot(sil_ward, col=1:length(unique(sil_ward[,1])))

