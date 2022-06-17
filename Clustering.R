#Summary of Marketing Data

seg.df <- read.csv("customer_data(1).csv")

#convert character to factor 
seg.df$gender<-as.factor(seg.df$gender)
seg.df$ownHome<-as.factor(seg.df$ownHome)
seg.df$subscribe<-as.factor(seg.df$subscribe)

summary(seg.df)

# Hierarchical Clustering
install.packages("cluster")
library(cluster)                  
ex.dist <- daisy(seg.df) # compute distance matrix
ex.hc   <- hclust(ex.dist) #run hierarchical clustering

#First plot dendrogram without splitting into groups
plot(ex.hc)

# see hclust's proposal for 4 groups (note: many possible answers)
rect.hclust(ex.hc, k=4, border="red")

#Save the group for each observation
seg.ex.segment <- cutree(ex.hc, k=4)

# Number of observations in each group
table(seg.ex.segment)

aggregate(. ~ seg.ex.segment, data=seg.df, mean) 

#plot to see gender by subscribe status
plot(jitter(as.numeric(seg.df$gender)) ~ jitter(as.numeric(seg.df$subscribe)), 
     col=seg.ex.segment, yaxt="n", xaxt="n", ylab="", xlab="")
axis(1, at=c(1, 2), labels=c("Subscribe: No", "Subscribe: Yes"))
axis(2, at=c(1, 2), labels=levels(seg.df$gender))

#### K-MEANS
# convert factor variables to numeric
seg.df.num <- seg.df
seg.df.num$gender    <- ifelse(seg.df$gender=="Male", 0, 1)
seg.df.num$ownHome   <- ifelse(seg.df$ownHome=="ownNo", 0, 1)
seg.df.num$subscribe <- ifelse(seg.df$subscribe=="subNo", 0, 1)
summary(seg.df.num)


set.seed(943)
seg.k <- kmeans(seg.df.num, centers=4,nstart=25) #run kmeans clustering
seg.k$centers

# plot one of the variables
boxplot(seg.df.num$income ~ seg.k$cluster, ylab="Income", xlab="Cluster")

#clustering algorithms and visualization
#install.packages("factoextra")

library(factoextra)
fviz_cluster(seg.k, data = seg.df.num)


