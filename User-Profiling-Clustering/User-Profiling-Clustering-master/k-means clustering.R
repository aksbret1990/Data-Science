set.seed(100)

seg.raw <- read.csv("UserID_Assignmentdata.csv")
head(seg.raw)
seg.df <- seg.raw[,-1]
summary(seg.df)
head(seg.df)
library(cluster)


# computing segment-level mean values
seg.summ <- function(data,groups){
  aggregate(data, list(groups), function(x) mean(as.numeric(x)))
}


############ K-means  #################
mydata <- seg.df
wss <- (nrow(mydata)-1)*sum(apply(mydata,2,var))
for (i in 2:15) wss[i] <- sum(kmeans(mydata, centers=i)$withinss)
plot(1:15, wss, type="b", xlab="Number of Clusters", ylab="Within groups sum of squares")


seg.df.num <- seg.df 
seg.k <- kmeans(seg.df.num, centers = 5)
seg.k$cluster
table(seg.k$cluster)
summary(seg.df.num)
seg.summ(seg.df.num,seg.k$cluster)
seg.k$centers
seg.k$totss
seg.k$withinss
seg.k$tot.withinss
seg.k$betweenss


library(fpc)
plotcluster(seg.df.num, seg.k$cluster, xlab = "", ylab = "")

# More complex
clusplot(seg.df.num, seg.k$cluster, color=TRUE, shade=TRUE, labels=2, lines=0)


#boxplot in terms of coins according to clusters
boxplot(seg.df.num$coins ~ seg.k$cluster, ylab = "coins", xlab = "Cluster")




#convert to a data frame
membership = cbind(c(1:8240),seg.k$cluster)
membership = data.frame(membership)


#rename columns
names(membership)[1] <- "user_id"
names(membership)[2] <- "cluster"

#write to a csv file
write.table(membership, file = "cluster_membership.csv", row.names = FALSE, sep=",")

