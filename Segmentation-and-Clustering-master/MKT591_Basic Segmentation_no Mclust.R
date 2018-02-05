############Segmentation Methods (Book)############

#subscription based service(such as cable television or membership in a warehouse club
# age, gender, income, number of kids, rent or own homes r currently subscribe or not... see( 5.1.4 in the book) )

seg.raw <- read.csv("http://goo.gl/qw303p")
seg.df <- seg.raw[,-7]
summary(seg.df)


head(seg.df)

seg.summ <- function(data,groups){
  aggregate(data, list(groups), function(x) mean(as.numeric(x)))
}# computing segment-level mean values

##########heirarchical clustering (page 390)##################
################ distance based ##########################

library(cluster)

seg.dist <- daisy(seg.df) #compute dissimilarity matrix, default=euclidean distance
#daisy: The handling of nominal, ordinal, and (a)symmetric binary data is
#achieved by using the general dissimilarity coefficient of Gower(1971).

as.matrix(seg.dist)[1:5,1:5]
dim(as.matrix(seg.dist)) #distance between 300 members

seg.hc <- hclust(seg.dist, method = "complete")
# complete linkage method evaluates the distance between every member
plot(seg.hc) #resulting tree for all N=300 observations of seg.df

## decide number of segments based on dendrogram
cut(as.dendrogram(seg.hc),h = 0.7)

cut(as.dendrogram(seg.hc),h = 0.5) #cut-point 0.5 or 0.7?


plot(cut(as.dendrogram(seg.hc), h = 0.5)$lower[[1]]) #cut with 0.5 in the plot


### Check Similarity ###
seg.df[c(101,107),]

### Specifying the number of groups we want ###
plot(seg.hc)

rect.hclust(seg.hc, k = 4, border = "red")  #prespecified k=4

#assignment vector
seg.hc.segment <- cutree(seg.hc, k = 4) #membership for 4 groups


seg.hc.segment[1:100]
table(seg.hc.segment)


seg.summ(seg.df,seg.hc.segment)
#computing segment-level mean values

plot(jitter(as.numeric(seg.df$gender)) ~
       jitter(as.numeric(seg.df$subscribe)),
     col = seg.hc.segment, yaxt = "n", xaxt = "n", ylab = "", xlab = "")
axis(1, at=c(1,2), labels = c("Subscribe:No","Subscribe:Yes"))
axis(2, at=c(1,2), labels = levels(seg.df$gender))


############ K-means (page 311)  #################
############ distance based ########################

seg.df.num <- seg.df #change categorical items to numeric values
seg.df.num$gender <- ifelse(seg.df$gender == "Male",0,1)
seg.df.num$ownHome <- ifelse(seg.df$ownHome == "ownNO",0,1)
seg.df.num$subscribe <- ifelse(seg.df$subscribe == "subno",0,1)

seg.k <- kmeans(seg.df.num, centers = 4)

seg.k$cluster

seg.summ(seg.df,seg.k$cluster)

#boxplot in terms of income
boxplot(seg.df.num$income ~ seg.k$cluster, ylab = "Income", xlab = "Cluster")




######### Class Example Toothpaste##############3

#### Heirarchical Clustering


Tooth <- read.csv("Toothpaste.csv", header = TRUE)

tooth.dist <- daisy(Tooth) #works with mixed data types by rescaling the values
as.matrix(tooth.dist)[1:5,1:5]
dim(as.matrix(tooth.dist)) #distances between 30 members

tooth.hc <- hclust(tooth.dist, method = "complete")
plot(tooth.hc) #resulting tree for all N=30 members of toothpaste



### let's think about how many clusters from the plot...
rect.hclust(tooth.hc, k=3, border = "red") #prespecified k=3

#assignment vector
tooth.hc.segment <- cutree(tooth.hc,k = 3) #membership for 3 groups

tooth.hc.segment
table(tooth.hc.segment)


############# K- means (distance based) #############
tooth.k <- kmeans(Tooth,centers = 3)

tooth.k$cluster
table(tooth.k$cluster)

#boxplot in terms of "prevent capacity"
boxplot(Tooth$Prevent ~ tooth.k$cluster, ylab = "Prevent" , xlab = "Cluster")

seg.summ(Tooth, tooth.k$cluster)









#############Latent Class Regression##########
############# Flexmix package ##############

library(flexmix)
library(MASS)
source("Reordering.R")


##########simulation study - Simulation data generation##############
## (N=200, k=2 p=10)
###




p=10 ; n=200; k=2; d_t = rep(1/k,k)
x=matrix(0,n,p)
tau2_t=1;
z_t=matrix(0,p,k)
mu_t=matrix(0,p,k)


z_t[,1]=c(rep(1,2),rep(0,8))
z_t[,2]=c(rep(0,2),rep(1,2),rep(0,6))
#z_t[,3]=c(rep(1,3),rep(0,18))

mu_t[,1]=c(rep(1,4),rep(0,6))
mu_t[,2]=c(rep(0,4),rep(1.5,4),rep(0,2))
#mu_t[,3]=c(rep(1.5,2),rep(0.4,11),rep(0,18))

H_t=sample(1:k,n,replace = TRUE,d_t)

newp=reorder(mu_t,H_t,z_t,d_t)
mu_t=newp$mu
H_t=newp$H
z_t=newp$z
d_t=newp$d

## Usually used in mixture type models for preventing label switching issue
## (Not covered in this course)

Y=rep(0,n)
X=mvrnorm(n,rep(0,p),diag(1,p))

#sig2_t=sum(mu_t^2/k/40)
sig2_t=0.1

for(i in 1:n)
  Y[i] = X[i,]%*%mu_t[,H_t[i]]+rnorm(1,0,sqrt(sig2_t))


## running linear model##
result1=lm(Y~0+X)
summary(result1)

##running flemix#
m1<-flexmix(Y~0+X,k=1)
rm1<-refit(m1)
summary(rm1) #k=1 Model

m2<-flexmix(Y~0+X,k=2)
rm2<-refit(m2)
summary(rm2) #k=2 Model
posterior(m2) #to know membership

m3<-flexmix(Y~0+X,k=3)
rm3<-refit(m3)
summary(rm3) #k=3 Model
