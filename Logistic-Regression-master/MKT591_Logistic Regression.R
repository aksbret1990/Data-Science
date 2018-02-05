XBeta = -10
exp(XBeta)/(exp(XBeta+1)) # for logistics -- prob

plogis(-Inf) # here, plogis=exp(x)/(exp(x)+1)
plogis(2)
plogis(0)


#let's see how prob looks like
XB = seq(-10,10,by=0.01)
logist=plogis(XB)
plot(logist,type = "l",xlab = "XB",ylab = "probability" )


## logit ##
log(.88/(1-.88))
qlogis(.88)


#In Class Catalog Example
Catalog <- read.csv("Catalog.csv",header = TRUE)
attach(Catalog)


res_Catalog <- glm(Choice ~ Recency + Frequency + Monetary, family = binomial("logit"))
summary(res_Catalog)


#prediction: internal sample
n=dim(Catalog)[1] #sample size
pred = predict.glm(res_Catalog,Catalog,type = "response")

pred.C = rep(0,n)
pred.C[pred>0.5]=1
table(pred.C) #frequency table for predicted choice
#apply Crosstab
table(Catalog[,5],pred.C)

##prediction for new data
NewData1=data.frame(4,9,66.71)
colnames(NewData1)=c("Recency", "Frequency", "Monetary")
predict(res_Catalog,NewData1,type = "link") #log odds
predict(res_Catalog,NewData1,type = "response") #predicted probability


NewData1=data.frame(5,80,70)
colnames(NewData1)=c("Recency", "Frequency", "Monetary")
predict(res_Catalog,NewData1,type = "link") #log odds
predict(res_Catalog,NewData1,type = "response") #predicted probability



#Customer Acquistion Example
JDPowers <- read.csv("JDPowers.csv",header = TRUE)
attach(JDPowers)

Email = as.factor(Email)
Coupon = as.factor(Coupon)

res_JD <- glm(Customer ~ Distance+Billboard+Email+Coupon, family = binomial)
summary(res_JD)

NewData2 = data.frame(2,30,as.factor(0),as.factor(1))
colnames(NewData2) = c("Distance","Billboard","Email","Coupon")
predict(res_JD,NewData2,type = "response")


#########################Bank Marketing Data##########################
##############################################################################
bank=read.csv("bank-additional-full.csv")
head(bank)

###split data to train and validation dataset
###data segmentation data loading


train.prop = .75 #set 75% for training dataset
train.cases  = sample(nrow(bank),nrow(bank)*train.prop)

length(train.cases)
head(train.cases) #checking data

##using age, material, housing, duration

class.train = bank[train.cases,c(1,3,6,11,14,21)]
class.valid = bank[-train.cases,c(1,3,6,11,14,21)]

class.train$marital <- as.factor(class.train[,2])
class.train$housing <- as.factor(class.train[,3])

Y.train = class.train[,6]
X.train = class.train[,-6]

n=dim(X.train)[1] #number of observations


##training Data Set#
fit.logit <- glm(y ~ age + marital + housing + duration, data = class.train, family = binomial("logit"))


##prepare dummy IVs for prediction
##(for categorical variable)##


pred.train = predict(fit.logit, class.train[,-6],type = 'response' ) ##check here
pred.train <- ifelse(pred.train > 0.5,1,0)
ct = table(Y.train,pred.train)
diag(prop.table(ct,1))
sum(diag(prop.table(ct)))
 

### validation data set - prediction hitting for external samples #
class.valid$marital <- as.factor(class.valid[,2])
class.valid$housing <- as.factor(class.valid[,3])


pred.valid = predict(fit.logit, class.valid[,-6], type = "response")  ##check here
pred.valid = ifelse(pred.valid > 0.5,1,0)

ctv = table(class.valid[,6],pred.valid)
diag(prop.table(ctv,1))
sum(diag(prop.table(ctv)))



### CrossTab --- Jaccard similarity
table(bank[,21])/dim(bank)[1]

library(gmodels)
Cross1 = CrossTable(class.valid[,6],pred.valid)


CrossTv = table(class.valid[,6],pred.valid)
CrossTv[2,2]/(dim(class.valid)[1] - CrossTv[1,1])
