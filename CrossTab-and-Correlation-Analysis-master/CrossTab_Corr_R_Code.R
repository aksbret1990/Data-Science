CrossTab = read.csv("CrossTab_Example.csv",header = TRUE)

AccountSize <- table(CrossTab[,2])

dimnames(AccountSize) <- list(c("Small","Medium","Large"))

Recomm <- table(CrossTab[,3])

dim(CrossTab)
head(CrossTab)
tail(CrossTab)

library(car)
some(CrossTab)
summary(CrossTab)

n = dim(CrossTab)[1]
table(CrossTab[,2])/n
  

mytable = t(table(CrossTab[,2],CrossTab[,3]))
chisq.test(mytable)


library(gmodels)

CrossTable(CrossTab[,2],CrossTab[,3],dnn=c("AcctSize","Recomm"))

AccountSize = CrossTab[,2]
AccountSize[AccountSize==1]="Small"
AccountSize[AccountSize==2]="Medium"
AccountSize[AccountSize==3]="Large"


Recomm = CrossTab[,3]
Recomm[Recomm==0]="Not_Recomm"
Recomm[Recomm==1]="Yes_Recomm"


CrossTable(AccountSize,Recomm,chisq = TRUE,expected = TRUE, dnn = c("AcctSize","Recomm"))



Corr = read.csv("Correlation_Example.csv",header = TRUE)
price <- Corr[,2] #price
sales <- Corr[,3] #sales

plot(price,sales) #scatter plot
cor(price,sales) #sample correlation

## for statistical testing
library(Hmisc)

rcorr(cbind(price,sales),type="pearson") #statistical testing

