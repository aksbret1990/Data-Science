#simple linear regression
#amusement park data provided in book

sat.df <- read.csv("http://goo.gl/HKnl74")
dim(sat.df)
str(sat.df)

head(sat.df)


library(gpairs)
gpairs(sat.df)

sat.df$logdist <- log(sat.df$distance)
hist(sat.df$logdist)

library(corrplot)

#running regular linear model with one independent variable
attach(sat.df)

m1=lm(overall ~ rides) #regression with one independent variable
sum_m1 = summary(m1)
names(sum_m1)

#plot for model fit
plot(overall ~ rides, data = sat.df,xlab="Satisfaction with Rides",ylab="Overall Satisfaction")
abline(m1)

plot(m1$fitted.values)


#rsquare--- how much variance can be explained by linear model
sum_m1$r.squared

#ANNOVA
sum_m1$coefficients


#prediction
betas = m1$coefficients
new_value=95

sum(betas[1]+betas[2]*new_value)

confint(m1)






#Multiple Linear Regression Model
#real estate example

realestate  = read.csv("RealEstate_ExampleData.csv")
Y=realestate[,2]
sqft = as.numeric(realestate[,3])
age=as.numeric(realestate[,4])

m2_1 <- lm(Y ~ sqft + age)
summary(m2_1)

### In-Class amusement park example

m2 <- lm(overall ~ rides + games + wait + clean, data = sat.df)
summary(m2)

library(coefplot)
coefplot(m2, intercept = FALSE ,ylab="Features", xlab="Relationships with DV")


#find better lm model
##standardizing predictor -- scale

sat.std <- sat.df[,-3] #exclude the 3rd column
sat.std[,3:8] <- scale(sat.std[,3:8])

m3 <- lm(overall ~ rides + games + wait + clean + weekend + logdist + num.child, data = sat.std )
summary(m3)
factor(sat.std$num.child)
sat.std$num.child.factor <- factor(sat.std$num.child)



####discrete independent variable
m4 <- lm(overall ~ rides + games + wait + clean + weekend + logdist + num.child.factor, data = sat.std )
summary(m4)

AIC(m2)
AIC(m3)
AIC(m4)


BIC(m2)
BIC(m3)
BIC(m4)


#model with binary has.child variable
sat.std$has.child <- factor(sat.std$num.child > 0)
m5 <- lm(overall ~ rides + games + wait + clean + weekend + logdist + has.child, data = sat.std )
summary(m5)


AIC(m5)
BIC(m5)




#interaction terms
m6 <- lm(overall ~ rides + games + wait + clean + weekend + logdist + has.child 
         +rides:has.child + games:has.child + wait:has.child +
           clean:has.child + rides:weekend + games:weekend +
           wait:weekend + clean:weekend, data = sat.std )

summary(m6)


m7 <- lm(overall ~ rides + games + wait + clean + weekend + logdist + has.child 
         + wait:has.child, data = sat.std )

summary(m7)

AIC(m7)
BIC(m7)




