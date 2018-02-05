grocery=read.csv("grocery.csv")
head(grocery)

###split data to train and validation dataset
###data segmentation data loading
train.prop = .75 #set 75% for training dataset
train.cases  = sample(nrow(grocery),nrow(grocery)*train.prop)

length(train.cases)
head(train.cases) #checking data

##using age, material, housing, duration

class.train = grocery[train.cases,c(2,3,4,6,7,8,9,10,14,15)]
class.valid = grocery[-train.cases,c(2,3,4,6,7,8,9,10,14,15)]

class.train$HealthyStore <- as.factor(class.train[,1])
class.train$StoreSize <- as.factor(class.train[,2])
class.train$StoreLayout <- as.factor(class.train[,3])
class.train$gender <- as.factor(class.train[,4])
class.train$WhoShoppingFor <- as.factor(class.train[,5])
class.train$Vegetarian <- as.factor(class.train[,6])
class.train$ShoppingStyle <- as.factor(class.train[,7])
class.train$CouponUser <- as.factor(class.train[,8])
class.train$CouponValue <- as.factor(class.train[,9])



#running a multiple linear regression model
m1 <- lm(AmountSpent ~ ., data = class.train)
summary(m1)

m2 <- lm(AmountSpent ~ StoreLayout + HealthyStore + WhoShoppingFor, data = class.train)
summary(m2)

AIC(m1)
AIC(m2)

#predict values in the validation data set
predict.valid <- predict(m1,class.valid)
predict.valid
class.valid[,10]

#calculate accuracy of multiple linear regression model
initial_accuracy = accuracy(predict.valid,class.valid[,10])


#perform stepwise regression
step_both <- stepAIC(m1, direction = "both")
summary(step_both)
AIC(step_both)


#predict values in the validation data set
predict.valid <- predict(step_both,class.valid)
predict.valid
class.valid[,10]

#calculate accuracy of stepwise regression model
final_accuracy = accuracy(predict.valid,class.valid[,10])


initial_accuracy
final_accuracy




