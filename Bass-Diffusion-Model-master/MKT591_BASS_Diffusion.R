### 1.iphone example
iphone_data = read.csv("sample_iphone_sales.csv",header = TRUE)
Sales = ts(iphone_data$Sales,start=c(2007,4),frequency = 4)


###############Bass Model from here##########
#plot the data
plot(Sales,col="red",type="l",lty=2)
points(Sales,pch=20,col="blue")
title("Cumulative iPhone Sales(millions)")


#plot cumulative sales
Y=cumsum(Sales)
Y=ts(Y,start = c(2007,3), frequency = 4)
plot(Y,type="l",lty=2,col="red")
points(Y,pch=20,col="blue")
title("Cumulative iPhone Sales(millions)")

### fit BASS regression and compute m.p.q
#first let's estimate a,b and c using linear regression:
Y = c(0,Y[1:(length(Y)-1)]) #we want Y_t-1 instead of Y_t
Ysq = Y^2
out = lm(Sales ~ Y +Ysq)   ##S(T) = a +bY(T)+c(Y(T))^2
summary(out)

a=out$coef[1]
b=out$coef[2]
c=out$coef[3]
# now we know a,b,c


mplus = (-b+sqrt(b^2-4*a*c))/(2*c) #m plus
mminus = (-b-sqrt(b^2-4*a*c))/(2*c) #m minus

#actually m is minus in the equation so let's select mminus
m=mminus
p=a/m
q=b+p


### External Effects and Internal Effects ###
Ext = NULL

for(t in 1:length(Y)){
  Ext <- c(Ext,p*(m-Y[t]))
}

plot(Ext,type = "l")


Int = NULL


for(t in 1:length(Y)){
  Int <- c(Int,q*(Y[t]/m)*(m-Y[t]))
}

plot(Int,type = 'l')



###prediction by using Bass Model function

## prediction model using p,q,m

Bass_Model = function(p,q,m,T=50){
  
  S=double(T)
  Y=double(T+1)
  Y[1]=0 #starting value
  
  
  for(t in 1:T){
    
    S[t] = p*m+(q-p)*Y[t]-(q/m)*Y[t]^2 #estimated Sales
    Y[t+1] = Y[t]+S[t]
  } #for t by here
  
  return(S)
  #return(list(sales=S,cumSales=cumsum(S)))
}###Bass_Model Function



#####prediction and Plots per each datasets#######
#### 1. for iPhone case #######
Spred = Bass_Model(p,q,m,T=25)
Spred = ts(Spred,start = c(2007,3),freq=4)
ts.plot(Sales,Spred,col=c("blue","red")) #actual sales and estimated sales(spread)
legend("topleft",legend=c("actual","Bass Model"), fill=c("blue","red"))

##for cummulative sales
Spred = Bass_Model(p,q,m)
CumSpred=ts(cumsum(Spred),start = c(2007,3),frequency = 4)
CumSales=ts(cumsum(Sales),start = c(2007,3),frequency = 4)
ts.plot(CumSales,CumSpred,col=c("blue","red"))
title("Prdeicted Cumulative iPhone Sales")
