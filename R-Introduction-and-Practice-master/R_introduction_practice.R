LifeStyle = read.csv("LifeStyle.csv",head=TRUE)

write.table(LifeStyle,file = "lifestyle.csv",sep = ',')

x1 <- c(1,2,3)

x2 <- rep(1,3)

Xr <- rbind(x1,x2)

Xc <- cbind(x1,x2)

is.numeric(x1)
is.logical(x1)
is.character(x1)

x_logic <- (x1==1)
is.logical(x_logic)

x_char = c("Arizona","State","University")
is.character(x_char)

x_list = list(x1,x_char)

x_list[1]
x_list[[1]]
x_list[[1]][1]


xseq <- 1:5
xseq*2

1:5*2
1:(5*2)

x4=sample(10)
x6=sample(10,replace = TRUE)

x5=sample(c(0,1),10,replace = TRUE)

x4[1]
x4[1:5]
x5_1 = which(x5==1)
x5_1
x6_max=which.max(x6)
x6_max







Xn1=rnorm(1000,0,1)
hist(Xn1)
plot(density(Xn1))


x7=1:10

for(i in 1:10){
  x7[i] = x7[i] + 1
}

x8 = 0

for(i in 1:10){
  x8=x8+i
}

x8


Ns=100

CLT_x = matrix(0,1000)

for(l in 1:1000){
  CLT_x[l]=mean(runif(Ns,-5,5))
}

hist(CLT_x)

