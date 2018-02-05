
#salary example for dummy variables
SalaryData = read.csv("Salary.csv",head=TRUE)
Salary = SalaryData[,2]
Age = SalaryData[,3]
GenderFactor = as.factor(SalaryData[,4])

res1 = lm(Salary ~ Age+GenderFactor)
summary(res1)

