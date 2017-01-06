# Regression Model in R ###
# Author : Manas Sikri
# Date: 13-Dec-2016

# required library
install.packages("MASS") # this package has some dataset which we will be using
library(MASS)
install.packages("ISLR")
library(ISLR)

### Simple linear regression
# using the already loaded boston dataset in R
# names of the variables in boston dataset
names(Boston)
# view the dataset
View(Boston)

?Boston

# plot medv versus lstat
plot(medv~lstat,Boston)
# fit the linear model
fit1=lm(medv~lstat,data=Boston)
# check the model
fit1
# summary of the fit
summary(fit1)

# perfect fit line for the model using abline
abline(fit1,col="red")
# check components of the model
names(fit1)
# confidence interval
confint(fit1)

# predict function to predict the values corresponding to new lstat values
predict(fit1
        ,data.frame(lstat=c(5,10,15))
        ,interval="confidence")


# Now lets look at the Multiple Linear Regression
### Multiple linear regression
fit2=lm(medv~lstat+age,data=Boston)
summary(fit2)

# third fit taking all the variables
fit3=lm(medv~.,Boston)
summary(fit3)

# using the par function to make multiple plots in single screen
par(mfrow=c(2,2))
plot(fit3)

# fit4 which is an update to fit3
# -age means we want to remove age
# -indus means we want to remove indus
fit4=update(fit3,~.-age-indus)
summary(fit4)

### Nonlinear terms and Interactions
# interaction between lstat and age
fit5=lm(medv~lstat*age,Boston)
summary(fit5)

# In the below model, I is the identity function, which puts in the square of lstat
fit6=lm(medv~lstat +I(lstat^2),Boston); summary(fit6)

attach(Boston)
par(mfrow=c(1,1))
# plot the two varibales
plot(medv~lstat)

# now plot the points from the fitted value of fit6
# each value of lstat with fitted value of model
points(lstat,fitted(fit6),col="red",pch=20)

# there is an easier way of fitting polynomials with the help of poly function
fit7=lm(medv~poly(lstat,4))
# add that to the plot with color blue
points(lstat,fitted(fit7),col="blue",pch=20)

# different plotting methods
plot(1:20,1:20,pch=1:20,cex=2)

###Qualitative predictors
# lets work somewhat on qualitative predictors
fix(Carseats)
names(Carseats)
summary(Carseats)

# fitting the model
# sales with all variables
# in addition to it we will add interactions between income and advertising
# + age and price
fit1=lm(Sales~.+Income:Advertising+Age:Price,Carseats)
summary(fit1)

# contrasts function shows on how R will interpret the variable
contrasts(Carseats$ShelveLoc)

###Writing R functions
regplot=function(x,y){
  fit=lm(y~x)
  plot(x,y)
  abline(fit,col="red")
}
attach(Carseats)
regplot(Price,Sales)
regplot=function(x,y,...){
  fit=lm(y~x)
  plot(x,y,...)
  abline(fit,col="red")
}
regplot(Price,Sales,xlab="Price",ylab="Sales",col="blue",pch=20)




