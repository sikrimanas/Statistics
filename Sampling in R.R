# Re-sampling in R #
# Author : Manas Sikri
# Date: 12/16/2016

# install the required packages for cross validation and bootstrap
require(ISLR) # contains our dataset
require(boot)

?cv.glm # This function calculates the estimated K-fold cross-validation prediction error 
        # for generalized linear models
# auto dataset is used
View(Auto)
# plot for auto data
plot(mpg~horsepower,data=Auto)

## LOOCV
# Leave-one-out cross-validation
# first, fit the linear model
glm.fit=glm(mpg~horsepower, data=Auto)
# execute the LOOCV type validation
cv.glm(Auto,glm.fit)$delta #pretty slow 

# it gives cross validation prediction error

##Lets write a simple function to use formula (5.2)
loocv=function(fit){
  h=lm.influence(fit)$h
  mean((residuals(fit)/(1-h))^2)
}

## Now we try it out
loocv(glm.fit)

# a vector with all zeroes
cv.error=rep(0,5)
# degree of polynomials
degree=1:5
# function to fit the model with increased degree of polynomial
for(d in degree){
  glm.fit=glm(mpg~poly(horsepower,d), data=Auto)
  cv.error[d]=loocv(glm.fit)
}
# plotting degree vs error
plot(degree,cv.error,type="b")

## 10-fold CV

# create a vector with 10 empty value containers
cv.error10=rep(0,5)
for(d in degree){
  # fit the model using glm
  glm.fit=glm(mpg~poly(horsepower,d), data=Auto)
  # using cv.glm function to calculate the errors
  cv.error10[d]=cv.glm(Auto,glm.fit,K=10)$delta[1]
}
lines(degree,cv.error10,type="b",col="red")


## Bootstrap
## Minimum risk investment - Section 5.2

alpha=function(x,y){
  vx=var(x) # variance of x
  vy=var(y) # variance of y
  cxy=cov(x,y) # covariance of x and y
  (vy-cxy)/(vx+vy-2*cxy)
}
# call the function
alpha(Portfolio$X,Portfolio$Y)

## What is the standard error of alpha?

alpha.fn=function(data, index){
  with(data[index,],alpha(X,Y))
}

# calling the function
alpha.fn(Portfolio,1:100)

# Use bootstrap now
# set the seed
set.seed(1)
alpha.fn (Portfolio,sample(1:100,100,replace=TRUE))

# we tell it to do a thousand bootstraps
boot.out=boot(Portfolio,alpha.fn,R=1000)
boot.out
plot(boot.out)
