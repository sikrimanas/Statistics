# Support Vector Machine
# Author: Manas Sikri
# Date : 27-Dec-2016

# Support Vector Machine - SVM
# -------------------------------------------------------------

# Linear SVM classifier
# ---------------------
# Lets generate some data in two dimensions, and make them a little separated.

set.seed(10111)
# first variable
a = matrix(rnorm(40),20,2)
# second variable
b = rep(c(-1,1), c(10,10))
a[b==1,]=a[b==1,]+1
# plot
plot(a,col=b+3,pch=19)

# package used 'e1071' for SVM
install.packages("e1071")
library(e1071)
# create data frame 
dat=data.frame(a,b=as.factor(b))
# fit the svm model
svmfit = svm(b~.
             ,data=dat
             ,kernel="linear"     # kernel method
             ,cost=10             # tuning parameter
             ,scale=FALSE)
print(svmfit)
plot(svmfit,dat)


make.grid=function(x,n=75){
  grange=apply(x,2,range)
  x1=seq(from=grange[1,1],to=grange[2,1],length=n)
  x2=seq(from=grange[1,2],to=grange[2,2],length=n)
  expand.grid(X1=x1,X2=x2)
}
xgrid=make.grid(a)
ygrid=predict(svmfit,xgrid)
# plot
plot(xgrid
     ,col=c("red","blue")[as.numeric(ygrid)]
     ,pch=20
     ,cex=.2)
# add the points to it
points(a,col=b+3,pch=19)
points(a[svmfit$index,],pch=5,cex=2)


# extract the linear coefficients, and then using simple algebra, 
# we include the decision boundary and the two margins

beta=drop(t(svmfit$coefs)%*%a[svmfit$index,])
beta0=svmfit$rho
# plot
plot(xgrid,col=c("red","blue")[as.numeric(ygrid)],pch=20,cex=.2)
# add points
points(a,col=b+3,pch=19)
# add points
points(a[svmfit$index,],pch=5,cex=2)
# add the sperator line
abline(beta0/beta[2],-beta[1]/beta[2])
# add margin
abline((beta0-1)/beta[2],-beta[1]/beta[2],lty=2)
# add margin
abline((beta0+1)/beta[2],-beta[1]/beta[2],lty=2)


# Non Linear Support Vector
# --------------------------------------------------------------

# We will use the mixture data from ESL

# load
load(url("http://www.stanford.edu/~hastie/ElemStatLearn/datasets/ESL.mixture.rda"))
names(ESL.mixture)
rm(x,y)
attach(ESL.mixture)

# These data are also two dimensional. 
# Lets plot them and fit a nonlinear SVM, using a radial kernel.

plot(x,col=y+1)
dat=data.frame(y=factor(y),x)
fit=svm(factor(y)~.,data=dat,scale=FALSE,kernel="radial",cost=5)

# Now we are going to create a grid, as before, and make predictions on the grid
xgrid=expand.grid(X1=px1,X2=px2)
ygrid=predict(fit,xgrid)
plot(xgrid,col=as.numeric(ygrid),pch=20,cex=.2)
points(x,col=y+1,pch=19)

# Bayes Decision Boundaries
func=predict(fit,xgrid,decision.values=TRUE)
func=attributes(func)$decision
xgrid=expand.grid(X1=px1,X2=px2)
ygrid=predict(fit,xgrid)
plot(xgrid,col=as.numeric(ygrid),pch=20,cex=.2)
points(x,col=y+1,pch=19)

contour(px1,px2,matrix(func,69,99),level=0,add=TRUE)
contour(px1,px2,matrix(prob,69,99),level=.5,add=TRUE,col="blue",lwd=2)
