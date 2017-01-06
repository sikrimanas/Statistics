# Model Selection in R ###
# Author : Manas Sikri
# Date: 22-Dec-2016

# this document will discuss various techniques on how to select models in Machine Learning

# install the required library
install.packages("ISLR")
library(ISLR)

# we will be using the baseball dataset in the ISLR package
View(Hitters)
summary(Hitters) # summary of the dataset
?Hitters # description about baseball dataset

# there are some NA's in the dataset
# remove the NA's
Hitters = na.omit(Hitters)
# check and verify if there are still some in NA's in the modified dataset
with(Hitters,sum(is.na(Salary)))


# BEST SUBSET REGRESSION
# -------------------------------------
# we will use library 'leaps' for best subset model selection
install.packages("leaps")
library(leaps)

# function - regsubsets
regfit.full = regsubsets(Salary~., data = Hitters)
summary(regfit.full)
# it gives by default best-subsets upto size 8;

# increase it to 19
regfit.full = regsubsets(Salary~., data = Hitters, nvmax=19)
reg.summary = summary(regfit.full)
# variables in the summary can be displayed with the help of names function
names(reg.summary)

# plot the cp variable - estimate of prediction error
plot(reg.summary$cp
     , xlab = "Number of Variables"
     , ylab = "Cp")
# you can check with the function which.min
which.min(reg.summary$cp)

# plot for best-subset
points(10, reg.summary$cp[10], pch=20, col="red")

# there is plot object for 'regsubsets' object
plot(regfit.full, scale = "Cp")
# having chosen the model 10, we have coeff method
coef(regfit.full, 10) # it gives coeff of all ten variables involved in the model


# Forward Stepwise Selection
# -----------------------------------------

# we use 'regsubsets' function  by specifying the method equal to forward option
regfit.fwd = regsubsets(Salary~.
                        ,data=Hitters
                        ,nvmax=19
                        ,method = "forward")
summary(regfit.fwd)
plot(regfit.fwd, scale = "Cp")


# Model Selection using a Validation Set
# ---------------------------------------------
# make a training and validation dataset to choose a good model

# a slightly different approach to split
dim(Hitters)    # check the dimensions for hitters datset
set.seed(1)

# we will go for approximately 2/3 training and 1/3 test
train = sample(seq(263), 180, replace = FALSE) # creates 180 random number from 1 to 263
View(train)
# fit the model with training dataset
regfit.fwd = regsubsets(Salary~.
                        , data = Hitters[train,]
                        , nvmax = 19
                        , method = "forward")

# now we will make predictions using data not used in training
# and we have 19 models
# we will create a vector to capture the errors
val.errors = rep(NA,19)
# predict
x.test = model.matrix(Salary~., data = Hitters[-train,])
for(i in 1:19){
  coefi=coef(regfit.fwd,id=i)
  pred=x.test[,names(coefi)]%*%coefi
  val.errors[i]=mean((Hitters$Salary[-train]-pred)^2)
}

# plot the MSE
plot(sqrt(val.errors)
     , ylab = "Root MSE"
     , ylim = c(250,450)
     , pch = 19
     , type = "b")
# now plot residual sum of squares
points(sqrt(regfit.fwd$rss[-1]/180)
       ,col="blue"
       ,pch=19
       ,type="b")
# annotate the model
legend("topright",legend=c("Training","Validation"),col=c("blue","black"),pch=19)

# we will write a method to predict
predict.regsubsets=function(object,newdata,id,...){
  form=as.formula(object$call[[2]])
  mat=model.matrix(form,newdata)
  coefi=coef(object,id=id)
  mat[,names(coefi)]%*%coefi
}

# Model Selection By Cross Validation
# ----------------------------------------

# we will do 10 fold cross validation
# set seeds
set.seed(11)
folds = sample(rep(1:10, length = nrow(Hitters)))
folds
table(folds)
# matrix for error calculation containing all NAs
cv.errors = matrix(NA,10,19)

for(k in 1:10){
  best.fit=regsubsets(Salary~.,data=Hitters[folds!=k,],nvmax=19,method="forward")
    for(i in 1:19){
      pred=predict(best.fit,Hitters[folds==k,],id=i)
      cv.errors[k,i]=mean((Hitters$Salary[folds==k]-pred)^2)
  }
}

# average the columns
rmse.cv = sqrt(apply(cv.errors,2,mean))
# plot
plot(rmse.cv, pch = 19, type = "b")


# Ridge Regression and Lasso
# ----------------------------------------------

# we will use the package "glmnet" which does not use model formula
# so we need to setup on 'x' and 'y'
install.packages("glmnet")
library(glmnet)
x = model.matrix(Salary~.,-1, data = Hitters)
y = Hitters$Salary

# first, we will fit the ridge-regression model
# calling glmnet with 'alpha=0'

# fyi - alpha = 0 is ridge and alpha = 1 is lasso
fit.ridge = glmnet(x,y,alpha = 0)
plot(fit.ridge, xvar = "lambda", label = TRUE)
# cross validation
cv.ridge = cv.glmnet(x,y,alpha = 0)
# plot
plot(cv.ridge)


# now we fit a lasso model using alpha = 1
fit.lasso = glmnet(x,y)
plot(fit.lasso, xvar = "lambda", label = TRUE)
# cross validation
cv.lasso = cv.glmnet(x,y)
plot(cv.lasso)
# coefficient
coef(cv.lasso)


#Suppose we want to use our earlier train/validation division to 
#select the `lambda` for the lasso.
#This is easy to do.

lasso.tr=glmnet(x[train,],y[train])
lasso.tr
pred=predict(lasso.tr,x[-train,])
dim(pred)
rmse= sqrt(apply((y[-train]-pred)^2,2,mean))
plot(log(lasso.tr$lambda),rmse,type="b",xlab="Log(lambda)")
lam.best=lasso.tr$lambda[order(rmse)[1]]
lam.best
coef(lasso.tr,s=lam.best)
