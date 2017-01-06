# Classification in R ###
# Author : Manas Sikri
# Date: 13-Dec-2016

# install and load the required packages in R
require(ISLR)
# check the column names for stock market dataset
names(Smarket)
View(Smarket) # view the data
# check the summary of the dataset
summary(Smarket)
?Smarket # help

# plot the variable pairs in stock market dataset
pairs(Smarket,col=Smarket$Direction)

# Logistic regression
# glm fit function
# direction variable is the response which has two variables i.e. up and down
glm.fit=glm(Direction~Lag1+Lag2+Lag3+Lag4+Lag5+Volume  # function
            ,data=Smarket                              # dataset
            ,family=binomial)                          # type of family i.e. tells to fit logistic regression model
# summary of the model
summary(glm.fit)
# predict the model
glm.probs=predict(glm.fit,type="response") 
glm.probs[1:5]

# setting the thresholds for classification by using ifelse commands
glm.pred=ifelse(glm.probs>0.5,"Up","Down")
attach(Smarket)
# create a table for predicted as well as original values of direction variable
table(glm.pred,Direction)
mean(glm.pred==Direction) # mean of the proportion

# Make training and test set
# create a train data set
train = Year<2005
# fit the data in glm model
glm.fit=glm(Direction~Lag1+Lag2+Lag3+Lag4+Lag5+Volume
            ,data=Smarket
            ,family=binomial
            , subset=train)  # give the input as training

# predict for Snamrket dataset which has rows not in train dataset
glm.probs=predict(glm.fit
                  ,newdata=Smarket[!train,]
                  ,type="response") 

# perform the classification
glm.pred=ifelse(glm.probs >0.5,"Up","Down")

# test data
Direction.2005=Smarket$Direction[!train]
# create a tabular form
table(glm.pred,Direction.2005)
mean(glm.pred==Direction.2005)


#Fit smaller model
# we are going to fit lag1 and lag2 and leave other variables
glm.fit=glm(Direction~Lag1+Lag2
            ,data=Smarket
            ,family=binomial
            ,subset=train)
# predict
glm.probs=predict(glm.fit
                  ,newdata=Smarket[!train,]
                  ,type="response") 
# classification
glm.pred=ifelse(glm.probs >0.5,"Up","Down")
# table
table(glm.pred,Direction.2005)
# mean
mean(glm.pred==Direction.2005)
106/(76+106)

# Now lets try our hands on Linear Discriminant Analysis
require(MASS)

## Linear Discriminant Analysis
# fit the model with the help of lda function
lda.fit=lda(Direction~Lag1+Lag2
            ,data=Smarket
            ,subset=Year<2005)
# check the model
lda.fit
# plot
plot(lda.fit)

# get the test data
Smarket.2005=subset(Smarket,Year==2005)
# predict
lda.pred=predict(lda.fit,Smarket.2005)
lda.pred[1:5,]
# check for the class of lda.pred
class(lda.pred)
# convert it into a dataframe
data.frame(lda.pred)[1:5,]
# check the result
# it will give you a sort of confusion matrix
table(lda.pred$class,Smarket.2005$Direction)
mean(lda.pred$class==Smarket.2005$Direction)

## K-Nearest Neighbors
# load the library
library(class)
# help
?knn

# attach function makes available variables of the dataset by name
attach(Smarket)
# create a matrix for lag1 and lag2
Xlag=cbind(Lag1,Lag2)
# train dataset
train=Year<2005
# model fit and predict with knn method
knn.pred=knn(Xlag[train,],Xlag[!train,],Direction[train],k=1)
# confusion matrix
table(knn.pred,Direction[!train])
# mean
mean(knn.pred==Direction[!train])
