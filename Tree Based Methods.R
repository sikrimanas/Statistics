# Tree Methods in R
# Author: Manas Sikri
# Date: 26-Dec-2016

# Decision Trees 
# ------------------------------------------------------------------

# install the required packages
require(ISLR)
install.packages("tree")
library("tree")

# attach the required dataset
attach(Carseats)
# View the carseats dataset
View(Carseats)
?Carseats

# histogram of sales
hist(Sales)
High = ifelse(Sales <= 8, "No", "Yes")
# create a new dataframe including this high column
Carseats = data.frame(Carseats, High)

# now we will try to fit a tree model and exlude Sales from it
tree.carseats = tree(High~.-Sales, data = Carseats)
summary(tree.carseats)                     # summary of the tree model
plot(tree.carseats)                        # plot the tree model
text(tree.carseats, pretty = 0)            # add text to the plot of tree model

# for detailed summary of the tree, print it
tree.carseats

# Lets create a training and test (250,150) split of the 400 observations,
# grow the tree in the training data
# and evaluate its performanc on test data
set.seed(1011)
# train index
train = sample(1:nrow(Carseats), 250)
tree.model = tree(High~.-Sales, Carseats, subset = train)
# plot and add text
plot(tree.model);text(tree.model,pretty=0)
# predict
tree.pred=predict(tree.model, Carseats[-train,], type="class")
with(Carseats[-train,], table(tree.pred,High)) # to create classification matrix
(72+33)/150


# This tree was grown to full depth, and might be too variable. 
# We now use CV to prune it.

# cross validation tree model
cv.carseats=cv.tree(tree.carseats,FUN=prune.misclass)
cv.carseats
# plot
plot(cv.carseats)
# prune
prune.carseats=prune.misclass(tree.carseats,best=13)
# plot and add text
plot(prune.carseats);text(prune.carseats,pretty=0)


# Random Forest and Boosting
# ------------------------------------------------------------------

# we will use boston housing data  to explore random forests and boosting
# the data are in the 'MASS' package

install.packages("MASS")
library(MASS)
install.packages("randomForest")
library(randomForest)

set.seed(101)
# dimensions of boston data set
dim(Boston)
?Boston

train = sample(1:nrow(Boston), 300)

# lets fit a random forest and check how it performs
# we want the response medv - median value of owner occupied homes in \$1000s
rf.Boston = randomForest(medv~., data = Boston, subset = train)
rf.Boston


# setup two variables to record the errors
oob.err = double(13)
test.err = double(13)

for(mtry in 1:13){
  # fit the model with corresponding mtry value
  fit=randomForest(medv~.,data=Boston,subset=train,mtry=mtry,ntree=400)
  # record out of bag error
  oob.err[mtry]=fit$mse[400]
  # predct
  pred=predict(fit,Boston[-train,])
  # record the test error
  test.err[mtry]=with(Boston[-train,],mean((medv-pred)^2))
  # print the value of mtry
  cat(mtry," ")
}

# plot
matplot(1:mtry
        ,cbind(test.err,oob.err)
        ,pch=19
        ,col=c("red","blue")
        ,type="b"
        ,ylab="Mean Squared Error")

# add legend
legend("topright",legend=c("OOB","Test"),pch=19,col=c("red","blue"))

# in the above case, mtry=13 is actually a case of Bagging

# Boosting
# ------------------------------------------------------------------

# Boosting builds lots of smaller trees. Unlike random forests, each new tree 
# in boosting tries to patch up the deficiencies of the current ensemble.

# install the required package
# gbm - Gradient Boosting Machine
install.packages("gbm")
library(gbm)
# fit the model
boost.boston=gbm(medv~.
                 ,data=Boston[train,]
                 ,distribution="gaussian" # type of distribution
                 ,n.trees=10000           # number of trees
                 ,shrinkage=0.01
                 ,interaction.depth=4)    # number of splits

# summary
summary(boost.boston) # it gives variable importance plot
# plot the variable lstat
plot(boost.boston, i = "lstat")
#plot the rm
plot(boost.boston, i = "rm")


# lets predict the boost model on the test set
n.trees=seq(from=100,to=10000,by=100)
predmat = predict(boost.boston
                  ,newdata=Boston[-train,]
                  ,n.trees=n.trees)
dim(predmat)
berr = with(Boston[-train,]
            ,apply( (predmat-medv)^2,2,mean))
# plot
plot(n.trees,berr
     ,pch=19
     ,ylab="Mean Squared Error"
     ,xlab="# Trees"
     ,main="Boosting Test Error")
abline(h=min(test.err),col="red")
