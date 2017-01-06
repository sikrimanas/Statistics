# Non-Linear Models in R #
# Author: Manas Sikri
# Date: 24-Dec-2014

# We will explore some non linear function in R using some tools and techniques
# install the load the required packages
install.packages("ISLR")
library("ISLR")
attach(Wage) # attached the wage dataset

#View the wage dataset
View(Wage)

# Polynomials #
# -------------------------------------------------------

# we will fit a fouth degree polynomial
fit = lm(wage ~ poly(age,4), data = Wage)
summary(fit)

# The `poly()` function generates a basis of *orthogonal polynomials*.
# Lets make a plot of the fitted function, along with the standard errors of the fit.

agelims=range(age)
# creating a grid from start of range till end of range
age.grid=seq(from=agelims[1],to=agelims[2])
# predict with new data equal to age.grid
preds=predict(fit,newdata=list(age=age.grid),se=TRUE)

# standard error bands ( plus and minus 2)
se.bands=cbind(preds$fit + 2*preds$se, preds$fit - 2*preds$se)
# plot
plot(age,wage,col="darkgrey")
lines(age.grid,preds$fit,lwd=2,col="blue")
# line type 2 means broken line, for standard error on both sides
matlines(age.grid,se.bands,col="blue",lty=2)

# There are other more direct ways of doing this in R. For example
fita=lm(wage~age+I(age^2)+I(age^3)+I(age^4),data=Wage)
summary(fita)


# Here `I()` is a *wrapper* function; we need it because `age^2` means something 
# to the formula language, while `I(age^2)` is protected.
# The coefficients are different to those we got before! However, the fits are the same:

plot(fitted(fit),fitted(fita))

#In general we would use `anova()` as this next example demonstrates:
fita = lm(wage~education, data=Wage)
fitb = lm(wage~education+age, data=Wage)
fitc = lm(wage~education+poly(age,2), data=Wage)
fitd = lm(wage~education+poly(age,3), data=Wage)
# these all four are nested linear model in complexity

# anova function to sort ot which ones we need
anova(fita,fitb,fitc,fitd)


# Polynomial Logistic Regression
# --------------------------------------------------------------------

# Now we fit a logistic regression model to a binary response variable, 
# constructed from `wage`. 
# We code the big earners (`>250K`) as 1, else 0.

# fit the model with the help of I-function to make it binary
fit=glm(I(wage>250) ~ poly(age,3), data=Wage, family=binomial)
# summary
summary(fit)
# make a prediction
preds = predict(fit,list(age = age.grid),se = T)
# make the standard error bands
se.bands=preds$fit + cbind(fit=0,lower=-2*preds$se,upper=2*preds$se)
se.bands[1:5,]

# transform it into probability scale
prob.bands = exp(se.bands)/(1+exp(se.bands))
#plot
matplot(age.grid
        ,prob.bands
        ,col="blue"         # color
        ,lwd=c(2,1,1)       # line type
        ,lty=c(1,2,2)
        ,type="l"           # type
        ,ylim=c(0,.1))
# adding points with the help of jitter
points(jitter(age)
       ,I(wage>250)/10
       ,pch="|"
       ,cex=.5)

# Splines
# ------------------------------------------------------------

# Splines are more flexible than polynomials, but the idea is rather similar.
# Here we will explore cubic splines.
require(splines)

# fit a cublic spline with knots at 25,40 and 60
fit=lm(wage ~ bs(age,knots=c(25,40,60)), data = Wage)
# plot
plot(age,wage,col="darkgrey")
# add the line
lines(age.grid,predict(fit,list(age=age.grid)),col="darkgreen",lwd=2)
# add knots
abline(v=c(25,40,60),lty=2,col="darkgreen")

# Smoothing Splines
# -------------------------------------------------------------

# The smoothing splines does not require knot selection, but it does have a smoothing parameter,
# which can conveniently be specified via the effective degrees of freedom or `df`.

fit = smooth.spline(age, wage, df=16)
lines(fit, col="red", lwd=2)

# using LOO cross validation
fit=smooth.spline(age,wage,cv=TRUE)
lines(fit,col="purple",lwd=2)

fit

# Generalized Additive Models
# --------------------------------------------------------------

# The `gam` package makes it easier to work with multiple nonlinear terms. In addition 
# it knows how to plot these functions and their standard errors.

install.packages("gam")
require(gam)
gam1 = gam(wage ~ s(age,df=4) + s(year,df=4) + education
           , data = Wage)
# divide the display frame into parts
par(mfrow=c(1,3))
# plot
plot(gam1,se=T)
# binary
gam2 = gam(I(wage>250) ~ s(age,df=4) + s(year,df=4) + education
           ,data=Wage
           ,family=binomial)
# plot
plot(gam2)

gam2a=gam(I(wage>250)~s(age,df=4)+year+education,data=Wage,family=binomial)
anova(gam2a,gam2,test="Chisq")

# One nice feature of the `gam` package is that it knows how to 
# plot the functions nicely,
# even for models fit by `lm` and `glm`.

par(mfrow=c(1,3))
# linear model
lm1 = lm(wage ~ ns(age,df=4) + ns(year,df=4) + education
         ,data=Wage)
# plot using gam
plot.gam(lm1,se=T)