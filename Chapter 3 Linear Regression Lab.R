#Load MASS and ISLR packages
library(MASS)
library(ISLR)

fix(Boston)
names(Boston)
?Boston
attach(Boston)

lm_fit = lm(medv~lstat)
lm_fit

summary(lm_fit)
names(lm_fit)
coef(lm_fit)
confint(lm_fit)

plot(lstat,medv)
abline(lm_fit)

abline(lm_fit, lwd=3)
abline(lm_fit, lwd=3, col="red")
plot(lstat,medv,col="red")
plot(lstat,medv,pch=20)
plot(lstat,medv,pch="+")
plot(1:20,1:20, pch=1:20)

par(mfrow=c(2,2))
plot(lm_fit)

plot(predict(lm_fit), residuals(lm_fit))
plot(predict(lm_fit), rstudent(lm_fit))

plot(hatvalues(lm_fit))
which.max(hatvalues(lm_fit))

#Multiple Linear Regression
lm_fit = lm(medv ~ lstat + age, data = Boston)
summary(lm_fit)

lm_fit = lm(medv ~ ., data = Boston)
summary(lm_fit)

library(car)
vif(lm_fit)

lm_fit_1 = lm(medv ~ .-age, data = Boston)
summary(lm_fit_1)

#Interaction Terms
summary(lm(medv ~ lstat*age, data = Boston))

#non-linear Transformation of Predictors
lm_fit_nonlin = lm(medv ~ lstat + I(lstat^2))
summary(lm_fit_nonlin)

lm_fit = lm(medv ~ lstat)
anova(lm_fit, lm_fit_nonlin)

par(mfrow=c(2,2))
plot(lm_fit_nonlin)

lm_fit_5th_ord = lm(medv ~ poly(lstat,5))
summary(lm_fit_5th_ord)

summary(lm(medv ~ log(rm), data = Boston))

#Qualitative Predictors
fix(Carseats)
names(Carseats)

lm_fit_qual = lm(Sales ~ .+Income:Advertising + Price:Age, data = Carseats)
summary(lm_fit_qual)

#Writing Functions
LoadLibraries
LoadLibraries()

LoadLibraries=function(){
  library(ISLR)
  library(MASS)
  print("The libraries have been loaded, bitch!")
}

LoadLibraries()
LoadLibraries

