#Validation Set Approach

library(ISLR)
set.seed(1)
train = sample(392,196)
?sample

lm_fit = lm(mpg ~ horsepower, data = Auto, subset = train)
attach(Auto)
mean((mpg-predict(lm_fit, Auto))[-train]^2)

lm_fit2 = lm(mpg ~ poly(horsepower, 2), data = Auto, subset = train)
mean((mpg-predict(lm_fit2, Auto))[-train]^2)

lm_fit3 = lm(mpg ~ poly(horsepower,3), data = Auto, subset = train)
mean((mpg-predict(lm_fit3, Auto))[-train]^2)

set.seed(2)
train = sample(392,196)
lm_fit = lm(mpg ~ horsepower, subset = train)

lm_fit = lm(mpg ~ horsepower, data = Auto, subset = train)
mean((mpg-predict(lm_fit, Auto))[-train]^2)

lm_fit2 = lm(mpg ~ poly(horsepower, 2), data = Auto, subset = train)
mean((mpg-predict(lm_fit2, Auto))[-train]^2)

lm_fit3 = lm(mpg ~ poly(horsepower,3), data = Auto, subset = train)
mean((mpg-predict(lm_fit3, Auto))[-train]^2)

#Leave-One_Out Cross-Validation














