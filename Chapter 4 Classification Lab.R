#Stock Market Data Look At
library(ISLR)
names(Smarket)
dim(Smarket)
summary(Smarket)
pairs(Smarket)
cor(Smarket)
cor(Smarket[,-9])
attach(Smarket)
plot(Volume)

#Logistic Regression
glm_fits = glm(Direction ~ Lag1 + Lag2 + Lag3 + Lag4 + Lag5 + Volume, data = Smarket, family = binomial)
summary(glm_fits)
coef(glm_fits)
summary(glm_fits)$coef
summary(glm_fits)$coef[,4]
glm_probs = predict(glm_fits, type = "response")
glm_probs[1:10]
contrasts(Direction)

glm_pred = rep("Down", 1250)
glm_pred[glm_probs>.5] = "Up"
table(glm_pred, Direction)

train = (Year<2005)
Smarket_2005 = Smarket[!train,]
dim(Smarket_2005)
Direction_2005 = Direction[!train]

glm_fits_SS = glm(Direction ~ Lag1 + Lag2 + Lag3 + Lag4 + Lag5 + Volume, data = Smarket, family=binomial, subset = train)
glm_probs_test = predict(glm_fits_SS, Smarket_2005, type = "response")
glm_pred = rep("Down",252)
glm_pred[glm_probs_test > 0.5]="Up"
table(glm_pred, Direction_2005)
mean(glm_pred==Direction_2005)
mean(glm_pred!=Direction_2005)

glm_fits_L1L2 = glm(Direction ~ Lag1 + Lag2, data = Smarket, family = binomial, subset = train)
glm_probs = predict(glm_fits_L1L2, Smarket_2005, type = "response")
glm_pred = rep("Down", 252)
glm_pred[glm_probs > 0.5] = "Up"
table(glm_pred, Direction_2005)
mean(glm_pred==Direction_2005)
106/(106+76)

#Linear Discriminant Analysis
library(MASS)
lda_fit = lda(Direction ~ Lag1 + Lag2, data = Smarket, subset = train)
lda_fit

lda_pred = predict(lda_fit, Smarket_2005)
names(lda_pred)

lda_class = lda_pred$class
table(lda_class, Direction_2005)
mean(lda_class==Direction_2005)

sum(lda_pred$posterior[,1]>=.5)
sum(lda_pred$posterior[,1]<.5)

lda_pred$posterior[1:20,1]
lda_class[1:20]

sum(lda_pred$posterior[,1]>.9)

#Quadratic Discriminant Analysis

qda_fit = qda(Direction ~ Lag1 + Lag2, data = Smarket, subset = train)
qda_fit

qda_class = predict(qda_fit, Smarket_2005)$class
table(qda_class, Direction_2005)
mean(qda_class == Direction_2005)

#KNN

library(class)
train_X = cbind(Lag1, Lag2)[train,]
test_X = cbind(Lag1, Lag2)[!train,]
train_direction = Direction[train]

set.seed(1)
knn_predict = knn(train_X, test_X, train_direction, k = 1)
table(knn_predict, Direction_2005)

knn_predict = knn(train_X, test_X, train_direction, k =3)
table(knn_predict, Direction_2005)

#Application to Caravan Insurance Data
dim(Caravan)
attach(Caravan)
summary(Purchase)
348/5822

standardized_X = scale(Caravan[,-86])
var(Caravan[,1])
var(Caravan[,2])
var(standardized_X[,1])
var(standardized_X[,2])

test = 1:1000
train_X = standardized_X[-test,]
test_X = standardized_X[test,]
train_Y = Purchase[-test]
test_Y = Purchase[test]
set.seed(1)
knn_predict = knn(train_X, test_X, train_Y, k=1)
mean(test_Y != knn_predict)
mean(test_Y != "No")

table(knn_predict, test_Y)
9/(68+9)

knn_predict = knn(train_X, test_X, train_Y, k = 3)
table(knn_predict, test_Y)
5/26

knn_predict = knn(train_X, test_X, train_Y, k = 5)
table(knn_predict, test_Y)
4/15


















