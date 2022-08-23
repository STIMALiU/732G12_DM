
# Lab: Linear Models and Regularization Methods


## Subset Selection Methods


### Best Subset Selection

###
library(ISLR2)
View(Hitters)
names(Hitters)
dim(Hitters)
sum(is.na(Hitters$Salary))
###
Hitters <- na.omit(Hitters)
dim(Hitters)
sum(is.na(Hitters))
###
library(leaps)
regfit.full <- regsubsets(Salary ~ ., Hitters)
summary(regfit.full)
###
regfit.full <- regsubsets(Salary ~ ., data = Hitters,
                          nvmax = 19)
reg.summary <- summary(regfit.full)
###
names(reg.summary)
###
reg.summary$rsq
###
par(mfrow = c(2, 2))
plot(reg.summary$rss, xlab = "Number of Variables",
     ylab = "RSS", type = "l")
plot(reg.summary$adjr2, xlab = "Number of Variables",
     ylab = "Adjusted RSq", type = "l")
###
which.max(reg.summary$adjr2)
points(11, reg.summary$adjr2[11], col = "red", cex = 2, 
       pch = 20)
###
plot(reg.summary$cp, xlab = "Number of Variables",
     ylab = "Cp", type = "l")
which.min(reg.summary$cp)
points(10, reg.summary$cp[10], col = "red", cex = 2,
       pch = 20)
which.min(reg.summary$bic)
plot(reg.summary$bic, xlab = "Number of Variables",
     ylab = "BIC", type = "l")
points(6, reg.summary$bic[6], col = "red", cex = 2,
       pch = 20)
###
plot(regfit.full, scale = "r2")
plot(regfit.full, scale = "adjr2")
plot(regfit.full, scale = "Cp")
plot(regfit.full, scale = "bic")
###
coef(regfit.full, 6)

### Forward and Backward Stepwise Selection

###
regfit.fwd <- regsubsets(Salary ~ ., data = Hitters,
                         nvmax = 19, method = "forward")
summary(regfit.fwd)
regfit.bwd <- regsubsets(Salary ~ ., data = Hitters,
                         nvmax = 19, method = "backward")
summary(regfit.bwd)
###
coef(regfit.full, 7)
coef(regfit.fwd, 7)
coef(regfit.bwd, 7)

### Choosing Among Models Using the Validation-Set Approach and Cross-Validation

###
set.seed(1)
train <- sample(c(TRUE, FALSE), nrow(Hitters),
                replace = TRUE)
test <- (!train)
###
regfit.best <- regsubsets(Salary ~ .,
                          data = Hitters[train, ], nvmax = 19)
###
test.mat <- model.matrix(Salary ~ ., data = Hitters[test, ])
###
val.errors <- rep(NA, 19)
for (i in 1:19) {
  coefi <- coef(regfit.best, id = i)
  pred <- test.mat[, names(coefi)] %*% coefi
  val.errors[i] <- mean((Hitters$Salary[test] - pred)^2)
}
###
val.errors
which.min(val.errors)
coef(regfit.best, 7)
###
predict.regsubsets <- function(object, newdata, id, ...) {
  form <- as.formula(object$call[[2]])
  mat <- model.matrix(form, newdata)
  coefi <- coef(object, id = id)
  xvars <- names(coefi)
  mat[, xvars] %*% coefi
}
###
regfit.best <- regsubsets(Salary ~ ., data = Hitters,
                          nvmax = 19)
coef(regfit.best, 7)
###
k <- 10
n <- nrow(Hitters)
set.seed(1)
folds <- sample(rep(1:k, length = n))
cv.errors <- matrix(NA, k, 19,
                    dimnames = list(NULL, paste(1:19)))
###
for (j in 1:k) {
  best.fit <- regsubsets(Salary ~ .,
                         data = Hitters[folds != j, ],
                         nvmax = 19)
  for (i in 1:19) {
    pred <- predict(best.fit, Hitters[folds == j, ], id = i)
    cv.errors[j, i] <-
      mean((Hitters$Salary[folds == j] - pred)^2)
  }
}
###
mean.cv.errors <- apply(cv.errors, 2, mean)
mean.cv.errors
par(mfrow = c(1, 1))
plot(mean.cv.errors, type = "b")
###
reg.best <- regsubsets(Salary ~ ., data = Hitters,
                       nvmax = 19)
coef(reg.best, 10)

## Ridge Regression and the Lasso

###

Hitters <- na.omit(Hitters)
dim(Hitters)
sum(is.na(Hitters))
x <- model.matrix(Salary ~ ., Hitters)[, -1]
y <- Hitters$Salary


### Ridge Regression

###
library(glmnet)

###
set.seed(1354)
train <- sample(1:nrow(x), nrow(x) / 2)
test <- (-train)

y_train<-y[train]
y_test <- y[test]

x_train<-x[train,]
x_test<-x[test,]

# estimate beta_start
set.seed(53)
cv.out <- cv.glmnet(x_train, y_train, alpha = 0,nfolds = 20)
plot(cv.out)
bestlam <- cv.out$lambda.min
bestlam

beta_start<-predict(cv.out, type = "coefficients",s = bestlam)[, ]

plot(beta_start) # med intercept
plot(beta_start[-1]) # utan


# skapa vikter
w<-1/abs(beta_start[-1])  # utan intercept
# testa även att använda beta från OLS som vikter 

# vi bestämmer vikter med argumentet penalty.factor
set.seed(353)
cv.out2 <- cv.glmnet(x_train, y_train, alpha = 1,nfolds = 20, penalty.factor=w)
plot(cv.out2)
bestlam2 <- cv.out2$lambda.min
beta_adaptive_lasso<-predict(cv.out2, type = "coefficients",s = bestlam2)[, ]

# hur många blev noll?
sum(beta_adaptive_lasso[-1]==0)

# jämför med ridge 
par(mfrow=c(3,1))
plot(beta_start[-1])
plot(beta_adaptive_lasso[-1])
plot(beta_adaptive_lasso[-1]-beta_start[-1])
par(mfrow=c(1,1))

library(ggplot2)
qplot(x=1:19,y = sort(beta_adaptive_lasso[-1]-beta_start[-1]),geom="point")
qplot(x=1:19,y = beta_adaptive_lasso[-1]-beta_start[-1],geom="point")


# jämför beta-skattningar mellan
# ridga, lasso och adaptive lasso
# beräkna tränings och test MSE är dessa modeller och jämför 





