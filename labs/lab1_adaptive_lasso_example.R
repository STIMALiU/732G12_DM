
#-------------------------------------------------------------------------------
# Adaptive lasso
#-------------------------------------------------------------------------------
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





