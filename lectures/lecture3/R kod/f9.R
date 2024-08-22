library(stats)
library(tidyverse)
library(caret)
n <- 500
x <- runif(n , min = 0, max = 5  )
y <- 0.5 + 3*x - 2*x^2 + .2*x^3 + log(2*x+1) + 3*sin(pi * x) + rnorm(n = n, sd = 1)
df <- data.frame(x = x, y = y)

plot(x, y)

modelLin <- lm(y ~ x)

plot(x, y)
abline(lm(y ~ x))

modelPoly <- lm(y ~ poly(x, 8, raw=T))
summary(modelPoly)

plot(x, y)
lines(sort(x), fitted(modelPoly)[order(x)])


modelStep <- lm(y ~ cut(x,10))
plot(x, y)
lines(sort(x), fitted(modelStep)[order(x)])

library(keras)
modelNN <- keras_model_sequential()
modelNN %>% layer_dense(
  units = 64,
  activation = "tanh",
  input_shape = c(1),
) %>% layer_dense(
  units = 32,
  activation = "tanh",
)%>% layer_dense(
  units = 16,
  activation = "tanh",
)%>%
layer_dense(
  units = 1
)
modelNN %>% compile(
  loss = 'mean_squared_error',
  optimizer = optimizer_sgd()
)
modelNN %>% fit(
  x, y,
  epochs = 30,
  batch_size = 32
)
plot(x, y)
y_pred <- modelNN %>% predict(sort(x))
lines(sort(x), y_pred, col = "red")

library(splines)

spl <- lm(y ~ bs(x, knots = c(1,2,4), degree = 2))
y_pred <- predict(spl)
plot(x,y)
lines(sort(x), fitted(spl)[order(x)], col = "red")

data <- data.frame(x,y)
spl1 <- lm(y ~ poly(x,2), data = data[data$x<1,])
spl2 <- lm(y ~ poly(x,2), data = data[data$x>=1 & data$x < 2.5,])
spl3 <- lm(y ~ poly(x,2), data = data[data$x>=2.5 & data$x < 4,])
spl4 <- lm(y ~ poly(x,2), data = data[data$x>=4,])

piece.quad <- ggplot(data, aes(x=x,y=y)) +
  geom_point(alpha = 0.55, color = "black") +
  theme_minimal()
 piece.quad

piece.quad +
  geom_line(data = data[data$x<1,],
  aes(y= predict(spl1), x = x ), color = "red") +
  geom_line(data = data[data$x>=1 & data$x < 2.5,],
  aes(y= predict(spl2), x = x ), color = "red") +
  geom_line(data = data[data$x>=2.5 & data$x < 4,],
  aes(y= predict(spl3), x = x ), color = "red") +
  geom_line(data = data[data$x>=4,],
  aes(y= predict(spl4), x = x ), color = "red")

splCont <- lm( y ~ x + I(x^2) +
I((x - 1)*(x >= 1)) + I((x - 1)^2*(x >= 1)) +
I((x - 2.5)*(x >= 2.5)) + I((x - 2.5)^2*(x >= 2.5)) +
I((x - 4)*(x >= 4)) + I((x - 4)^2*(x >= 4)), data = data  )

piece.quad.cont <- ggplot(data, aes(x=x,y=y)) +
  geom_point(alpha = 0.55, color = "black") +
  theme_minimal()
piece.quad.cont +
  geom_line(data = data,
    aes(y = predict(splCont), x=x), color = "red")
piece.quad.cont

splCont2 <- lm( y ~ x + I(x^2)
I((x - 1)^2*(x >= 1)) +
I((x - 2)^2*(x >= 2)) +
I((x - 3)^2*(x >= 3)) +
I((x - 4)^2*(x >= 4)), data = data  )

piece.quad.cont2 <- ggplot(data, aes(x=x,y=y)) +
  geom_point(alpha = 0.55, color = "black") +
  theme_minimal()
piece.quad.cont2 +
  geom_line(data = data,
    aes(y = predict(splCont2), x=x), color = "red")
piece.quad.cont2

cubic.bs <- lm(y ~ bs(x, knots = c(1,2,3,4), degree = 3), data = data)
cubic.ns <- lm(y ~ ns(x, knots = c(1,2,3,4)), data = data)
cubic.ns2 <- lm(y ~ ns(x, knots = c(0.5,1,2,3,4,4.5)), data = data)
plot.cubic <- ggplot(data, aes(x=x,y=y)) +
  geom_point(alpha = 0.55, color = "black") +
  theme_minimal()
plot.cubic +
  geom_line(data = data,
  aes(y = predict(cubic.bs), x = x), color = "blue")  +
  geom_line(data = data,
  aes(y = predict(cubic.ns), x = x), color = "red") +
  geom_line(data = data,
  aes(y = predict(cubic.ns2), x = x), color = "green")
plot.cubic

df <- c(4:15)
mse.bs <- c()
mse.ns <- c()
indx <- createDataPartition(x, p=0.8, list = FALSE)
x_train <- x[indx]
y_train <- y[indx]
x_test <- x[-indx]
y_test <- y[-indx]
for (d in df){
  mod.bs <- lm(y ~ bs(x, df = d, intercept = TRUE), data = data[indx,])
  mseVal.bs <- sum( (y_test - predict(mod.bs, newdata= list(x = x_test)))^2   )/length(y_test)
  mse.bs <- c(mse.bs, mseVal.bs)
  mod.ns <- lm(y ~ ns(x, df = d, intercept = TRUE), data = data[indx,])
  mseVal.ns <- sum( (y_test - predict(mod.ns, newdata= list(x = x_test)))^2   )/length(y_test)
  mse.ns <- c(mse.ns, mseVal.ns)
}

plot.mse <- ggplot(data.frame(mse.bs,mse.ns,df)) +
  geom_point(aes(x = df, y = mse.bs), col = "blue") +
  geom_point(aes(x = df, y = mse.ns), col = "red") +
  theme_minimal() +
  ylab("MSE") +
  xlab("Frihetsgrader")
plot.mse

d.bs = 9
d.ns = 8

mod.bs <- lm(y ~ bs(x, df = d.bs), data = data[indx,])
mod.ns <- lm(y ~ ns(x, df = d.ns), data = data[indx,])

plot.cubic <- ggplot(data, aes(x=x,y=y)) +
  geom_point(alpha = 0.55, color = "black") +
  theme_minimal()
plot.cubic +
  geom_line(data = data,
  aes(y = predict(mod.bs, list(x=x)), x = x), color = "blue")  +
  geom_line(data = data,
  aes(y = predict(mod.ns, list(x=x)), x = x), color = "red")
plot.cubic


lm.1 <- lm(y ~ x, data = data[indx,])
lm.2 <- lm(y ~ poly(x, 2), data = data[indx,])
lm.3 <- lm(y ~ poly(x, 3), data = data[indx,])
lm.4 <- lm(y ~ poly(x, 4), data = data[indx,])
lm.5 <- lm(y ~ poly(x, 5), data = data[indx,])
lm.8 <- lm(y ~ poly(x, 8), data = data[indx,])
lm.12 <- lm(y ~ poly(x, 12), data = data[indx,])
lm.15 <- lm(y ~ poly(x, 15), data = data[indx,])

plot.poly <- ggplot(data, aes(x=x,y=y)) +
  geom_point(alpha = 0.55, color = "black") + theme_minimal()
plot.poly
plot.poly +
  geom_line(data = data,
  aes(y = predict(lm.1, list(x=x)), x = x), color = "lightblue")+
  geom_line(data = data,
  aes(y = predict(lm.2, list(x=x)), x = x), color = "blue")+
  geom_line(data = data,
  aes(y = predict(lm.3, list(x=x)), x = x), color = "darkblue")+
  geom_line(data = data,
  aes(y = predict(lm.4, list(x=x)), x = x), color = "red")+
  geom_line(data = data,
  aes(y = predict(lm.5, list(x=x)), x = x), color = "pink")+
  geom_line(data = data,
  aes(y = predict(lm.8, list(x=x)), x = x), color = "purple")+
  geom_line(data = data,
  aes(y = predict(lm.12, list(x=x)), x = x), color = "green")+
  geom_line(data = data,
  aes(y = predict(lm.15, list(x=x)), x = x), color = "brown")

logit <- function(x){
  log(x) - log(1-x)
}
invlogit <- function(x){
  1/(1+exp(x))
}
y_bin <- invlogit(y);
y_bin <- as.integer(y_bin > 0.5)


fit1 <- smooth.spline(x,y,df = 9)
fit2 <- smooth.spline(x,y,cv = TRUE)
fit2$df
fit1$df
fit1.pred <- predict(fit1)
fit2.pred <- predict(fit2)
plot.smooth.cv <- ggplot(data, aes(x=x,y=y)) +
  geom_point(alpha = 0.55, color = "black") + theme_minimal()
plot.smooth.cv
plot.smooth.cv +
  geom_line(
  aes(y = fit1.pred$y, x = fit1.pred$x), color = "red") +
  geom_line(
  aes(y = fit2.pred$y, x = fit2.pred$x), color = "blue")

library(gam)
class.smooth <- gam(I(y > 0) ~ s(x, df = 10), family = binomial, data = data)
plot.bin.smooth <- ggplot(data.bin, aes(x=x,y=y_bin)) +
  geom_point(alpha = 0.55, color = "black") + theme_minimal()
plot.bin.smooth
plot.bin.smooth + 
  geom_line(
    aes(y = predict(class.smooth), x = x)
  )
