library(stats)
library(FNN)
library(tidyverse)
library(gplm)

x_train <- runif(100)
y_train <- sin(4*x_train) + rnorm(n = 100, sd = 1/3)



x_test <- seq(0,1, length.out = 1000)

mod.knn.reg.10 <- knn.reg(train = data.frame(x_train), test = data.frame(x_test), y = data.frame(y_train), k = 10, algorithm = "brute")
mod.knn.reg.30 <- knn.reg(train = data.frame(x_train), test = data.frame(x_test), y = data.frame(y_train), k = 30, algorithm = "brute")
mod.knn.reg.10

knn.reg.plot <- ggplot(data = data.frame(x_train, y_train), mapping = aes(x = x_train, y = y_train)) +
  geom_point(alpha = 0.55, color = "black") + theme_minimal() + ylab("Y") + xlab("X")
knn.reg.plot
knn.reg.plot +
  geom_line(aes(y = sin(4*x_train), x = x_train), color = "red") +
  geom_line(aes(y = mod.knn.reg.10$pred, x = x_test), color = "blue", data = data.frame(mod.knn.reg.10$pred, x_test)) +
  geom_line(aes(y = mod.knn.reg.30$pred, x = x_test), color = "green", data = data.frame(mod.knn.reg.30$pred, x_test))


mod.kreg.triangle <- kreg(x = as.matrix(x_train), y = as.matrix((y_train)), kernel = "triangle" )
mod.kreg.epan <- kreg(x = as.matrix(x_train), y = as.matrix((y_train)), kernel = "epanechnikov" )
mod.kreg.quartic <- kreg(x = as.matrix(x_train), y = as.matrix((y_train)), kernel = "quartic" )

dftri = data.frame(x = mod.kreg.triangle$x, y = mod.kreg.triangle$y)
dfepa = data.frame(x = mod.kreg.epan$x, y = mod.kreg.epan$y)
dfqua = data.frame(x = mod.kreg.quartic$x, y = mod.kreg.quartic$y)

kreg.plot <- ggplot(data = data.frame(x_train, y_train), mapping = aes(x = x_train, y = y_train)) +
  geom_point(alpha = 0.55, color = "black") + theme_minimal() + ylab("Y") + xlab("X")
kreg.plot
kreg.plot +
  geom_line(aes(y = sin(4*x_train), x = x_train), color = "red") +
  geom_line(aes(x = Var1, y = y), data = dftri, color = "green") +
  geom_line(aes(x = Var1, y = y), data = dfepa, color = "brown") +
  geom_line(aes(x = Var1, y = y), data = dfqua, color = "blue")
dftri$x

mod.loc.reg1 <- loess(y_train ~ x_train, span = .1)
mod.loc.reg2 <- loess(y_train ~ x_train, span = .2)
mod.loc.reg3 <- loess(y_train ~ x_train, span = .5)

loc.reg.plot <- ggplot(data = data.frame(x_train, y_train), mapping = aes(x = x_train, y = y_train)) +
  geom_point(alpha = 0.55, color = "black") + theme_minimal() + ylab("Y") + xlab("X")
loc.reg.plot
loc.reg.plot +
  geom_line(aes(y = sin(4*x_train), x = x_train), color = "red") +
  geom_line(aes(x = x_test, y = predict(mod.loc.reg1, data.frame(x_train = x_test))), data = data.frame(x_test, x_train), color = "blue") +
  geom_line(aes(x = x_test, y = predict(mod.loc.reg2, data.frame(x_train = x_test))), data = data.frame(x_test, x_train), color = "green") +
  geom_line(aes(x = x_test, y = predict(mod.loc.reg3, data.frame(x_train = x_test))), data = data.frame(x_test, x_train), color = "purple")
