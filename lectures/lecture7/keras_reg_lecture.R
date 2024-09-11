
# rensar den globala miljön:
rm(list=ls())

library(keras3)
library(ggplot2)
n<-2000
set.seed(243)
x<-sort(runif(n = n,min = -5,max = 5))
# vi skapar en y-variabel som är positiv och varierar icke-linjärt med x:
y<-log(1+ exp(sin(x*pi)/x-0.1+ rnorm(n = n,sd = 0.1))) 
plot(x,y,t="p")


x<-scale(x)[,]
plot(x,y,t="p")

y_train<-matrix(y)
x_train<-matrix(x)



#-------------------------------------------------------------------------------
# modell med två lager
nn_model <- keras_model_sequential()
nn_model %>%
  layer_dense(
    units = 20, 
    activation = "relu", 
    input_shape = c(ncol(x_train)),
    use_bias = TRUE
  ) %>% layer_dense(
    units = 20, 
    activation = "relu", 
    use_bias = TRUE
  ) %>%
  layer_dense(
    units = 1, 
    activation = "softplus"  # y kan inte vara negativ för denna data, så vi tvingar y_hat att vara positiv
  )
summary(nn_model)

nn_model %>% compile(
  optimizer = optimizer_sgd(learning_rate=0.1),
  loss = 'MSE',
  metrics = c('MSE')
)

set_random_seed(seed=232)
history <- nn_model %>% fit(
  x = x_train, y = y_train, 
  epochs = 50, 
  batch_size = 128, 
  validation_split = 0 
)

# träningshistorik
plot(history)

y_hat_train <- nn_model %>% predict(x_train)

plot(x_train[,1],y_train[,1],t="p")
points(x_train[,1],y_hat_train[,1],col="blue")


#-------------------------------------------------------------------------------
# ny modell
# fler lager
nn_model2 <- keras_model_sequential()
nn_model2 %>%
  layer_dense(
    units = 30, 
    activation = "relu", 
    input_shape = c(ncol(x_train)),
    use_bias = TRUE
  ) %>% layer_dense(
    units = 30, 
    activation = "relu", 
    use_bias = TRUE
  ) %>% layer_dense(
    units = 30, 
    activation = "relu", 
    use_bias = TRUE
  ) %>% layer_dense(
    units = 30, 
    activation = "relu", 
    use_bias = TRUE
  )%>%layer_dense(
    units = 1, 
    activation = "softplus"
  )
summary(nn_model2)

nn_model2 %>% compile(
  optimizer = optimizer_sgd(learning_rate=0.1),
  loss = 'MSE',
  metrics = c('MSE')
)

set_random_seed(seed=232)
history <- nn_model2 %>% fit(
  x = x_train, y = y_train, 
  epochs = 50, 
  batch_size = 128, 
  validation_split = 0 
)


y_hat_train2 <- nn_model2 %>% predict(x_train)

plot(x_train[,1],y_train[,1],t="p")
points(x_train[,1],y_hat_train2[,1],col="blue")



#-------------------------------------------------------------------------------
# ny modell
# nya förklarande variabler


# lägger till några transformationer där x är en funktion av sin()
plot(x_train[,1],sin(pi*x_train[,1]),t="l")
plot(x_train[,1],sin(2*pi*x_train[,1]),t="l")
plot(x_train[,1],sin(3*pi*x_train[,1]),t="l")
plot(x_train[,1],sin(4*pi*x_train[,1]),t="l")

x_train2<-matrix(cbind(x,sin(pi*x),sin(pi*2*x),sin(pi*3*x),sin(pi*4*x)),ncol = 5)
head(x_train2)

# kollar linjära beroenden mellan nya x och y:
plot(x_train2[,2],y_train)
plot(x_train2[,3],y_train)
plot(x_train2[,4],y_train)
plot(x_train2[,5],y_train)
# notera: ingen av dessa nya x har ett tydligt linjärt samband med y


nn_model3 <- keras_model_sequential()
nn_model3 %>%
  layer_dense(
    units = 5, 
    activation = "relu", 
    input_shape = c(ncol(x_train2)),
    use_bias = TRUE
  ) %>% layer_dense(
    units = 5, 
    activation = "relu", 
    use_bias = TRUE
  ) %>% layer_dense(
    units = 1, 
    activation = "softplus"
  )
summary(nn_model3)

nn_model3 %>% compile(
  optimizer = optimizer_sgd(learning_rate=0.1),
  loss = 'MSE',
  metrics = c('MSE')
)

set_random_seed(seed=232)
history <- nn_model3 %>% fit(
  x = x_train2, y = y_train, 
  epochs = 50, 
  batch_size = 128, 
  validation_split = 0 
)


y_hat_train3 <- nn_model3 %>% predict(x_train2)

plot(x_train[,1],y_train[,1],t="p")
points(x_train[,1],y_hat_train3[,1],col="blue")
# vi ser en mycket bättre anpassning jämfört med tidigare
# ibland så behöver vi hjälpa våra neurala nätverk med att skapa några 
# transformationer av x som är relevanta för problemet


# alla modellerna tillsammans

df<-data.frame(y=y_train[,1],x=x_train[,1],y_hat=c(y_hat_train[,1],y_hat_train2[,1],y_hat_train3[,1]),
              model=c(rep("1",nrow(y_train)),rep("2",nrow(y_train)),rep("3",nrow(y_train))))

ggplot(data = df,aes(x=x,y=y))+geom_point(alpha=0.3)+
  geom_line(aes(y = y_hat,color=model),linewidth=1)+theme_bw()




