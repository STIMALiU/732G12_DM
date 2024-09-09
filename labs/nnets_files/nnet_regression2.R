#-------------------------------------------------------------------------------
library(ggplot2)
library(keras3)
#-------------------------------------------------------------------------------
data("diamonds")
?diamonds
dim(diamonds)
summary(diamonds)

# price är rätt skev, så vi analyserar log(price) som respons
hist(diamonds$price)
hist(log(diamonds$price))
#-------------------------------------------------------------------------------


# det är många observationer, så vi tar ett urval för att göra skattningarna 
# lite snabbare (detta är främst för att göra skattningarna av de olika 
# trädmodellerna snabbare)
set.seed(352)
index<-sample(x = nrow(diamonds),size = nrow(diamonds)*0.5,replace = FALSE)

diamonds2<-diamonds[index,]
diamonds2<-diamonds[-index,]
str(diamonds2)

# standardisera numeriska x:
diamonds2[,c(1,5,6,8,9,10)]<-scale(diamonds2[,c(1,5,6,8,9,10)])[]
# skapar log av price:
diamonds2$price<-log(diamonds2$price)
colnames(diamonds2)[7]<-"log_price"

#-------------------------------------------------------------------------------
# skapar träningsdata och valideringsdata
# 70 % av data för träning
set.seed(98)
index_train<-sample(x = nrow(diamonds2),size = nrow(diamonds2)*0.7,replace = FALSE)
train_data_df<-diamonds2[index_train,]
val_data_df<-diamonds2[-index_train,]

# skapar lämpliga matriser:
y_train<-matrix(train_data_df$log_price)
y_val<-matrix(val_data_df$log_price)
x_train<-as.matrix(model.matrix(log_price ~.-1,data=train_data_df))
dim(x_train)
x_val<-as.matrix(model.matrix(log_price ~.-1,data=val_data_df))
dim(x_val)
#-------------------------------------------------------------------------------



#-------------------------------------------------------------------------------
# modell 2:
nn_model2 <- keras_model_sequential()
nn_model2 %>%
  layer_dense(
    units = 50, 
    activation = "relu", 
    input_shape = c(ncol(x_train)),
    use_bias = TRUE
  ) %>% layer_dense(
    units = 50, 
    activation = "relu", 
    use_bias = TRUE
  ) %>%
  layer_dense(
    units = 1, 
    activation = "linear"
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

# träningshistorik
plot(history)

score_train <- nn_model2 %>% evaluate(x_train, y_train, verbose = 0)
cat('Train loss:', score_train[["loss"]], "\n")
score_train <- nn_model2 %>% evaluate(x_val, y_val, verbose = 0)
cat('Train loss:', score_train[["loss"]], "\n")

source("https://raw.githubusercontent.com/STIMALiU/732G12_DM/master/labs/lm_diagnostics.R")

nn_model2_fit<-nn_model2 %>% predict(x_train)


nn_model2_res<-y_train[,1]-nn_model2_fit[,1]


model_diagnostics(res_vect = nn_model2_res,fit_vect =nn_model2_fit[,1])

#-------------------------------------------------------------------------------



#-------------------------------------------------------------------------------
# modell 5:

library(randomForest)
set.seed(67)
tree2<-randomForest(log_price~.,data=train_data_df,ntree=100,mtry=3)
y_hat_tree2<-predict(tree2)
y_hat_tree2_val<-predict(tree2,newdata = val_data_df[,-7])

mse_forest_train<-mean((train_data_df$log_price-y_hat_tree2)^2)
mse_forest_val<-mean((val_data_df$log_price-y_hat_tree2_val)^2)

mse_forest_train
mse_forest_val

tree2_res<-train_data_df$log_price-y_hat_tree2
model_diagnostics(res_vect = tree2_res,fit_vect =y_hat_tree2)


#-------------------------------------------------------------------------------

