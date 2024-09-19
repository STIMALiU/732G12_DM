#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
# läs in paket
library(keras3)
library(GGally)
library(Matrix)
library(caret)
library(randomForest)

source("https://raw.githubusercontent.com/STIMALiU/732G12_DM/master/labs/class_evaluation.R")
source("https://raw.githubusercontent.com/STIMALiU/732G12_DM/master/labs/class_evaluation_keras.R")


#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------


#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
# Läsa in och processa data
#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
# info om data: https://raw.githubusercontent.com/STIMALiU/732G12_DM/master/labs/Email_Spam_dataset_info.pdf

D<-read.csv2(file = "https://raw.githubusercontent.com/STIMALiU/732G12_DM/master/data/spambase.csv")
colnames(D)
D2<-D


# Då våra förklarande variabler är tungsvansade kan en log-transformation fungera bra:
# (log(x + 0.1)), där vi lägger på 0.1 för att undvika log(0) problem.
for(i in 1:57){
  D2[,i]<-log(D[,i]+0.1)
}

cor_mat<-Matrix(round(cor(D2[,-58]),2))
image(cor_mat)

D3<-D2
D3[,-58]<-scale(D2[,-58])
D3$Spam<-as.factor(D3$Spam)

train_data<-as.matrix(D3[,-58])
summary(train_data)


#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
# Jämförande modeller på orginaldata
#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------


#-------------------------------------------------------------------------------
# (i) Linjär logistik regression
#-------------------------------------------------------------------------------
log_reg_obj1<-glm(formula = Spam~.,data = D3,family=binomial(link='logit'))
summary(log_reg_obj1)

log_reg_obj1_fit<-ifelse(predict(log_reg_obj1,type = "response")>=0.5,1,0)

# träffsäkerhet:
mean(log_reg_obj1_fit==D3$Spam)
#-------------------------------------------------------------------------------



#-------------------------------------------------------------------------------
# (ii) Random forest
#-------------------------------------------------------------------------------
set.seed(4654)
tree_model<-randomForest(formula= Spam~.,data=D3,ntree=100,mtry=57/3)
# träffsäkerhet:
mean(predict(tree_model)==D3$Spam)

#-------------------------------------------------------------------------------



#-------------------------------------------------------------------------------
# (iii) MLP
#-------------------------------------------------------------------------------

y_train<-as.matrix(as.numeric(D3$Spam)-1)
nn_model1 <- keras_model_sequential()
nn_model1 %>%
  ## Definierar första lagret, input_shape ska anges för att definiera hur många förklarande variabler som finns i data
  layer_dense(
    # Anger antal gömda noder i det GÖMDA lagret
    units = 100, 
    # Anger aktiveringsfunktionen i lagret
    activation = "relu", 
    # Det första gömda lagret anger hur många förklarande variabler som ska kopplas till lagret
    # Notera att input_shape styr input-lagret, units styr det gömda lagret
    input_shape = c(ncol(train_data)),
    # Anger att vi vill ha med en bias-term i lagret
    use_bias = TRUE
  ) %>% layer_dense(
    # Anger antal gömda noder i det GÖMDA lagret
    units = 100, 
    # Anger aktiveringsfunktionen i lagret
    activation = "relu", 
    # Det första gömda lagret anger hur många förklarande variabler som ska kopplas till lagret
    # Notera att input_shape styr input-lagret, units styr det gömda lagret
    # Anger att vi vill ha med en bias-term i lagret
    use_bias = TRUE
  ) %>%
  ## Anger det sista lagret där antalet units är antalet kategorier
  layer_dense(
    units = 1
  )
summary(nn_model1)

nn_model1 %>% compile(
  optimizer = optimizer_adam(learning_rate=0.01),
  loss = loss_binary_crossentropy(from_logits = TRUE),
  metrics = c('accuracy')
)

set_random_seed(seed=542)
history1 <- nn_model1 %>% fit(
  x = train_data, y = y_train, 
  epochs = 30, 
  batch_size = 64, 
  shuffle=TRUE,
  validation_split = 0 
)
# träffsäkerhet:
(nn_model1%>% evaluate(x = train_data,y = y_train,verbose = 0))[[1]]
#-------------------------------------------------------------------------------






#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
# Autoencoder
#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------


#-------------------------------------------------------------------------------
# Del 1: Encoder
#-------------------------------------------------------------------------------

# sätter input:
input_layer <- layer_input(shape = dim(train_data)[2])

# definerar encoder:
encoder <- 
  input_layer %>% 
  layer_dense(units = 100, activation = "relu") %>% 
  layer_batch_normalization() %>%
  layer_dense(units = 50, activation = "relu") %>% 
  layer_batch_normalization() %>%  
  layer_dense(units = 25, activation = "relu") %>% 
  layer_batch_normalization() %>% 
  layer_dense(units = 2,activation = "relu") # bottleneck

#-------------------------------------------------------------------------------
# Del 2: Decoder
#-------------------------------------------------------------------------------
decoder <- 
  encoder %>% 
  layer_dense(units = 25, activation = "relu") %>% 
  layer_batch_normalization() %>%
  layer_dense(units = 50, activation = "relu") %>% 
  layer_batch_normalization() %>%
  layer_dense(units = 100, activation = "relu") %>% 
  layer_dense(units = dim(train_data)[2], activation = "linear")


#-------------------------------------------------------------------------------
# Kombinerar delarna:
#-------------------------------------------------------------------------------
autoencoder_model <- keras_model(inputs = input_layer, outputs = decoder)

autoencoder_model %>% compile(
  loss = 'mean_squared_error',
  optimizer = optimizer_adam(learning_rate=0.01),
  metrics = c() 
)

summary(autoencoder_model)
#plot(autoencoder_model,show_shapes = TRUE)


set_random_seed(seed=753)
#set_random_seed(seed=3)
history <- autoencoder_model %>% fit(
  x = train_data, 
  y = train_data, 
  epochs = 50, 
  shuffle = TRUE, 
  batch_size = 128,
  validation_split = .2,
)
#plot(history) + theme_bw()

X_hat<-autoencoder_model %>% predict(x = train_data)

# kan kolla anpassningen på X om vi vill, blir många plottar.
many_plots<-FALSE
if(many_plots){
  cor_vect<-rep(0,57)
  for(i in 1:57){
    cor_temp<-cor(train_data[,i],X_hat[,i])
    cor_vect[i]<-cor_temp
    plot(train_data[,i],X_hat[,i],main=paste0("X-var: ",i,", cor: ",cor_temp),
         xlab="original X",ylab="X_hat")
  }
  hist(cor_vect,15)
  summary(cor_vect)
}


#-----------------------------------



# sätt en sökväg + namn för att spara modellen:
save_path<-"min sökväg till en mapp/spambase_autoencoder_model.keras"

autoencoder_model |> save_model(save_path,overwrite = TRUE)

#autoencoder_model_load <- load_model(model = save_path)

# definerar bara encoder:
encoder_model <- keras_model(inputs = input_layer, outputs = encoder)
#plot(encoder_model,show_shapes = TRUE)

# lägger till skattade parametarar till encoder:
encoder_model %>% load_model_weights(filepath = save_path,
                                     skip_mismatch = TRUE)

encoder_model %>% compile(
  loss='mean_squared_error',
  optimizer='adam',
  metrics = c('accuracy')
)

# Gör prediktioner med encoder och får data med 2 dimensioner:
embeded_points <- 
  encoder_model %>% 
  keras::predict_on_batch(x = train_data)

head(embeded_points,20)

# skapa data.frame med nya variabler:
df<-data.frame(embeded_points,Spam=as.factor(D3$Spam))

ggpairs(data = df,mapping = aes(col=Spam,alpha=0.1))

ggplot(data = df,mapping = aes(x=X1,y=X2,col=Spam))+geom_point(alpha=0.2)

# B<-princomp(df[,1:2],cor = TRUE)
# df2<-data.frame(B$scores,Spam=as.factor(D3$Spam))
# ggpairs(data = df2,mapping = aes(col=Spam,alpha=0.2))


#-------------------------------------------------------------------------------
# logistik regression
#-------------------------------------------------------------------------------
log_reg_obj2<-glm(formula = Spam~.,data = df,family=binomial(link='logit'))
log_reg_obj2_fit<-ifelse(predict(log_reg_obj2,type = "response")>=0.5,1,0)
# träffsäkerhet:
mean(log_reg_obj2_fit==D3$Spam)
#-------------------------------------------------------------------------------



#-------------------------------------------------------------------------------
# KNN
#-------------------------------------------------------------------------------
set.seed(675)
knn_model <- train(form = Spam ~ ., 
                   data = df, 
                   method = "knn", 
                   # Standardiserar förklarande variabler
                   preProcess = c("center", "scale"), 
                   trControl = trainControl(
                     # Anger att korsvalidering ska köras
                     method = "repeatedcv", 
                     # Anger antalet k i k-fold korsvalidering, alltså inte k för KNN
                     number = 10, 
                     # Repeterar valideringen tre gånger
                     repeats = 3, 
                     # Anger att manuell val av utforskade k kommer ges, anges i tuneGrid
                     search = "grid"), 
                   # Anger vilka k som ska letas igenom i valideringen
                   tuneGrid = expand.grid(k = c(5, 7, 9, 11, 13, 15, 17, 19, 21)) 
) 
knn_model
class_evaluation(new_data = df[,-ncol(df),drop=FALSE],model = knn_model,true_y = df$Spam,type = "raw")
#-------------------------------------------------------------------------------



#-------------------------------------------------------------------------------
# (iii) MLP
#-------------------------------------------------------------------------------

# har embeded_points som träningsdata
nn_model2 <- keras_model_sequential()
nn_model2 %>%
  ## Definierar första lagret, input_shape ska anges för att definiera hur många förklarande variabler som finns i data
  layer_dense(
    # Anger antal gömda noder i det GÖMDA lagret
    units = 50, 
    # Anger aktiveringsfunktionen i lagret
    activation = "relu", 
    # Det första gömda lagret anger hur många förklarande variabler som ska kopplas till lagret
    # Notera att input_shape styr input-lagret, units styr det gömda lagret
    input_shape = c(ncol(embeded_points)),
    # Anger att vi vill ha med en bias-term i lagret
    use_bias = TRUE
  ) %>% layer_dense(
    # Anger antal gömda noder i det GÖMDA lagret
    units = 50, 
    # Anger aktiveringsfunktionen i lagret
    activation = "relu", 
    # Det första gömda lagret anger hur många förklarande variabler som ska kopplas till lagret
    # Notera att input_shape styr input-lagret, units styr det gömda lagret
    # Anger att vi vill ha med en bias-term i lagret
    use_bias = TRUE
  ) %>%
  ## Anger det sista lagret där antalet units är antalet kategorier
  layer_dense(
    units = 1
  )
summary(nn_model2)

nn_model2 %>% compile(
  optimizer = optimizer_adam(learning_rate=0.01),
  loss = loss_binary_crossentropy(from_logits = TRUE),
  metrics = c('accuracy')
)

set_random_seed(seed=222)
history2 <- nn_model2 %>% fit(
  x = embeded_points, y = y_train, 
  epochs = 30, 
  batch_size = 64, 
  shuffle=TRUE,
  validation_split = 0 
)
# träffsäkerhet:
(nn_model2%>% evaluate(x = embeded_points,y = y_train,verbose = 0))[[1]]
#-------------------------------------------------------------------------------










#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
# PCA
#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
A<-princomp(x = train_data,cor = TRUE)
summary(A)
plot(A)
plot(A$scores[,1:2],col=D3$Spam+1,pch=16)

df_pca<-data.frame(A$scores[,1:5],Spam=as.factor(D3$Spam))
ggpairs(data = df_pca,mapping = aes(col=Spam))

# skatta modeller med df_pca