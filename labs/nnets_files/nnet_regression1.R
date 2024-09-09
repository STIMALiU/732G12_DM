#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
# Regression med neurala nätverk
#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------

library(keras3)
library(ggplot2)

# syftet med denna kod är vissa hur vi kan använda Keras för att skatta 
# vanlig linjär regression, vilket kan vara användbart om vi har många
# observationer och/eller förklarande variabler.


#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
# Enkel linjär regression
#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------


#-------------------------------------------------------------------------------
# Skapa dataset
#-------------------------------------------------------------------------------

# Om vi har ett neuralt nätverk med inga gömda lager och identitetsfunktionen som
# aktiveringsfunktion i sista lagret så får vi den vanliga regressionsmodellen.

# genera regressionsdata:


reg_data_simulate<-function(beta0,beta1,n,sd_noise,seed=NULL,x_min=-5,x_max=5){
  set.seed(seed)
  x<-scale(sort(runif(n = n,min = x_min,max = x_max)))[,]
  y<-beta0+beta1*x +rnorm(n = n,mean = 0,sd = sd_noise)
  df<-data.frame(x=x,y=y)
  return(df)
}
n<-300
df<-reg_data_simulate(beta0 = 10,beta1 = 2,x_min = 0,x_max = 10,
                      n = n,sd_noise = 0.1,seed=321)

plot(df)

# vi standardiserar x:
df$x<-scale(df$x)[,]
plot(df)

# skapar matriser med träningsdata:
x_train<-matrix(df$x,ncol=1)
y_train<-matrix(df$y,ncol=1)

#-------------------------------------------------------------------------------
## Modell
#-------------------------------------------------------------------------------
# Skapar grundmodell
nn_model1 <- keras_model_sequential()
# Notera: om ni vill träna om modellen "från början" så behöver ni skapa om 
# grundmodellen och sen köra stegen nedan i ordning.


# Skapar arkitekturen
# 1 x-variabler -> 1 output nod
nn_model1 %>%
  ## Anger det sista lagret där antalet units är 1
  layer_dense(
    units = 1, 
    input_shape = c(1),
    activation = "linear"
  )

## Anger en översiktsbild av den angivna arkitekturen
summary(nn_model1)
# 2 parametrar: beta0 och beta1

#-------------------------------------------------------------------------------
# Kompilerar modellen
#-------------------------------------------------------------------------------

# Här bestämmer vi vilken optimierng vi vill ha, kostandsfunktion och 
# utvärderignsmått


nn_model1 %>% compile(
  # Typ av optimering och inlärningstakt 
  optimizer = optimizer_sgd(learning_rate=0.2),
  # Anger förlustfunktionen som vi vill använda. Denna styrs direkt av vilken typ av y-variabel som finns i data
  loss = 'MSE',
  # Anger att träffsäkerheten ska användas som mått för utvärdering
  metrics = c('MSE')
)


#-------------------------------------------------------------------------------
# Skatta modellen
#-------------------------------------------------------------------------------

# skattar modellen med vanliga gradient decent:
set_random_seed(seed=232)
history <- nn_model1 %>% fit(
  x = x_train, y = y_train, 
  epochs = 30, 
  # här kan man ändra till batch-learning (= vanliga gradient decent) genom att ange nrow(x_train)
  batch_size = n, 
  validation_split = 0 
)

# träningshistorik
plot(history)

#-------------------------------------------------------------------------------
# Utvärdera modellen
#-------------------------------------------------------------------------------


# prediktionerna för testdata
y_hat <- nn_model1 %>% predict(x_train)


df$y_hat<-y_hat

ggplot(data=df,aes(x=x,y=y))+geom_point()+
  geom_line(aes(y=y_hat),col="red",linewidth=1)

beta_temp<-get_weights(nn_model1)
beta1_hat<-beta_temp[[1]][1]
beta0_hat<-beta_temp[[2]][1]
# nära de sanna värdena som är 2 och 10:
beta1_hat
beta0_hat

# utvärdera träningsdata:
score <- nn_model1 %>% evaluate(x_train, y_train, verbose = 0)
cat('Train loss:', score[["loss"]], "\n")

# gör en lm()

lm_obj<-lm(y~x,data = df)
summary(lm_obj)
# beräknar MSE
mean((df$y-fitted(lm_obj))^2)
# väldigt likt det som modellen ovan får.



#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
# Fler antal obs och SGD
#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------

n2<-5e5
df2<-reg_data_simulate(beta0 = 10,beta1 = 2,x_min = 0,x_max = 10,
                       n = n2,sd_noise = 0.1,seed=321)
dim(df2)

# vi standardiserar x:
df2$x<-scale(df2$x)[,]
# skapar matriser med träningsdata:
x_train<-matrix(df2$x,ncol=1)
y_train<-matrix(df2$y,ncol=1)



# Skapar grundmodell
nn_model2 <- keras_model_sequential()
# Notera: om ni vill träna om modellen "från början" så behöver ni skapa om 
# grundmodellen och sen köra stegen nedan i ordning.


# Skapar arkitekturen
# 1 x-variabler -> 1 output nod
nn_model2 %>%
  ## Anger det sista lagret där antalet units är 1
  layer_dense(
    units = 1, 
    input_shape = c(1),
    activation = "linear"
  )

## Anger en översiktsbild av den angivna arkitekturen
summary(nn_model2)
# 2 parametrar: beta0 och beta1

#-------------------------------------------------------------------------------
# Kompilerar modellen
#-------------------------------------------------------------------------------

# Här bestämmer vi vilken optimierng vi vill ha, kostandsfunktion och 
# utvärderignsmått

nn_model2 %>% compile(
  optimizer = optimizer_sgd(learning_rate=0.1),
  loss = 'MSE',
  metrics = c('MSE')
)


#-------------------------------------------------------------------------------
# Skatta modellen
#-------------------------------------------------------------------------------

## Anpassar modellen med fif()
set_random_seed(seed=643)
system.time({
  history <- nn_model2 %>% fit(
    x = x_train, y = y_train, 
    epochs = 5, 
    # här kan man ändra till batch-learning (= vanliga gradient decent) genom att ange nrow(x_train)
    batch_size = 128, 
    validation_split = 0 
  )
})
# kort tid kan vi skatta en modell där data har 5e5 obs


# träningshistorik
plot(history)

#-------------------------------------------------------------------------------
# Utvärdera modellen
#-------------------------------------------------------------------------------


beta_temp<-get_weights(nn_model2)
beta1_hat<-beta_temp[[1]][1]
beta0_hat<-beta_temp[[2]][1]
# nära de sanna värdena som är 2 och 10:
beta1_hat
beta0_hat

# utvärdera träningsdata:
score <- nn_model2 %>% evaluate(x_train, y_train, verbose = 0)
cat('Train loss:', score[["loss"]], "\n")



#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
# Multipel linjär regression
#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------

# Vi kan enkelt utöka till en linjär modell med flera förklarande variabler

# skapa lite data:

beta0<- -17
beta1<-8
beta2<-0.5
beta3<-0
beta4<- -4
beta5 <-0
beta_vect<-c(beta0,beta1,beta2,beta3,beta4,beta5)

n3<-5000
x_train<-matrix(data = rnorm(n = n3*5,mean = 0,sd = 1),ncol = 5)
dim(x_train)

set.seed(469)
y<-cbind(1,x_train)%*%beta_vect +rnorm(n = n3,sd = 1)

y_train<-matrix(y)
df<-data.frame(y=y,x_train)
head(df)
#-------------------------------------------------------------------------------
# testar en lm() först:
lm_obj<-lm(y~.,data=df)
summary(lm_obj)
coef(lm_obj)
# jämför med
beta_vect
#-------------------------------------------------------------------------------

# skatta neuralt nätverk

nn_model3 <- keras_model_sequential()

# Skapar arkitekturen
# 5 x-variabler -> 1 output nod
nn_model3 %>%
  ## Anger det sista lagret där antalet units är 1
  layer_dense(
    units = 1, 
    input_shape = c(5),
    activation = "linear"
  )

summary(nn_model3)
# 6 parametrar: beta0 till beta5

#-------------------------------------------------------------------------------
# Kompilerar modellen
#-------------------------------------------------------------------------------

nn_model3 %>% compile(
  optimizer = optimizer_sgd(learning_rate=0.1),
  loss = 'MSE',
  metrics = c('MSE')
)

set_random_seed(seed=4323)
system.time({
  history <- nn_model3 %>% fit(
    x = x_train, y = y_train, 
    epochs = 10, 
    # här kan man ändra till batch-learning (= vanliga gradient decent) genom att ange nrow(x_train)
    batch_size = 128, 
    validation_split = 0 
  )
})


#-------------------------------------------------------------------------------
# Utvärdera modellen
#-------------------------------------------------------------------------------


beta_temp<-get_weights(nn_model3)
beta_temp
beta_vect_hat<-c(beta_temp[[2]][1],beta_temp[[1]][,1])
beta_vect_hat
# jämför med sanna värden:
beta_vect

# utvärdera träningsdata:
score <- nn_model23 %>% evaluate(x_train, y_train, verbose = 0)
cat('Train loss:', score[["loss"]], "\n")
