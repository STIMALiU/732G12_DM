

# denna kod utgår från keras3 (se paketet keras för en äldre version)
# installera keras:
# https://hastie.su.domains/ISLR2/keras-instructions.html
# https://rstudio.github.io/cheatsheets/html/keras.html?_gl=1*1p01l90*_ga*MTczNDI3OTQ5Ni4xNzIxNzQyNDAw*_ga_2C0WZ1JHG0*MTcyNTQ1NjczMS44LjEuMTcyNTQ1NjgwNy4wLjAuMA..
library(keras3)



# Läser in funktionen class_evaluation_keras från github:
source("https://raw.githubusercontent.com/STIMALiU/732G12_DM/master/labs/class_evaluation_keras.R")


#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
# Del 1: Neurala Nätverk: klassificering
#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------



#-------------------------------------------------------------------------------
# Förbreder data:
#-------------------------------------------------------------------------------
# ange rätt sökvägar till filerna:
path_train<-"/home/joswi05/Documents/732G12_admin/mnist_train.csv"
path_test<-"/home/joswi05/Documents/732G12_admin/mnist_test.csv"

mnist_train <- read.csv2(file = path_train, sep = ";", dec = ",")
mnist_test <- read.csv2(file = path_test, sep = ";", dec = ",")

#colnames(mnist_train)

x_train <- as.matrix(mnist_train[, -785])
x_test <- as.matrix(mnist_test[, -785])

# kollar storleken:
dim(x_train)
dim(x_test)

colnames(x_train)[1:50]



y_train_vect<-mnist_train[,785]
y_test_vect<-mnist_test[,785]
# one-hot encoding av vår kategoriska responsvariabel
# vektor med 10 klasser blir en binär matris med 10 kolumner här
y_train <- to_categorical(mnist_train[, 785,drop=FALSE], num_classes = 10)
y_test <- to_categorical(mnist_test[, 785], num_classes = 10)

# Vilken klass har rad 2 och 4 nedan?
head(y_train)
mnist_train[1:6,785]

head(y_test)
mnist_test[1:6,785]


library(Matrix)
# kollar på två bilder:
image_temp1<-Matrix(x_train[2,],nrow = 28,ncol = 28,byrow = TRUE)
image_temp2<-Matrix(x_train[5361,],nrow = 28,ncol = 28,byrow = TRUE)
image(image_temp1)
image(image_temp2)
# vilka siffror ser ni?
# jämför med:
mnist_train[2,785]
mnist_train[5361,785]

#-------------------------------------------------------------------------------
# Skapar ett neuralt nätverk
#-------------------------------------------------------------------------------

## Modell
# Skapar grundmodell
nn_model <- keras_model_sequential()
# Notera: om ni vill träna om modellen "från början" så behöver ni skapa om 
# grundmodellen och sen köra stegen nedan i ordning.


# Skapar arkitekturen
# 784 x-variabler -> 10 gömda noder med relu -> 10 output noder med softmax
nn_model %>%
  ## Definierar första lagret, input_shape ska anges för att definiera hur många förklarande variabler som finns i data
  layer_dense(
    # Anger antal gömda noder i det GÖMDA lagret
    units = 10, 
    # Anger aktiveringsfunktionen i lagret
    activation = "relu", 
    # Det första gömda lagret anger hur många förklarande variabler som ska kopplas till lagret
    # Notera att input_shape styr input-lagret, units styr det gömda lagret
    input_shape = c(ncol(x_train)),
    # Anger att vi vill ha med en bias-term i lagret
    use_bias = TRUE
  ) %>%
  ## Anger det sista lagret där antalet units är antalet kategorier
  layer_dense(
    units = 10, 
    activation = "softmax"
  )

## Anger en översiktsbild av den angivna arkitekturen
summary(nn_model)

#-------------------------------------------------------------------------------
# Kompilerar modellen
#-------------------------------------------------------------------------------

# Här bestämmer vi vilken optimierng vi vill ha, kostandsfunktion och 
# utvärderignsmått

?compile.keras.src.models.model.Model 
?optimizer_sgd 
?loss_categorical_crossentropy

nn_model %>% compile(
  # Typ av optimering och inlärningstakt 
  optimizer = optimizer_sgd(learning_rate=0.1),
  # Anger förlustfunktionen som vi vill använda. Denna styrs direkt av vilken typ av y-variabel som finns i data
  loss = 'categorical_crossentropy',
  # Anger att träffsäkerheten ska användas som mått för utvärdering
  metrics = c('accuracy')
)



#-------------------------------------------------------------------------------
# Skatta modellen
#-------------------------------------------------------------------------------

## Anpassar modellen med fif()
?fit.keras.src.models.model.Model 

?set_random_seed
set_random_seed(seed=232)
history <- nn_model %>% fit(
  # Anger data
  x = x_train, y = y_train, 
  # Maximala antalet gånger alla observationer i träningsmängden ska gås igenom innan anpassningen avslutas
  epochs = 30, 
  # Anger hur många observationer som ska gås igenom innan vikterna uppdateras, här kan man ändra till batch-learning genom att ange nrow(x_train)
  # Anger hur många observationer som ska vara med slumpmässiga urvalet för SGD,
  # här kan man ändra till batch-learning (= vanliga gradient decent) genom att ange nrow(x_train)
  batch_size = 128, 
  # Anger hur mycket av träningsdata som ska användas som valideringsmängd. 
  # Plockar ur data indexerat från slutet, så var vaksam på att data måste vara slumpat innan
  # Om man har en separat valideringsmängd används validation_data = data
  validation_split = 0 # 0 % av träningsdata används som valideringsdata
)

# träningshistorik
plot(history)


#-------------------------------------------------------------------------------
# Göra prediktioner
#-------------------------------------------------------------------------------


class_pred_train <- nn_model %>% predict(x_train)
# är en matris med sannolikheter för alla klasser

# Undersöker en obs:
class_pred_train[3,]
barplot(class_pred_train[3,],names.arg = 0:9)
sum(class_pred_train[3,])
# sann klass för obs 3:
y_train_vect[3]

round(class_pred_train[1:5,],2)

# ta fram klasserna för prediktionerna som en vektor:
temp<-class_pred_train %>% keras::k_argmax()
class_pred_train_vect<-as.vector(as.array(temp))

class_pred_train_vect


# prediktionerna för testdata
class_pred_test <- nn_model %>% predict(x_test)

#-------------------------------------------------------------------------------
# Utvärdera modellen
#-------------------------------------------------------------------------------



# utvärdera träningsdata:
score <- nn_model %>% evaluate(x_train, y_train, verbose = 0)
cat('Train loss:', score[["loss"]], "\n")
cat('Train accuracy:', score[["accuracy"]], "\n")


# utvärdera testdata:
score <- nn_model %>% evaluate(x_test, y_test, verbose = 0)
cat('Test loss:', score[["loss"]], "\n")
cat('Test accuracy:', score[["accuracy"]], "\n")


# förväxlingsmatris mm:
class_evaluation_keras(new_data = x_train, model = nn_model, true_y = y_train_vect)
class_evaluation_keras(new_data = x_test, model = nn_model, true_y = y_test_vect)


# vi kan undersöka hur säkra våra prediktioner är i genomsnitt genom att kolla 
# på värdet av arg_max k P(Y="k") för alla observationer som vi beräknar 
# anpassade värden för, dvs vi kan kolla på radvisa maxvärden i matriserna
# class_pred_train och class_pred_test

p_max<-apply(X = class_pred_train,MARGIN = 1,FUN = max)
hist(p_max,100)
boxplot(p_max)
summary(p_max)
quantile(x = p_max,probs = c(0,0.01,0.05,0.1))
# modellen är väldigt säker på sina prediktioner då 75% av alla 
# max-sannolikheter > 0.9512
# Notera att en dålig modell kan ha "säkra" prediktioner


p_max_test<-apply(X = class_pred_test,MARGIN = 1,FUN = max)
hist(p_max_test,100)
boxplot(p_max_test)
summary(p_max_test)
quantile(x = p_max_test,probs = c(0,0.01,0.05,0.1))

