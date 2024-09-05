# från:
# https://www.isakhietala.com/teaching/732g12/#sammanfattande-funktion-f%C3%B6r-modellutv%C3%A4rdering 


## Funktion som skapar förväxlingsmatris med tillhörande mått för klassificeringsproblem

# new_data  : Materialet innehållande förklarande variabler 
# model     : Den skattade modellen
# true_y    : Vektor innehållande de sanna klass-värden i samma ordning som new_data

class_evaluation <- function(new_data, model, true_y, type = "class", digits = 3){
  library(stringr)
  # Predikterar klassen för new_data givet den skattade modellen
  if(any(str_detect(class(model), pattern = "keras"))){
    pred <- model %>% predict(new_data) %>% k_argmax()  
  } else {
    pred <- predict(model, newdata = new_data, type = type)  
  }
  
  # Konverterar de predikterade klasserna till en faktor med samma nivåer som de sanna klasserna
  pred <- factor(pred, levels = levels(factor(true_y)))
  
  # Skapar förväxlingsmatris med rader som indikerar sanna klassen och kolumnen som indikerar predikterade klassen
  confusion <- table(true_y, pred)
  
  # Träffsäkerhet är antalet korrekta prediktioner dividerat med antalet observationer
  accuracy <- sum(diag(confusion)) / sum(confusion)
  
  # Felkvoten är 1 - träffsäkerheten
  misclass <- 1 - accuracy
  
  # Sensitivitet är antalet korrekta prediktioner AV DEN NUVARANDE KLASSEN dividerat med antalet observationer AV DEN NUVARANDE KLASSEN
  sensitivity <- diag(confusion) / rowSums(confusion) 
  
  # Specificitet är antalet korrekta prediktioner AV ICKE DEN NUVARANDE KLASSEN dividerat med antalet observationer AV ICKE DEN NUVARANDE KLASSEN
  specificity <- NULL
  for(i in 1:nrow(confusion)){
    specificity[i] <- sum(confusion[-i, -i])/sum(confusion[-i, ])
  }
  
  # Sammanställer alla resultat i en egen lista som sedan returneras
  evaluation <- list(confusion_matrix = confusion, 
                     overall = 
                       cbind(acc = accuracy, 
                             mis = misclass),
                     class_wise = 
                       rbind(sensitivity = round(sensitivity, digits = digits),
                             specificity = round(specificity, digits = digits))
  )
  
  return(evaluation)
}