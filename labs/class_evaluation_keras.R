# new_data - new data for prediction
# model    - a keras model
# digits   - number of digits used 

# funktionen använder k_argmax() från paketet keras
class_evaluation_keras <- function(new_data, model, true_y, digits = 3){
  # Predikterar klassen för new_data givet den skattade modellen
  class_pred <- model %>% predict(new_data) %>% keras::k_argmax()
  pred<-as.vector(as.array(class_pred))

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