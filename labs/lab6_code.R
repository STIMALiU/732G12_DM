#install.packages("arules")

library(arules)

path<-"path to data"

data_assoc<-read.csv2(file = path)
names(data_assoc)
head(data_assoc)

table(data_assoc$item)
length(unique(data_assoc$ID))
dim(data_assoc)
trans <- as(split(data_assoc[,"PRODUCT"], data_assoc[,"TRANS_ID"]), "transactions")
trans
class(trans)
str(trans)
class(trans)
str(trans)
trans@data
trans@itemInfo
trans@itemsetInfo

?apriori

# Kan titta i materialet med inspect()
inspect(head(trans, n = 10))
inspect(tail(trans, n = 10))

## Kör apriori-algoritmen
rules <- apriori(data = trans, 
                 parameter = list(
                   # Definierar supporttröskeln
                   supp = 0.1, 
                   # Definierar konfidenströskeln
                   conf = 0.4, 
                   # Definierar minsta antalet enheter i sökta enhetsmängder
                   minlen = 2, 
                   # Definierar största antalet enheter i sökta enhetsmängder
                   maxlen = 3)
) 
#rules
#support()
#rhs
#str(rules)
L<-lhs(x = rules)
support(x = L,transactions = trans)
mean(trans@data[10,])

trans@itemInfo[10,]

inspect(head(rules, n = 100))
dim(L@data)
colSums(L@data)

L@data[,]


inspect(
  subset(
    # Anger vilket objekt som innehåller alla regler
    rules, 
    # Urvalet nedan plockar ut alla regler som har bicuits i högerledet
    subset = rhs %in% "biscuits")) 

inspect(
  subset(
    # Anger vilket objekt som innehåller alla regler
    rules, 
    # Urvalet nedan plockar ut alla regler som har bicuits i högerledet
    subset = lhs %in% "tunny")) 


## inspect för att plocka ut en tabell över resultatet
inspect(head(rules, n = 5))

## sort används för att sortera utefter någon utav måtten i tabellen
inspect(head(sort(rules, by = "confidence", decreasing = TRUE), n = 5))

inspect(head(sort(rules, by = "support", decreasing = TRUE), n = 5))
inspect(head(sort(rules, by = "lift", decreasing = TRUE), n = 5))




