
library(arules)
rm(list=ls())
# sökväg till data:
path<-"https://raw.githubusercontent.com/STIMALiU/732G12_DM/master/data/marbas.csv"

data_assoc<-read.csv2(file = path)
names(data_assoc)
head(data_assoc)

table(data_assoc$PRODUCT)
length(unique(data_assoc$TRANS_ID))
dim(data_assoc)
trans <- as(split(data_assoc[,"PRODUCT"], data_assoc[,"TRANS_ID"]), "transactions")
trans
class(trans)
str(trans)
trans@data
trans@itemInfo
trans@itemsetInfo
summary(trans)
inspect(head(trans))
itemFrequencyPlot(trans)
barplot(sort(itemFrequency(trans)),cex.names = 1)
itemFrequency(trans)

# Kan titta i materialet med inspect()
inspect(head(trans, n = 10))
inspect(tail(trans, n = 10))


#-------------------------------------------------------------------------------
# 2)
# Skapa en associationsanalys med en supporttröskel på 5 procent, det maximala antalet enheter i
# en regel till 2, och konfidenströskel på 50 procent.
#-------------------------------------------------------------------------------
## Kör apriori-algoritmen
?apriori
rules <- apriori(data = trans, 
                 parameter = list(
                   # Definierar supporttröskeln
                   supp = 0.1, 
                   # Definierar konfidenströskeln
                   conf = 0.5, 
                   # Definierar minsta antalet enheter i sökta enhetsmängder
                   minlen = 1, 
                   # Definierar största antalet enheter i sökta enhetsmängder
                   maxlen = 2)
) 

#-------------------------------------------------------------------------------
# Hur många regler fås ut från algoritmen? Visa de fem regler som har högst support och de fem
# regler som har högst konfidens. Vilka av dessa anser ni vara intressanta från er synvinkel?
#-------------------------------------------------------------------------------
rules # 22 rules

inspect(rules)
inspect(sort(rules, by = "support", decreasing = TRUE))
inspect(sort(rules, by = "confidence", decreasing = TRUE))
inspect(sort(rules, by = "lift", decreasing = TRUE))

inspect(head(sort(rules, by = "confidence", decreasing = FALSE)))
inspect(head(sort(rules, by = "support", decreasing = FALSE)))