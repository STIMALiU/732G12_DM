#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
# Labb 2
#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------





#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
# Del 1: Trädmodeller
#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------




#-------------------------------------------------------------------------------
# Begränsningar av beslutsträd
#-------------------------------------------------------------------------------

# 1)
D<-read.csv2(file = "https://raw.githubusercontent.com/STIMALiU/732G12_DM/master/data/issue.csv")

head(D)
dim(D)
library(ggplot2)
qplot(x = D$X1,y =D$X2,geom = "point",col=as.factor(D$Y))+theme_bw()
# en linjär beslutsgräns -> logistisk linjär regression är bra här


set.seed(762)
train_index<-sample(x = nrow(D),size = nrow(D)/2,replace = FALSE)
D_train<-D[train_index,]
D_valid<-D[-train_index,]
names(D)



# 2)
library(rpart)

tree1 <- rpart(
  formula = Y ~ .,
  data = D_train,
  method = "class",
  # Anger föroreningsmått
  parms = list(split = "gini"), 
  
  control = list(
    ## Stopkriterier
    # Anger att antalet observationer som krävs för att en förgrening ska ske
    minsplit = 10,
    # Anger maxdjupet av träder, där 0 är rotnoden
    maxdepth = 5, 
    # Anger den minsta tillåtna förbättringen som måste ske för att en förgrening ska ske
    cp = 0,
    # Två inställningar som inte används mer i detalj
    maxcompete = 0,
    maxsurrogate = 0,
    
    ## Trädanpassning
    # Anger antalet korsvalideringar som ska ske medan modellens tränas, intern validering
    xval = 0, 
    # Tillåter att förgreningar har surrogatregler som kan användas vid saknade värden
    # Ska vara 2 om saknade värden finns i datamaterialet
    usesurrogate = 0
  )
)

## Sammanfattar alla noder, deras klassindelningar och fel
summary(tree1, digits = 3) 
print(tree1)




plot(tree1, 
     # Justerar placeringen av noder
     uniform = TRUE,
     # Lägger till vita kanter för att inte texten ska försvinna utanför diagrammet
     margin = 0.05) 

## Lägger till information vid varje split i trädet
text(tree1, 
     # Lägger till all information i förgreningen
     all = TRUE,
     # Styr storleken av texten som läggs till
     cex = 0.6) 

library(rpart.plot)
rpart.plot(tree1)


# 3)

# a)

# läser in class_evaluation()
source("https://raw.githubusercontent.com/STIMALiU/732G12_DM/master/labs/class_evaluation.R")
library(stringr)
eval_tree1<-class_evaluation(new_data = D_train,model = tree1,true_y = D_train$Y,digits=3 )
eval_tree1
eval_tree1$overall
# error rate = 0.02666667

eval_tree1_valid<-class_evaluation(new_data = D_valid,model = tree1,true_y = D_valid$Y,digits=3 )
eval_tree1_valid
eval_tree1_valid$overall
# error rate = 0.046
# så vi har lite större fel på valideringsmängden, vilket är vanligt

# b)
?rpart.object

# ger info om noderna
leaf_index<-tree1$frame$var=="<leaf>"

# antal löv
sum(leaf_index)  # 17 löv
# teoretiskt maxantal med djup=5: 2^5 = 32

# så vi får ett ganska komplicerat träd

tree1$frame[leaf_index,1:3]

# undersöka storleken på löven: 
plot(sort(tree1$frame[leaf_index,2]))

# djup
nodes <- as.numeric(rownames(tree1$frame))
max(rpart:::tree.depth(nodes))
# alt
rpart.plot(tree1) # kolla djup i plotten



# 4)
qplot(x = D$X1,y =D$X2,geom = "point",col=as.factor(D$Y))+theme_bw()

# 5)

y_hat_valid<-predict(object = tree1,newdata = D_valid,type = "class")

qplot(x = D_valid$X1,y =D_valid$X2,geom = "point",col=as.factor(y_hat_valid))+theme_bw()
# beslutsgränsen ser ut som en trappa 

# 6)

tree2 <- rpart(
  formula = Y ~ .,
  data = D_train,
  method = "class",
  # Anger föroreningsmått
  parms = list(split = "gini"), 
  
  control = list(
    ## Stopkriterier
    # Anger att antalet observationer som krävs för att en förgrening ska ske
    minsplit = 10,
    # Anger maxdjupet av träder, där 0 är rotnoden
    maxdepth = 10, 
    # Anger den minsta tillåtna förbättringen som måste ske för att en förgrening ska ske
    cp = 0,
    # Två inställningar som inte används mer i detalj
    maxcompete = 0,
    maxsurrogate = 0,
    
    ## Trädanpassning
    # Anger antalet korsvalideringar som ska ske medan modellens tränas, intern validering
    xval = 0, 
    # Tillåter att förgreningar har surrogatregler som kan användas vid saknade värden
    # Ska vara 2 om saknade värden finns i datamaterialet
    usesurrogate = 0
  )
)
rpart.plot(tree1)
rpart.plot(tree2)
y_hat_valid<-predict(object = tree2,newdata = D_valid,type = "class")
qplot(x = D_valid$X1,y =D_valid$X2,geom = "point",col=as.factor(y_hat_valid))+theme_bw()
# liknande beslutsgräns

# ger info om noderna
leaf_index<-tree2$frame$var=="<leaf>"
# antal löv
sum(leaf_index)  # 21 löv (tree1 hade 17)
# teoretiskt maxantal med djup=5: 2^10 = 1024
# så vi får ett träd ungefär lika komplicerat träd som med maxdepth = 5


tree2$frame[leaf_index,1:3]

# undersöka storleken på löven: 
plot(sort(tree2$frame[leaf_index,2]))

# djup
nodes <- as.numeric(rownames(tree2$frame))
max(rpart:::tree.depth(nodes))

eval_tree2_valid<-class_evaluation(new_data = D_valid,model = tree2,true_y = D_valid$Y,digits=3 )
eval_tree2_valid
eval_tree2_valid$overall



