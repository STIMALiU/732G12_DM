
library(rpart)
library(rpart.plot)

x<-seq(0,2*pi,length=300)
set.seed(345)

# testa med olika y:
y<-3*sin(x*2)+rnorm(n = 300,sd = 0.3)
#y<-ifelse(test = x>3,4,-2)+rnorm(n = 300,sd = 0.1)


plot(x,y)
df<-data.frame(x=x,y=y)

# viktiga hyperparameterar:
# Anger att antalet observationer som krävs för att en förgrening ska ske
# minsplit = 20,
# Anger maxdjupet av träder, där 0 är rotnoden
# maxdepth = 10, 
# Anger den minsta tillåtna förbättringen som måste ske för att en förgrening ska ske
# cp = 0,

# testa att ändra dessa till olika värden och notera hur det skattade trädet och
# anpassade värden ändras


# med y alt 2: testa cp = 0, 0.1, 0.001 och se ev skillnad i resultatet


tree <- rpart(
  formula = y ~ .,
  data = df,
  ,
  # Anger föroreningsmått
  parms = list(split = "mse"), 
  
  control = list(
    ## Stopkriterier
    # Anger att antalet observationer som krävs för att en förgrening ska ske
    minsplit = 20,
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

rpart.plot(tree)
y_hat<-predict(tree)

plot(x,y)
lines(x,y_hat,lwd=2,col="red")






