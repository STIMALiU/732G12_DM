


# läsa in funktionen:
source( "https://raw.githubusercontent.com/STIMALiU/RegVar_student/main/lab8_code/trunc_power_basis.R")


source("https://raw.githubusercontent.com/STIMALiU/RegVar_student/main/general_code/lm_diagnostics.R")

# skapa data:
x<-seq(0,1,length=1000)
set.seed(64)
y<-sin(pi*x*7)+x*5+rnorm(n = 1e3,sd=0.3)
plot(x,y)


B_linear_list<-trunc_power_basis(x = x,no_basis = 100,type = "linear")
B_quad_list<-trunc_power_basis(x = x,no_basis = 100,type = "quadratic")

B_linear<-B_linear_list$basis_mat
B_quad<-B_quad_list$basis_mat




# Kolla här: https://cran.r-project.org/web/packages/bigstep/vignettes/bigstep.html


#-------------------------------------------------------------------------------
# linjära baser:
#-------------------------------------------------------------------------------
library(bigstep)
prepare1 <- prepare_data(y, B_linear)

results1 <- stepwise(prepare1, crit = bic)
results1$model
summary(results1)

results1$model



# hur många basfunktioner?
length(results1$model)


B1_chosen<-B_linear[,colnames(B_linear)%in%results1$model]
sort(colnames(B1_chosen))
sort(results1$model)

df1<-data.frame(y=y,B1_chosen)

lm_obj1<-lm(y~.,data=df1)

plot(x,y)
lines(x,fitted(lm_obj1),col="red",lwd=4)
# ganska bra anpassning

lm_diagnostics(lm_obj = lm_obj1)
# ser bra ut

#-------------------------------------------------------------------------------
# kvadratiska baser:
#-------------------------------------------------------------------------------

prepare2 <- prepare_data(y, B_quad)

results2 <- stepwise(prepare2, crit = bic)
results2$model
summary(results2)

results2$model

# hur många basfunktioner?
length(results2$model)


B2_chosen<-B_quad[,colnames(B_quad)%in%results2$model]
sort(colnames(B2_chosen))
sort(results2$model)

df2<-data.frame(y=y,B2_chosen)

lm_obj2<-lm(y~.,data=df2)

plot(x,y)
lines(x,fitted(lm_obj2),col="blue",lwd=4)
# ser ut att vara en bra anpassning, bättre än med linjära baser

lm_diagnostics(lm_obj = lm_obj2)
# ser bra ut


#-------------------------------------------------------------------------------
# jämför baser:
#-------------------------------------------------------------------------------

plot(x,y)
lines(x,fitted(lm_obj1),col="red",lwd=3)
lines(x,fitted(lm_obj2),col="blue",lwd=3)

# modell 1: ger en mer kantig anpassning
# modell 2: ger en mer mjuk anpassning
