library(ISLR2)
attach(Wage)
library(splines)
library(cv)
library(ggplot2)
library(fANCOVA)

#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
# CV för naturliga splines
#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------

# skattar en mängd modeller med naturliga splines med olika frihetsgrader
# sparar alla modeller i en lista
df_vect<-seq(3,by=1,length=15)
model_list<-vector("list",length = length(df_vect))
for(i in 1:length(df_vect)){
  model_list[[i]]<-lm(wage ~ ns(age, df = df_vect[i]), data = Wage)
}
names(model_list)<-paste0("model_df_",df_vect)

# cv package:
# https://cran.r-project.org/web/packages/cv/index.html
# https://cran.r-project.org/web/packages/cv/vignettes/cv.html
?cv

# testa att ändra seed till några olika värden: tex 7 och 78
# leave-one-out cross-validation:
cv_wage_ns_n <- cv(models(model_list),data = Wage, seed = 7,k = "n")
# 10 fold cross-validation
cv_wage_ns_10 <- cv(models(model_list),data = Wage, seed = 7,k = 10)

cv_mse1<-as.data.frame(cv_wage_ns_n,rows="cv",columns="criteria")$criterion
cv_mse2<-as.data.frame(cv_wage_ns_10,rows="cv",columns="criteria")$criterion


df<-data.frame(df=c(df_vect,df_vect),cv_mse=c(cv_mse1,cv_mse2),folds=c(rep("n",length(df_vect)),rep("10",length(df_vect))))
ggplot(data = df,mapping = aes(x = df,y = cv_mse,color=folds))+geom_line()+geom_point() +theme_bw()

# lägst df:
df_vect[which.min(cv_mse1)]
df_vect[which.min(cv_mse2)]

fit_ns<-predict(model_list[[which.min(cv_mse2)]],newdata = data.frame(age=sort(Wage$age)))
plot(x = Wage$age, y = Wage$wage)
lines(x = sort(Wage$age),fit_ns,t="l",col="blue",lwd=3)


#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
# CV för loess
#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------

# se: https://www.r-bloggers.com/2016/02/automated-parameter-selection-for-loess-regression/

wage_loess_cv <- loess.as(x = Wage$age, y = Wage$wage, degree = 1, criterion = c("gcv"), user.span = NULL, plot = F)
fit_loess <- predict(wage_loess_cv, newdata =  data.frame(x=sort(Wage$age)))
plot(x = Wage$age, y = Wage$wage)
lines(x = sort(Wage$age),fit_loess,col="red",lwd=3)

plot(sort(Wage$age),fit_loess)

# båda modellerna:
plot(x = Wage$age, y = Wage$wage)
lines(x = sort(Wage$age),fit_ns,t="l",col="blue",lwd=3,lty="dashed")
lines(x = sort(Wage$age),fit_loess,col="red",lwd=3,lty="dashed")


#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
# CV för gam
#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------

library(mgcv)

gam_cv <- mgcv::gam(wage ~  s(year,k=7,bs="cr") + s(age,k=7,bs="cr") + education,data = Wage)

plot(gam_cv,pages=1,residuals=TRUE)  ## show partial residuals
plot(gam_cv,pages=1,seWithMean=TRUE) ## `with intercept' CIs
gam_cv
summary(gam_cv)


# för mer exempel:
?mgcv::gam
example(gam)

