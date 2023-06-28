


# sanna beslutsgränsen
f<-function(x,L,k,x0){
  L/(1+exp(-k*(x-x0)))
}

# kollar vilken klass obs tillhör baserat på beslutsgränsen
f_cond<-function(x2,x1,L,k,x0){
  y<-(x2*(L+exp(-k*(x1-x0))))>L
  return(as.numeric(y))
}

x_line<-seq(0,1,length=1000)
y_line<-f(x = x_line,L = 1,k = 1,x0 = 0.5)
# sanna beslutsgränsen
plot(x_line,y_line,t="l")

#---------------------------------------------------------------------------
# generera data
nobs<-500
my_sd<-0.5
set.seed(343)
x1_0<-runif(n = nobs)
x2_0<-runif(n = nobs,min = -0.5,max = 1.5)
#---------------------------------------------------------------------------
# lägger till brus till data
set.seed(2341)
sd_val<-0.15 # kontrollerar bruset
x1<-x1_0+rnorm(n = nobs,sd = sd_val)
x2<-x2_0+rnorm(n = nobs,sd = sd_val)
#---------------------------------------------------------------------------
plot(x1,x2)
b<-1
a<-1
y_lab<-f_cond(x2 = x1_0,x1 = x2_0,L = 1,k = 1,x0 = 0.5)
plot(x1,x2,col=y_lab+1,pch=16)
lines(x_line,y_line)

