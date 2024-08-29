
#' @title trunc_power_basis
#'
#' @description 
#' Creates truncated power basis functions, which can sevre as basis functions in spline regression.
#' 
#' @param x a numeric vector with data
#' @param no_knots number of basis funciton
#' @param knots Postions for the truncation of the basis functions to end, 
#'  default is NULL, which means that the basis functions are equally spread in
#'  the range of x.
#' @param type which type of basis functions? Options: linear, quadratic
#' @param show_plot shall the basis functions be plotted?
#' 
#' @return 
#' A list with 
#'  - a matrix with basis functions as columns
#'  - x
#'
#' @export

trunc_power_basis<-function(x,no_basis,knots=NULL,type="linear",show_plot=FALSE){
  n_obs<-length(x)
  
  if(is.null(knots)){
    max_val<-max(x)
    min_val<-min(x)
    max_knot<-max_val*(1-1/no_basis)
    knot_pos<-seq(min_val,max_knot,length.out = no_basis)
  }else{
    if(no_basis!=length(knots)) stop("knots must have no_basis elements if not NULL")
    knot_pos<-knots
  }
  
  spline_basis<-matrix(0,n_obs,no_basis)
  
  
  for(i in 1:no_basis){
    if(type=="linear"){
      spline_basis[,i]<-pmax(0,(x)-knot_pos[i])
    }else if(type=="quadratic"){
      spline_basis[,i]<-(pmax(0,(x)-knot_pos[i]))^2
    } else{
      stop("Argument type is not correct!")
    }
  } 
  spline_basis<-spline_basis/max(spline_basis)
  colnames(spline_basis)<-paste0("basis",1:ncol(spline_basis))
  
  if(show_plot){
    spline_basis2<-spline_basis
    if(is.unsorted(x)){
      for(i in 1:no_basis){
        if(type=="linear"){
          spline_basis2[,i]<-pmax(0,sort(x)-knot_pos[i])
        }else if(type=="quadratic"){
          spline_basis2[,i]<-(pmax(0,sort(x)-knot_pos[i]))^2
        } else{
          stop("Argument type is not correct!")
        }
      } 
    }
    spline_basis2<-spline_basis2/max(spline_basis2)
    plot(sort(x),spline_basis2[,1],type="l",col=1,ylim=c(min(spline_basis2),max(spline_basis2)),ylab="",xlab="")
    for(i in 2:no_basis){
      lines(sort(x),spline_basis2[,i],col=i)
    }
  }
  return(list(basis_mat=spline_basis,x=x))
}