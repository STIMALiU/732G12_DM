

#' @title cosine_basis
#'
#' @description 
#' Creates cosine basis functions, which can sevre as basis functions in spline regression.
#' 
#' @param x a numeric vector with data
#' @param order maximal order of the cosine basis functions
#' @param x_min min of x
#' @param x_max max of x
#' 
#' @return 
#' A list with 
#'  - a matrix with basis functions as columns
#'  - x
#'
#' @export


cosine_basis<-function(x,order,x_min=min(x),x_max=max(x)){
  n<-length(x)
  B<-matrix(0,n,order)
  L<-x_max-x_min
  for(k in 1:order){
    #B[,k]<-sqrt(2/L)*cos(pi*k*( (x-x_min)/L) )
    B[,k]<-cos(pi*k*( (x-x_min)/L) )
  }
  colnames(B)<-paste0("basis",1:order)
  return(list(basis_mat=B,x=x))
}