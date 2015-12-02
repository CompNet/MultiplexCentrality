library(magic) # Block matrices
library(igraph)
library(ggplot2)
# Implementation of the gradient for solving min 1^tx subject to Ax<=b, cx-d=0 and 1^tx<1.  
# Computation of the gradient
# The matrix A is a square matrix of dimension I*C


#############################################################################################
# <description>
#
# A: description of the first parameter...
# ...
# returns: description of the value returned by the function.
#############################################################################################
df<-function(A,b,C,I,u,d,x,g1,g2,g3,npar=TRUE,print=TRUE){
  y<-0; # gradient initialization
  l<-C*I; # dimension of the matrix
  B1<-b-A%*%x; 
  B2<-A; 
  for(i in 1:l)
  {
    y<-y+g1*(1/B1[i])*A[1:l,i];
  }     
  v<-c(u*c(u%*%x-d))
 # df<-array(-1,c(l))+y+g2*(v)+g3*array(1/(1-sum(x)),c(l));
  df<-array(-1,c(l))+y+g2*(v)+g3*array(1/(1-sum(x)),c(l));
  return(df)
}

# gradient scheme
#############################################################################################
# <description>
#
# A: description of the first parameter...
# ...
# returns: description of the value returned by the function.
#############################################################################################
Sol<-function(A,b,C,I,u,d,x,g1,g2,g3,time,a,npar=TRUE,print=TRUE){
  l<-I*C
  Result<-array(runif((C*I)^2, 0, 1),c(l,time))
  Result[1:l,1]<-x
  # update
  for(k in 1:(time-1)){
    Result[1:l,k+1]<-Result[1:l,k]-(1/k)*(df(A,b,C,I,u,d,Result[1:l,k],g1,g2,g3))
    Result[1:l,k+1]<-unlist(lapply(lapply( Result[1:l,k+1],function(x) max(x,0.0001)), function (x) min(x,1)))
  } 
  return(Result)
}

#Vector1<-function(A,b){
#  Vector<--0.5*(solve(A,-b))
#  return(Vector)
#}

#############################################################################################
# <description>
#
# A: description of the first parameter...
# ...
# returns: description of the value returned by the function.
#############################################################################################
Matrix<-function(E,gamma,R){
l<-dim(E)[1]
  #0.5*E-diag(array(R*sum(gamma),c(l)))
  0.5*0.1*E-diag(array(R*sum(gamma),c(l)))
}

