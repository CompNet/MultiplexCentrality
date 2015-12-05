#############################################################################################
# Functions used to solve the optimiztion problem.
#
# Alexandre Reiffers 12/2015
#############################################################################################
library(magic) # Block matrices
library(igraph)
library(ggplot2)



#############################################################################################
# This function computes the gradient of the following expression: 
#	-1^tx-lambda1\sum_i[log((b-Ax)_i)]+lambda2\sum_i[(cx-d)_i]^2-\lambda3[log(1-1^tx)]
#
# A: TODO
# b: TODO
# number.layers: number of layers in the network.
# number.nodes: number of nodes in each layer.
# u: TODO
# d: TODO
# x: TODO
# lambda1: TODO
# lambda2: TODO
# lambda3: TODO
# npar: TODO
# print: TODO
# returns: the gradient in x of the above expression.
#############################################################################################
df <- function(A, b, number.layers, number.nodes, u, d, x, lambda1, lambda2, lambda3, npar=TRUE, print=TRUE){
	# gradient initialization
	y <- 0
	
	# dimension of the matrix
	l<-number.layers*number.nodes
	B1 <- b - A %*% x
	B2 <- A
	
	# gradient computation of lambda1\sum_i[log((b-Ax)_i)]
	for(i in 1:l)
		y <- y + lambda1*(1/B1[i])*A[1:l,i]
	
	# gradient computation of \sum_i[(cx-d)_i]^2
	v <- c(u*c(u%*%x-d))
	
	# gradient of the whole function
	df <- array(-1,c(l)) + y + lambda2*(v) + lambda3*array(1/(1-sum(x)),c(l))
	return(df)
}



#############################################################################################
# This function performs the gradient scheme over the expression:
#	1^tx-lambda1\sum_i[log((b-Ax)_i)]+lambda2\sum_i[(cx-d)_i]^2-\lambda3[log(1-1^tx)]
#
# A: TODO
# b: TODO
# number.nodes: number of nodes in each layer.
# number.layers: number of layers in the network.
# u: TODO
# d: TODO
# x: initial value of  the gradient scheme
# lambda1: TODO
# lambda2: TODO
# lambda3: TODO
# grad.horizon: number of iteration for the gradient descent.
# npar: TODO
# print: TODO
# returns: the minimum of 1^tx-lambda1\sum_i[log((b-Ax)_i)]+lambda2\sum_i[(cx-d)_i]^2-\lambda3[log(1-1^tx)].
#############################################################################################
Sol <- function(A, b, number.layers, number.nodes, u, d, x, lambda1, lambda2, lambda3, grad.horizon, npar=TRUE, print=TRUE)
{	l <- number.nodes*number.layers

	#Initialization of the gradient scheme
	Result <- array(runif((number.layers*number.nodes)^2, 0, 1),c(l,grad.horizon))
	Result[1:l,1] <- x
	
	# update of the gradient scheme
	for(k in 1:(grad.horizon-1))
	{	Result[1:l,k+1] <- Result[1:l,k]-(1/k)*(df(A,b,number.layers,number.nodes,u,d,Result[1:l,k],lambda1,lambda2,lambda3))
		Result[1:l,k+1] <- unlist(lapply(lapply( Result[1:l,k+1],function(x) max(x,0.0001)), function (x) min(x,1)))
	}
	
	return(Result)
}



#############################################################################################
# This function computes the constraint matrix.
#
# E: adjacency matrix of the network.
# R: budget constraint.
# gamma: activation vector.
# returns: constraint matrix A.
#############################################################################################
Matrix <- function(E, gamma, R)
{	l <- dim(E)[1]
	0.5*E - diag(array(R*sum(gamma),c(l)))
}
