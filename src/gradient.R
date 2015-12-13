#############################################################################################
# Functions used to solve the opinion maximization optimization problem.
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
# A: inequality matrix, size (number.nodes*number.layers)^2.
# b: inequality vector, size (number.nodes*number.layers).
# number.nodes: number of nodes in each layer.
# number.layers: number of layers in the network.
# u: equality vector 1, size (number.nodes*number.layers).
# d: equality vector 2, size (number.nodes*number.layers).
# x: initial value of  the gradient scheme.
# lambda1: first coefficient for the relaxation, dimension 1, positive.
# lambda2: first coefficient for the relaxation, dimension 2, positive.
# lambda3: first coefficient for the relaxation, dimension 3, positive.
# grad.horizon: number of iteration for the gradient descent.
# print: print solution.
# returns: the minimum of 1^tx-lambda1\sum_i[log((-Ax)_i)]+lambda2\sum_i[(cx-d)_i]^2-\lambda3[log(1-1^tx)].
#############################################################################################
df <- function(A, number.layers, number.nodes, u, d, x, lambda1, lambda2, lambda3)#, npar=TRUE, print=TRUE)
{	# gradient initialization
	y <- 0

	# dimension of the matrix
	l<-number.nodes
	B1 <- - A %*% x
	B2 <- A

	# gradient computation of lambda1\sum_i[log((b-Ax)_i)]
	for(i in 1:l)
		y <- y + lambda1*((1/B1[i])*A[1:l,i])
  
	# gradient computation of \sum_i[(cx-d)_i]^2
	v <- c(u*c(u%*%x-d))
  
	# gradient of the whole function
	df <- array(-1,c(l)) + y + lambda2*(v) + lambda3*array(1/(1-sum(x)),c(l))
	return(df)
}



#############################################################################################
# This function performs the gradient scheme over the expression:
#	1^tx-lambda1\sum_i[log((b-Ax)_i)]+lambda2\sum_i[(ux-d)_i]^2-\lambda3[log(1-1^tx)]
#
# A: inequality matrix, size (number.nodes*number.layers)^2.
# b: inequality vector, size (number.nodes*number.layers).
# number.nodes: number of nodes in each layer.
# number.layers: number of layers in the network.
# u: equality vector 1, size (number.nodes*number.layers).
# d: equality vector 2, size (number.nodes*number.layers).
# x: initial value of  the gradient scheme.
# lambda1: first coefficient for the relaxation, dimension 1, positive.
# lambda2: first coefficient for the relaxation, dimension 2, positive.
# lambda3: first coefficient for the relaxation, dimension 3, positive.
# grad.horizon: number of iteration for the gradient descent.
# returns: the minimum of 1^tx-lambda1\sum_i[log((b-Ax)_i)]+lambda2\sum_i[(cx-d)_i]^2-\lambda3[log(1-1^tx)].
#############################################################################################
Sol <- function(B, number.layers, number.nodes, u, d, x, lambda1, lambda2, lambda3, grad.horizon)#, npar=TRUE, print=TRUE)
{	l <- number.nodes
#	B <- solve(diag(number.nodes)-A)
	Result <- array(0,c(l,grad.horizon))
#	Result[,1] <- (1/lambda1)*(apply(B,1,sum)+(lambda1-sum(apply(B,1,sum)))/number.nodes)
	Result[,1] <- (1/lambda1)*(apply(B,1,sum)+(lambda1*d-sum(apply(B,1,sum)))/number.nodes)
	
	#Result[,1]<-(1/(max(apply(B,1,sum))))*apply(B,1,sum)
	
	# update of the gradient scheme
	for(k in 1:(grad.horizon-1))
	{	Result[,k+1] <- Result[,1]
		#Result[,k+1] <- Result[,k]-(1/k)*(max(-B%*%Result[,k]+lambda1*array((sum(Result[,k])-d),c(l,1)))) #nesterov 
		#Result[1:l,k+1] <- unlist(lapply(lapply( Result[1:l,k+1],function(x) max(x,-10000000)), function (x) min(x,10000000)))
	}
	return(Result)
}
