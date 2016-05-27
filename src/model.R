#############################################################################################
# Script used to solve the optimization problem, leading to the computation of the centrality.
#
# Alexandre Reiffers 12/2015
#############################################################################################

#############################################################################################
# This function computes the regularized opinion centrality. 
# 
# network: list of graphs constituting the multiplex network.
# alpha: intensity of node activation.
# budget: budget constraint over intensity.
# personal.opinion: vector that generate the probability that a user will do nothing its own opinion.
# returns: the opinion centrality measure.
#############################################################################################
process.opinion.centrality <- function(network, alpha, budget, personal.opinion)
{	number.layers<-length(network)
	number.nodes=vcount(network[[1]])

	# contruction of the constraint matrix E
	E <- alpha[,1]*as.matrix(get.adjacency(network[[1]])/(degree(network[[1]])+c(personal.opinion[1,])))
	for(j in 2:number.layers)
	{	E <- alpha[,j]*as.matrix(get.adjacency(network[[j]])/(degree(network[[j]])+c(personal.opinion[j,])))+E
	}
	E[E=="NaN"] <- 0

	# value of Lambda
	Lambda <- sum(alpha)+budget
	
	# computation of the matrix A
	A <- solve( Lambda*diag(number.nodes)-E)
	#gamma <- (number.nodes^2*(max(A)-min(A)))/(budget*Lambda)+1
	gamma <- 1

	# computation of the regularized opinion centrality by using proposition 5
	y <- Sol(A, number.nodes, budget, gamma, Lambda, logscale=FALSE)
	
	# return the y values
	return(y)
}
