#############################################################################################
# Functions used to solve the opinion maximization optimization problem.
#
# Alexandre Reiffers 12/2015
#############################################################################################
library(magic) # Block matrices
library(igraph)
library(ggplot2)



#############################################################################################
# This function performs the computation of the regularized opinion centrality.
#
# B: inequality matrix, size (number.nodes*number.layers)^2.
# number.layers: number of layers in the network.
# number.nodes: number of nodes in each layer.
# budget: value of the budget
# x: initial value of  the gradient scheme.
# Lambda: total intensity of event
# gamma : regularization coeffficient
# returns: the regularized opinion centrality
#############################################################################################
Sol <- function(B, number.nodes, budget, gamma, Lambda,logscale)#, npar=TRUE, print=TRUE)
{	l <- number.nodes
	Result <- array(0,c(l))
	
	# computation of the regularized opinion centrality by using the formula from proposition 5.
	Result<- (budget/number.nodes)+sum(B)/(gamma*number.nodes*Lambda)-(apply(B,2,sum))/(gamma*Lambda)
	
	# log scale
	if(logscale) 
		Result <- log(Result)
	
	return(Result)
}
