#############################################################################################
# This function compute the interest centrality. 
# 
# A: influence matrix.
# network: list of graphs constituting the multiplex network.
# alpha: intensity of node activation.
# budget: budget constraint over intensity.
# b: external world society interest.
# grad.horizon: number of iteration for the gradient descent.
# returns: the interest centrality measure.
#############################################################################################
process.interest.centrality <- function( network, alpha, budget, grad.horizon=1000)
{	number.layers<-length(network)
	number.nodes=vcount(network[[1]])
	
	####### fixed parameters in the computation of the gradient
	#threshold=(number.layers*number.layers)>100
	lambda1=1/(number.nodes-budget)
	  #0.5/(number.layers*number.nodes)^4
	  #0.5*exp(-(number.layers*number.nodes));
	#0.0000000005
	#
	lambda2=0.1/(number.nodes)
	  #0.1/(number.layers*number.nodes)^4
	  #0.1*exp(-(number.layers*number.nodes));
	#0.0000000001
	#
	lambda3=0.5/(number.nodes)
	  #0.5/(number.layers*number.nodes)^4
	  #0.5*exp(-(number.layers*number.nodes));
	    #0.5*exp((number.layers*number.nodes));
	
	######## constraint matrix
	E=alpha[,1]*as.matrix(get.adjacency(network[[1]])/degree(network[[1]]))
	for(j in 2:number.layers)
		E=alpha[,j]*as.matrix(get.adjacency(network[[j]])/degree(network[[j]]))+E
	E[E=="NaN"]=0
	#### processing of the external world society interest
	#b.layer<-array(0,c(number.nodes*number.layers,1)) #modif
#	for(k in 1:number.layers)
#		b.layer[((k-1)*number.nodes+1):(k*number.nodes)]=array(b.inv[k],c(number.nodes,1))
	
	# uniform initialization of the states
	x0 <- array(1/(number.nodes),c(number.nodes))

	# processing the constraints
	constraints.matrix <- E
	  #Matrix(E,alpha,budget)
	c1 <- array(1,c(1,number.nodes))%*%constraints.matrix
		
	# applying gradient descent
	y <- Sol.v2(constraints.matrix,number.layers,number.nodes,c1,budget,x0,lambda1,lambda2,lambda3,grad.horizon)
	
	# return the last y values
	result <- y[, grad.horizon]
	return(result)
}
