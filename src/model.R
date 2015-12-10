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
process.interest.centrality <- function(A, network, alpha, budget, b, grad.horizon=1000)
{	number.layers<-length(network)
	number.nodes=vcount(network[[1]])
	
	####### computation of  (A-Id)^-1
	A <- A - diag(array(1,c(number.layers)))  
	A <- solve(A)
	
	####### fixed parameters in the computation of the gradient
	threshold=(number.layers*number.layers)>100
	lambda1=0.5/(number.layers*number.nodes)^4
	  #0.5*exp(-(number.layers*number.nodes));
	#0.0000000005
	#
	lambda2=0.1/(number.layers*number.nodes)^4
	  #0.1*exp(-(number.layers*number.nodes));
	#0.0000000001
	#
	lambda3=0.5/(number.layers*number.nodes)^4
	  #0.5*exp(-(number.layers*number.nodes));
	    #0.5*exp((number.layers*number.nodes));
	
	####### definition matrix topic
	B=array(0,c(number.nodes*number.layers,number.nodes*number.layers))
	for(k in 1:number.layers)
	{	for(k1 in 1:number.layers)
		{	B[((k-1)*number.nodes+1):(k*number.nodes),((k1-1)*number.nodes+1):(k1*number.nodes)]=array(A[k,k1],c(number.nodes,number.nodes))
		}
	}
	
	######## constraint matrix
	E=as.matrix(get.adjacency(network[[1]])/degree(network[[1]]))
	for(j in 2:number.layers)
		E=adiag(E,as.matrix(get.adjacency(network[[j]])/degree(network[[j]])))
	E[E=="NaN"]=0
	E=E-0.5*B 

	#### processing of the external world society interest
	b.inv <- solve(A,-b) #modif
	b.layer<-array(0,c(number.nodes*number.layers,1)) #modif
	for(k in 1:number.layers)
		b.layer[((k-1)*number.nodes+1):(k*number.nodes)]=array(b.inv[k],c(number.nodes,1))
	
	# uniform initialization of the states
	x0 <- array(1/(number.layers*number.nodes),c(number.layers*number.nodes))

	# processing the constraints
	constraints.matrix <- Matrix(E,alpha,budget)
	c1 <- array(1,c((number.layers*number.nodes)))%*%constraints.matrix
		
	# applying gradient descent
	y <- Sol(constraints.matrix,b.layer,number.layers,number.nodes,c1,budget,x0,lambda1,lambda2,lambda3,grad.horizon)
	
	# return the last y values
	result <- y[, grad.horizon]
	return(result)
}
