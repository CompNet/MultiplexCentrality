#############################################################################################
# Main script, performing the following steps:
#	1) Load the networks.
#	2) Load the centrality measures previously processed (with MuxViz).
#	3) Process our own multiplex measure.
#	4) Compare them and produce various plots.
#
# Alexandre Reiffers 12/2015
# Vincent Labatut 12/2015
#############################################################################################
setwd("D:/Eclipse/workspaces/Networks/MultiplexCentrality")
#setwd("/Users/jeanlouis/Desktop/MultiplexCentrality-master 6")
#source("src/main.R")
source('src/gradient.R')
source('src/model.R')
source('src/plots.R')
source('src/misc.R')



# init data-related variables
data.pars <- list()
#data.pars[["EUAir"]] <- list(
#	data.folder="data/EUAir_Multiplex_Transport/",
#	rdata.filename="EUAir.Rdata",
#	centrality.filename="EUAir_centrality_table.csv")
data.pars[["Kapferer1"]] <- list(
	data.folder="data/kaptail1-GraphML/",
	rdata.filename="kaptail1.Rdata",
	centrality.filename="Kapferer1_centrality_table.csv")
data.pars[["Kapferer2"]] <- list(
	data.folder="data/kaptail2-GraphML/",
	rdata.filename="kaptail2.Rdata",
	centrality.filename="Kapferer2_centrality_table.csv")
data.pars[["Knoke"]] <- list(
	data.folder="data/knokbur-GraphML/",
	rdata.filename="knokbur.Rdata",
	centrality.filename="Knoke_centrality_table.csv")
#data.pars[["london"]] <- list(
#	data.folder="data/London_Multiplex_Transport/",
#	rdata.filename="london.Rdata",
#	centrality.filename="london_centrality_table.csv")
data.pars[["Padgett"]] <- list(
	data.folder="data/padgett-GraphML/",
	rdata.filename="padgett.Rdata",
	centrality.filename="Padgett_centrality_table.csv")
data.pars[["Roethlisberger"]] <- list(
	data.folder="data/wiring-GraphML/",
	rdata.filename="wiring.Rdata",
	centrality.filename="Roethlisberger_centrality_table.csv")
data.pars[["Sampson"]] <- list(
	data.folder="data/sampson-GraphML/",
	rdata.filename="sampson.Rdata",
	centrality.filename="Sampson_centrality_table.csv")
data.pars[["Thurmann"]] <- list(
	data.folder="data/thuroff-GraphML/",
	rdata.filename="thuroff.Rdata",
	centrality.filename="Thurmann_centrality_table.csv")
data.pars[["Wolfe"]] <- list(
	data.folder="data/wolfe-GraphML/",
	rdata.filename="wolfe.Rdata",
	centrality.filename="Wolfe_centrality_table.csv")

# select the previously processed centrality measures
measures <- c(
	"Degree",
#	"DegreeIn",
#	"DegreeOut",
	"PageRank",
	"Eigenvector",
	"Hub",
	"Authority",
	"Katz"
)

# setup alpha
alpha.vals <- seq(from=0,to=40,by=0.25)			# distinct values of alpha
alpha.vals <- alpha.vals[alpha.vals!=0]
#alpha.vals <- c(alpha.vals,seq(from=30,to=100,by=10))
l <- length(alpha.vals)						# number of distinct values of alpha
#round(c(1:l)/(l-3),2)

# plot folder
plot.folder <- "plots"		# name of the folder containing the plots
scale <- 20					# node scale for graph plots
formats <- c(				# file format of the plots
	"PDF",
	"PNG"
)	 

# process each multiplex network
network.names <- names(data.pars)
for(multiplex.index in 1:length(data.pars))
{	network.name <- network.names[multiplex.index]
	cat("Processing network ",multiplex.index," (",network.name,")\n",sep="")
	data.par <- data.pars[[network.name]] 
	
	# load network
	net.file <- paste(data.par$data.folder,data.par$rdata.filename,sep="")
	multiplex.network <- retrieve.rdata.object(net.file)
	number.layers <- length(multiplex.network)
	number.nodes <- vcount(multiplex.network[[1]])
	cat("  Number of layers: ",number.layers," - Nodes by layer: ",number.nodes,"\n",sep="")
	
	# process aggregated network (for later plots)
	adj <- matrix(0,nrow=number.nodes, ncol=number.nodes)
	for(j in 1:length(multiplex.network))
	{	
		#tmp <- alpha[,j] * as.matrix(get.adjacency(multiplex.network[[j]], type="both"))
		#tmp[tmp=="NaN"] <- 0
		#adj <- adj + tmp
		adj <- adj + as.matrix(get.adjacency(multiplex.network[[j]], type="both"))
	}
	#print(adj)
	if(all(adj==t(adj)))
	  aggregated.network <- graph.adjacency(adjmatrix=adj,weighted=TRUE,mode="directed")
	else
	  aggregated.network <- graph.adjacency(adjmatrix=adj,weighted=TRUE,mode="undirected")
	
	# layout the aggregated plot
	V(aggregated.network)$size <- scale
	lay <- layout.fruchterman.reingold(graph=aggregated.network)
	
	# load previously processed centralities
	centr.file <- paste(data.par$data.folder,data.par$centrality.filename,sep="")
	mutiplex.centralities <- read.csv(file=centr.file,sep=";")
	
	# init centrality tables
	opinion.centralities <- array(0,c(l,number.nodes))
	other.centralities <- as.matrix(mutiplex.centralities)[(number.layers*number.nodes+1):((number.layers+1)*number.nodes),measures]
	class(other.centralities) <- "numeric"

	# init plot folder
	net.plot.folder <- paste(plot.folder,"/",network.name,"/",sep="")
	dir.create(net.plot.folder,showWarnings=FALSE,recursive=TRUE)
  	
	# init correlation table
	correlation.values <- matrix(NA,nrow=l,ncol=length(measures))
	colnames(correlation.values) <- measures
  
	# process our centrality measure
	cat("  Processing the opinion centrality\n",sep="")
	for(i in 1:l)
	{	cat("    for alpha=",alpha.vals[i]," (",i,"/",l,")",sep="")
#		alpha <- cbind(array(alpha.vals[i],c(number.nodes,floor(number.layers/2))),array((alpha.vals[i])^2,c(number.nodes,(floor(number.layers/2)+1))))
		alpha <- matrix(alpha.vals[i],nrow=number.nodes, ncol=number.layers)
#		print(alpha)

		####### process opinion centrality measure
		centrality <- process.opinion.centrality(network=multiplex.network, alpha, budget=1,  grad.horizon=1000)
		
		opinion.centralities[i,] <- t(centrality)
		stdev <- sd(opinion.centralities[i,])
		if(stdev==0)
			cat("....WARNING: stdev=",stdev," (value=",opinion.centralities[i,1],")\n",sep="")
		else
			cat("....stdev=",stdev,"\n",sep="")
	}
	
	# record our measure as a table
	out.file <- paste(net.plot.folder,"/opinion-centrality.csv",sep="")
	col.node <- 1:number.nodes
	centr <- cbind(col.node, t(opinion.centralities))
	colnames(centr) <- c("Node", paste("alpha=",alpha.vals,sep=""))
	write.csv2(centr, file=out.file, row.names=FALSE)

	# compare each measure to our own
	cat("  Compare the opinion measure to the others\n")
	# we consider all possible values of alpha
	for(i in 1:l)
	{	cat("    Processing alpha=",alpha.vals[i],"\n",sep="")
		
		# produce the opinion centrality histogram for the considered value of alpha
		cat("      Generate histogram for the opinion centrality with alpha=",alpha.vals[i],"\n",sep="")
		measure.histo(vals=opinion.centralities[i,], alpha=alpha.vals[i], folder=net.plot.folder, formats=formats)
		
		# process each alternate measure individually
		for(measure in measures)
		{	# check if the considered measure was processed
			if(!any(is.na(other.centralities[,measure])))
			{	# process the rank correlation with our measure
				correlation.values[i,measure] <- cor(opinion.centralities[i,], other.centralities[,measure], method="spearman")
	
				# plot ranking differences
				cat("      Generate line plot representing ranking differences with measure ",measure,"\n",sep="")
#				rank.diff.lineplot(ref.vals=other.centralities[,measure], comp.vals=opinion.centralities[i,], ref.measure=measure, alpha=alpha.vals[i], folder=net.plot.folder, formats=formats)
			
				# ranking differences as a barplot
				cat("      Generate barplot representing ranking differences with measure ",measure,"\n",sep="")
				rank.diff.barplot(ref.vals=other.centralities[,measure], comp.vals=opinion.centralities[i,], ref.measure=measure, alpha=alpha.vals[i], folder=net.plot.folder, formats=formats)
				
				# plot the network with each existing measure as the size, and the opinion measure as the color
				cat("      Generate a plot representing the graph and measure ",measure,"\n",sep="")
#				graph.plot(g=aggregated.network, ref.vals=other.centralities[,measure], comp.vals=opinion.centralities[i,], ref.measure=measure, alpha=alpha.vals[i], folder=net.plot.folder, layout=lay, scale=scale, formats=formats)
			}
		}
	}
	
	# generate plots comparing existing centrality measures
#	cat("  Generate plots comparing existing centrality measures\n")
#	for(m1 in 1:(length(measures)-1))
#	{	for(m2 in (m1+1):length(measures))
#			rank.diff.barplot(ref.vals=other.centralities[,measures[m1]], comp.vals=other.centralities[,measures[m2]], ref.measure=measures[m1], comp.measure=measures[m2], alpha=NA, folder=net.plot.folder, formats=formats)
#	}
	
	# plot the correlation between our measure and the other ones
	cat("  Plot the correlations between the opinion measure and the other measures\n")
	for(measure in measures)
	{	# check if the considered measure was processed
		if(any(is.na(other.centralities[,measure])) | length(unique(other.centralities[,measure]))==1)
			cat("    WARNING: could not find the values for measure ",measure,", so no correlation plot\n",sep="")
		
		# check if our own centrality could be processed 
		else if(all(is.na(correlation.values[,measure])))
			cat("    WARNING: Interest centrality could not be processed, so no correlation plot for ",measure,"\n",sep="")
		else
		{	cat("    With measure ",measure,"\n",sep="")
#			corr.plot(cor.vals=correlation.values[,measure], alpha.vals, measure, folder=net.plot.folder, formats=formats)
		}
	}
	
	# draw all measure correlations in the same plot
	corr.plot.all(cor.vals=correlation.values, alpha.vals, measures, folder=net.plot.folder, formats=formats)
	
	# process and record correlation matrix for opinion measure only
	cat("  Record the correlations for the opinion measure only (in function of alpha)\n")
	opinion.correlation <- matrix(NA,nrow=l,ncol=l)
	colnames(opinion.correlation) <- alpha.vals
	rownames(opinion.correlation) <- alpha.vals
	for(i in 1:l)
	{	for(j in 1:l)
			opinion.correlation[i,j] <- cor(opinion.centralities[i,], opinion.centralities[j,], method="spearman")
	}
	correlation.plot(corr.mat=opinion.correlation, folder=net.plot.folder, formats=formats)
}
