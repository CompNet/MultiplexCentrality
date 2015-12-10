#############################################################################################
# Main script, performing the following steps:
#	1) Load the networks.
#	2) Load the centrality measures previously processed with MuxViz.
#	3) Process our own multiplex measure.
#	4) Compare them and produce various plots.
#
# Alexandre Reiffers 12/2015
# Vincent Labatut 12/2015
#############################################################################################
# setwd("D:/Eclipse/workspaces/Networks/MultiplexCentrality")
# setwd("/Users/jeanlouis/Downloads/MultiplexCentrality-master")
# source("src/main.R")
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
data.pars[["london"]] <- list(
	data.folder="data/London_Multiplex_Transport/",
	rdata.filename="london.Rdata",
	centrality.filename="london_centrality_table.csv")
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

# select the centrality measures previously processed by MuxViz
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

# setup p
l <- 5								# number of distinct values of p
p.vals = round(c(1:l)/(l+1),2)		# distinct values of p

# plot folder
plot.folder <- "plots"

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
	
	# load previously processed centralities
	centr.file <- paste(data.par$data.folder,data.par$centrality.filename,sep="")
	mutiplex.centralities <- read.csv(file=centr.file,sep=";")
	
	# init centrality tables
	interest.centralities <- array(0,c(l,number.nodes))
	other.centralities <- as.matrix(mutiplex.centralities)[(number.layers*number.nodes+1):((number.layers+1)*number.nodes),measures]
	class(other.centralities) <- "numeric"

	# init plot folder
	net.plot.folder <- paste(plot.folder,"/",network.name,"/",sep="")
	dir.create(net.plot.folder,showWarnings=FALSE,recursive=TRUE)
  	
	# init correlation table
	correlation.values <- matrix(NA,nrow=l,ncol=length(measures))
	colnames(correlation.values) <- measures
  
	# process our centrality measure
	cat("  Processing interest centrality\n",sep="")
	for(i in 1:l)
	{	cat("    for p=",p.vals[i]," (",i,"/",l,")",sep="")
		alpha <- array(0.9,c(number.layers*number.nodes,1))
	
		####### process A in function of p
		parameter.topics=p.vals[i]
		#A <- array((1-p.vals[i])/2*(number.layers-1),c(number.layers,number.layers))-diag(array((1-p.vals[i])/2*(number.layers-1),c(number.layers)))+diag(array(p.vals[i],c(number.layers)))
		A <- array((1-p.vals[i])/(number.layers-1),c(number.layers,number.layers))-diag(array((1-p.vals[i])/(number.layers-1),c(number.layers)))+diag(array(p.vals[i],c(number.layers)))
		b <- array(1/(number.layers),c(number.layers,1))

		####### process interest centrality measure
#		centrality <- process.interest.centrality(A, network=multiplex.network, alpha, budget=1, b, grad.horizon=1000)
# using dummy values, just for testing
centrality <- runif(n=number.nodes,min=0,max=1)
		
		interest.centralities[i,] <- t(centrality)
		stdev <- sd(interest.centralities[i,])
		if(stdev==0)
			cat("....WARNING: stdev=",stdev," (value=",interest.centralities[i,1],")\n",sep="")
		else
			cat("....stdev=",stdev,"\n",sep="")
	}
	
	# record our measure as a table
	out.file <- paste(net.plot.folder,"/interest-centrality.csv",sep="")
	col.node <- 1:number.nodes
	centr <- cbind(col.node, t(interest.centralities))
	colnames(centr) <- c("Node", paste("p=",p.vals,sep=""))
	write.csv2(centr, file=out.file)

	# compare each measure to our own
	# we consider all possible values of p
	for(i in 1:l)
	{	# produce the interest centrality histogram for the considered value of p
		measure.histo(vals=interest.centralities[i,], p=p.vals[i], folder=net.plot.folder)
		
		# process each MuxViz measure individually
		for(measure in measures)
		{	# check if MuxViz could process the considered measure
			if(!any(is.na(other.centralities[,measure])))
			{	# process the rank correlation with our measure
				correlation.values[i,measure] <- cor(interest.centralities[i,], other.centralities[,measure], method="spearman")
	
				# plot ranking differences
				rank.diff.lineplot(ref.vals=other.centralities[,measure], comp.vals=interest.centralities[i,], ref.measure=measure, p=p.vals[i], folder=net.plot.folder)
			
				# ranking differences as a barplot
				rank.diff.barplot(ref.vals=other.centralities[,measure], comp.vals=interest.centralities[i,], ref.measure=measure, p=p.vals[i], folder=net.plot.folder)
			}
		}
	}
	# plot the correlation between our measure and the other ones
	cat("  Plot correlations\n")
	for(measure in measures)
	{	# check if MuxViz could process the considered measure
		if(any(is.na(other.centralities[,measure])) | length(unique(other.centralities[,measure]))==1)
			cat("    WARNING: MuxViz could not process measure ",measure,", so no correlation plot\n",sep="")
		
		# check if our own centrality could be processed 
		else if(all(is.na(correlation.values[,measure])))
			cat("    WARNING: Interest centrality could not be processed, so no correlation plot for ",measure,"\n",sep="")
		else
		{	cat("    With measure ",measure,"\n")
			corr.plot(cor.vals=correlation.values[,measure], p.vals, measure, folder=net.plot.folder)
		}
	}
}
