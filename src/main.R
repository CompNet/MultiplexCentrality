#############################################################################################
# Main script, performing the following steps:
# 	1) Load the networks.
#	2) Load the centrality measures previously processed with MuxViz.
#	3) Process our own multiplex measure.
#	4) Compare them and produce various plots.
#
# Alexandre Reiffers 12/2015
# Vincent Labatut 12/2015
#############################################################################################
# setwd("D:/Eclipse/workspaces/Networks/MultiplexCentrality")
# source("src/main.R")
source('src/gradient.R')
source('src/model.R')
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
l <- 20								# number of distinct values of p
p.vals = round(c(1:l)/(l+1),2)		# distinct values of p

# plot folder
plot.folder <- "plots2"

# process each multiplex network
network.names <- names(data.pars)
for(multiplex.index in 1:length(data.pars))
{	network.name <- network.names[multiplex.index]
	data.par <- data.pars[[network.name]]
	cat("Processing network ",multiplex.index," (",network.name,")\n",sep="")
	
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
	opinion.centralities <- array(0,c(l,number.layers*number.nodes))
	other.centralities <- as.matrix(mutiplex.centralities)[1:(number.layers*number.nodes),measures]
	class(other.centralities) <- "numeric"
	dir.create(paste(plot.folder,"/",network.name,sep=""),showWarnings=FALSE,recursive=TRUE)
  	
	# init correlation table
	correlation.values <- matrix(NA,nrow=l,ncol=length(measures))
	colnames(correlation.values) <- measures
  
	# define plot titles
	titles.correlation <- c()
	titles.ranking <- matrix(NA,ncol=length(measures),nrow=l)
	colnames(titles.ranking) <- measures
	titles.density <- rep(NA,l)
	for(measure in measures)
	{	titles.correlation[measure] <- paste("Correlation with ",measure," in ",network.name," Multiplex",sep="")
		for(i in 1:l)
			titles.ranking[i,measure] <- paste("Entities sorted by ranking difference with ",measure," in ",network.name," Multiplex for p=",p.vals[i],sep="")
	}  
	for(i in 1:l)
		titles.density[i] <- paste("Histogram of Centralities for p=",p.vals[i]," in ",network.name," Multiplex",sep="")

	# process our centrality measure
	cat("  Processing opinion centrality\n",sep="")
	for(i in 1:l)
	{	cat("    for p=",p.vals[i]," (",i,"/",l,")",sep="")
		alpha <- array(0.9,c(number.layers*number.nodes,1))
	
		####### processing A in function of p
		parameter.topics=p.vals[i]
		A <- array((1-p.vals[i])/2*(number.layers-1),c(number.layers,number.layers))-diag(array((1-p.vals[i])/2*(number.layers-1),c(number.layers)))+diag(array(p.vals[i],c(number.layers)))-diag(array(1,c(number.layers)))
		A <- solve(A)
 
		b <- array(1/(number.layers),c(number.layers,1))#modif
		centrality <- process.opinion.centrality(A, network=multiplex.network, alpha, budget=1, b, grad.horizon=1000)
		
		opinion.centralities[i,] <- t(centrality)
		stdev <- sd(opinion.centralities[i,])
		if(stdev==0)
			cat("....WARNING: stdev=",stdev,"\n",sep="")
		else
			cat("....stdev=",stdev,"\n",sep="")
	}
	
	# record our measure as a table
	out.file <- paste(data.par$data.folder,"opinion-centrality.csv",sep="")
	col.layer <- c(sapply(1:number.layers, function(i) rep(i,number.nodes)))
	col.node <- c(sapply(1:number.layers, function(i) 1:number.nodes))
	centr <- cbind(col.layer, col.node, t(opinion.centralities))
	colnames(centr) <- c("Layer", "Node", paste("p=",p.vals,sep=""))
	write.csv2(centr, file=out.file)

	# compare each measure to our own
	# we consider all possible values of p
	for(i in 1:l)
	{	# produce the opinion centrality histogram for the considered value of p 
		dfm <- data.frame(Centrality=opinion.centralities[i,])
		plt <- ggplot(data=dfm, aes(x=Centrality)) +  geom_histogram(colour="steelblue", fill="steelblue1", alpha=0.3)+ggtitle(titles.density[i])
		ggsave(plot=plt, file=paste(plot.folder,"/",network.name,"/",titles.density[i],".pdf",sep=""))
		
		# process each MuxViz measure individually
		for(measure in measures)
		{	# check if MuxViz could process the considered measure
			if(!any(is.na(other.centralities[,measure])))
			{	# process the rank correlation with our measure
				correlation.values[i,measure] <- cor(opinion.centralities[i,], other.centralities[,measure], method="spearman")
	
				# plot ranking differences
				dfm <- data.frame(number.nodes=c(1:(number.layers*number.nodes)),Ranking.difference=sort(rank(opinion.centralities[i,])-rank(other.centralities[,measure])))
				plt <- ggplot(data=dfm, aes(x=number.nodes,y=Ranking.difference)) + geom_point(size=4,colour="steelblue")+ geom_line(size=1,colour="steelblue")+ggtitle(titles.ranking[i,measure])
				ggsave(plot=plt, file=paste(plot.folder,"/",network.name,"/",titles.ranking[i,measure],".pdf",sep=""))
				
				# ranking differences as a barplot
				plot <- generate.comparison.barplot(ref.vals=other.centralities[,measure], comp.vals=opinion.centralities[i,], ref.measure=measure)
				#TODO inverser les deux mesures dans les autres appels
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
			cat("    WARNING: Opinion centrality could not be processed, so no correlation plot for ",measure,"\n",sep="")
		else
		{	cat("    With measure ",measure,"\n")
			dfm <- data.frame(p=p.vals,Correlation=correlation.values[,measure])
			plt <- ggplot(data=dfm, aes(x=p,y=Correlation)) + geom_point(size=4,colour="steelblue")+ geom_line(size=1,colour="steelblue")+ggtitle(titles.correlation[measure])
			ggsave(plot=plt, file=paste(plot.folder,"/",network.name,"/",titles.correlation[measure],".pdf",sep=""))
		}
	}
}
