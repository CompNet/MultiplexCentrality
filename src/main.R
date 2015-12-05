#############################################################################################
# Main script, performing the following steps:
# 	1) Load the networks.
#	2) Load the centrality measures previously processed with MuxViz.
#	3) Process our own multiplex measure.
#	4) Compare them and produce various plots.
#
# Alexandre Reiffers 12/2015
#############################################################################################
# setwd("D:/Eclipse/workspaces/Networks/MultiplexCentrality")
# source("src/main.R")
source('src/gradient.R')
source('src/model.R')
source('src/misc.R')



# init data-related variables
data.pars <- list()
data.pars[["EUAir"]] <- list(
	rdata.filename="data/EUAir_Multiplex_Transport/EUAir.Rdata",
	centrality.filename="data/EUAir_Multiplex_Transport/EUAir_centrality_table.csv")
data.pars[["Kapferer1"]] <- list(
	rdata.filename="data/kaptail1-GraphML/kaptail1.Rdata",
	centrality.filename="data/kaptail1-GraphML/Kapferer1_centrality_table.csv")
data.pars[["Kapferer2"]] <- list(
	rdata.filename="data/kaptail2-GraphML/kaptail2.Rdata",
	centrality.filename="data/kaptail2-GraphML/Kapferer2_centrality_table.csv")
data.pars[["Knoke"]] <- list(
	rdata.filename="data/knokbur-GraphML/knokbur.Rdata",
	centrality.filename="data/knokbur-GraphML/Knoke_centrality_table.csv")
data.pars[["london"]] <- list(
	rdata.filename="data/London_Multiplex_Transport/london.Rdata",
	centrality.filename="data/London_Multiplex_Transport/london_centrality_table.csv")
data.pars[["Padgett"]] <- list(
	rdata.filename="data/knokbur-GraphML/knokbur.Rdata",
	centrality.filename="data/padgett-GraphML/Padgett_centrality_table.csv")
data.pars[["Roethlisberger"]] <- list(
	rdata.filename="data/wiring-GraphML/wiring.Rdata",
	centrality.filename="data/wiring-GraphML/Roethlisberger_centrality_table.csv")
data.pars[["Sampson"]] <- list(
	rdata.filename="data/sampson-GraphML/sampson.Rdata",
	centrality.filename="data/sampson-GraphML/Sampson_centrality_table.csv")
data.pars[["Thurmann"]] <- list(
	rdata.filename="data/thuroff-GraphML/thuroff.Rdata",
	centrality.filename="data/thuroff-GraphML/Thurmann_centrality_table.csv")
data.pars[["Wolfe"]] <- list(
	rdata.filename="data/wolfe-GraphML/wolfe.Rdata",
	centrality.filename="data/wolfe-GraphML/Wolfe_centrality_table.csv")

# load all the networks and tables
mutiplex.network.names <- names(data.pars)
mutiplex.networks <- list()
mutiplex.centralities <- list()
for(name in mutiplex.network.names)
{	data.par <- data.pars[[name]]
	mutiplex.centralities[[name]] <- read.csv(file=data.par$centrality.filename,sep=";")
	mutiplex.networks[[name]] <- retrieve.rdata.object(data.par$rdata.filename)
}

# select the centrality measures previously processed by MuxViz
measures <- c(
	"Degree",
#	"DegreeIn",
#	"DegreeOut",
	"PageRank",
	"Eigenvector",
#	"Hub",
#	"Authority",
	"Katz"
)

# setup p
l <- 5	# number of distinct values of p
p.vals = round(c(1:l)/(l+1),2)	# distinct values of p

# process each multiplex network
for(multiplex.index in 1:length(mutiplex.networks))
{	network.name <- mutiplex.network.names[multiplex.index]
	cat("Processing network ",multiplex.index," (",network.name,")\n",sep="")
	multiplex.network <- mutiplex.networks[[network.name]]
	number.layers <- length(multiplex.network)
	number.nodes <- vcount(multiplex.network[[1]])
	
	opinion.centralities <- array(0,c(l,number.layers*number.nodes))
	other.centralities <- as.matrix(mutiplex.centralities[[network.name]])[1:(number.layers*number.nodes),measures]
	class(other.centralities) <- "numeric"
	dir.create(paste("plots/",network.name,sep=""),showWarnings=FALSE,recursive=TRUE)
  
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
	for(i in 1:l)
	{	alpha <- array(0.9,c(number.layers*number.nodes,1))
	
		####### processing A in function of p
		parameter.topics=p.vals[i]
		A <- array((1-p.vals[i])/2*(number.layers-1),c(number.layers,number.layers))-diag(array((1-p.vals[i])/2*(number.layers-1),c(number.layers)))+diag(array(p.vals[i],c(number.layers)))-diag(array(1,c(number.layers)))
		A <- solve(A)
 
		b <- array(1/(number.layers),c(number.layers,1))#modif
		centrality <- process.opinion.centrality(A, network=multiplex.network, alpha, budget=1, b, grad.horizon=1000)
		
		opinion.centralities[i,] <- t(centrality)
	}

	# compare each measure to our own
	# we consider all possible values of p
	for(i in 1:l)
	{	# produce the opinion centrality histogram for the considered value of p 
		dfm <- data.frame(Centrality=opinion.centralities[i,])
		plt <- ggplot(data=dfm, aes(x=Centrality)) +  geom_histogram(colour="steelblue", fill="steelblue1", alpha=0.3)+ggtitle(titles.density[i])
		ggsave(plot=plt, file=paste("plots/",network.name,"/",titles.density[i],".pdf",sep=""))
		
		# process each MuxViz measure individually
		for(measure in measures)
		{	# check if MuxViz could process the considered measure
			if(!any(is.na(other.centralities[,measure])))
			{	# process the rank correlation with our measure
				correlation.values[i,measure] <- cor(opinion.centralities[i,], other.centralities[,measure], method = "spearman")
	
				# plot ranking differences
				dfm <- data.frame(number.nodes=c(1:(number.layers*number.nodes)),Ranking.difference=sort(order(opinion.centralities[i,])-order(other.centralities[,measure])))
				plt <- ggplot(data=dfm, aes(x=number.nodes,y=Ranking.difference)) + geom_point(size=4,colour="steelblue")+ geom_line(size=1,colour="steelblue")+ggtitle(titles.ranking[i,measure])
				ggsave(plot=plt, file=paste("plots/",network.name,"/",titles.ranking[i,measure],".pdf",sep=""))
			}
		}
	}
	# plot the correlation between our measure and the other ones
	for(measure in measures)
	{	# check if MuxViz could process the considered measure
		if(!any(is.na(other.centralities[,measure])) & length(unique(other.centralities[,measure]))>1)
		{	cat("Processing measure ",measure,"/n")
			dfm <- data.frame(p=p.vals,Correlation=correlation.values[,measure])
			plt <- ggplot(data=dfm, aes(x=p,y=Correlation)) + geom_point(size=4,colour="steelblue")+ geom_line(size=1,colour="steelblue")+ggtitle(titles.correlation[measure])
			ggsave(plot=plt, file=paste("plots/",network.name,"/",titles.correlation[measure],".pdf",sep=""))
		}
	}
}
