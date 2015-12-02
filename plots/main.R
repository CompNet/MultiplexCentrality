#load("/Users/jeanlouis/Downloads/padgett.Rdata")
#load("/Users/jeanlouis/Downloads/data/wolfe-GraphML/wolfe.Rdata")
#load("/Users/jeanlouis/Downloads/data/knokbur-GraphML/knokbur.Rdata")
#load("/Users/jeanlouis/Downloads/data/wiring-GraphML/wiring.Rdata")
#load("/Users/jeanlouis/Downloads/data/thuroff-GraphML/thuroff.Rdata")
#load("/Users/jeanlouis/Downloads/data/sampson-GraphML/sampson.Rdata")
#load("/Users/jeanlouis/Downloads/data/kaptail2-GraphML/sampson.Rdata")
#load("/Users/jeanlouis/Downloads/data/kaptail1-GraphML/sampson.Rdata")
#source('~/Desktop/nws/gradient.R')

source('src/gradient.R')
source('src/model.R')

load("data/padgett-GraphML/padgett.Rdata")
load("data/wolfe-GraphML/wolfe.Rdata")
load("data/knokbur-GraphML/knokbur.Rdata")
load("data/wiring-GraphML/wiring.Rdata")
load("data/thuroff-GraphML/thuroff.Rdata")
load("data/sampson-GraphML/sampson.Rdata")
sampson <- sampson[3:length(sampson)]
#load("data/kaptail2-GraphML/sampson.Rdata")
#load("data/kaptail1-GraphML/sampson.Rdata")
Padgett_centrality_table <- read.csv(file="data/padgett-GraphML/Padgett_centrality_table.csv",sep=";")
Wolfe_centrality_table <- read.csv(file="data/wolfe-GraphML/Wolfe_centrality_table.csv",sep=";")
Knoke_centrality_table <- read.csv(file="data/knokbur-GraphML/Knoke_centrality_table.csv",sep=";")
Roethlisberger_centrality_table <- read.csv(file="data/wiring-GraphML/Roethlisberger_centrality_table.csv",sep=";")
Thurmann_centrality_table <- read.csv(file="data/thuroff-GraphML/Thurmann_centrality_table.csv",sep=";")
Sampson_centrality_table <- read.csv(file="data/sampson-GraphML/Sampson_centrality_table.csv",sep=";")

#Padgett_centrality_table
#degree<-mutiplex.centralities[1][7]
#v3<-c(Padgett_centrality_table$DegreeIn)[1:32]
#v4<-c(Padgett_centrality_table$DegreeOut)[1:32]
#v5<-c(Padgett_centrality_table$PageRank)[1:32]
#v6<-c(Padgett_centrality_table$Katz)[1:32]
#V<-matrix(c(v2,v3,v4,v5,v6),nrow=5) 

measures <- c("Degree","PageRank","Eigenvector","Katz")
mutiplex.networks<-list(knokbur,padgett,sampson,thuroff,wiring,wolfe)
mutiplex.network.names <- c("Knoke","Padgett","Sampson","Thuroff","Wiring","Wolfe")
mutiplex.centralities <- list(Knoke_centrality_table,Padgett_centrality_table,Sampson_centrality_table,Thurmann_centrality_table,Roethlisberger_centrality_table,Wolfe_centrality_table)
l<-5
p.vals = round(c(1:l)/(l+1),2)
for(multiplex.index in 1:length(mutiplex.networks)){
	cat("Processing network ",multiplex.index," (",mutiplex.network.names[multiplex.index],")\n",sep="")
	multiplex.network <- mutiplex.networks[[multiplex.index]]
  number.layers <- length(multiplex.network)
  number.nodes <- vcount(multiplex.network[[1]])
  opinion.centralities <- array(0,c(l,number.layers*number.nodes))
  other.centralities <- as.matrix(mutiplex.centralities[[multiplex.index]])[1:(number.layers*number.nodes),measures]
  class(other.centralities) <- "numeric"
  dir.create(paste("plots/",mutiplex.network.names[multiplex.index],sep=""),showWarnings=FALSE,recursive=TRUE)
  
  correlation.values <- matrix(NA,nrow=l,ncol=length(measures))
  colnames(correlation.values) <- measures
  
  # titles
  titles.correlation <- c()
  titles.ranking <- matrix(NA,ncol=length(measures),nrow=l)
  colnames(titles.ranking) <- measures
  titles.density <- rep(NA,l)
  for(measure in measures)
  { titles.correlation[measure] <- paste("Correlation with ",measure," in ",mutiplex.network.names[multiplex.index]," Multiplex",sep="")
    for(i in 1:l)
     titles.ranking[i,measure] <- paste("Entities sorted by ranking difference with ",measure," in ",mutiplex.network.names[multiplex.index]," Multiplex for p=",p.vals[i],sep="")
  }  
    for(i in 1:l)
      titles.density[i] <- paste("Histogram of Centralities for p=",p.vals[i]," in ",mutiplex.network.names[multiplex.index]," Multiplex",sep="")

  #Centrality Definition
  for(i in 1:l)
  {	alpha <- array(0.9,c(number.layers*number.nodes,1))
	
	####### processing A in function of p
	parameter.topics=p.vals[i]
	A <- array((1-p.vals[i])/2*(number.layers-1),c(number.layers,number.layers))-diag(array((1-p.vals[i])/2*(number.layers-1),c(number.layers)))+diag(array(p.vals[i],c(number.layers)))-diag(array(1,c(number.layers)))
    A <- solve(A)
 
	b <- array(1/(number.layers),c(number.layers,1))#modif
	centrality <- process.opinion.centrality(A, network=multiplex.network, alpha, budget=1, b, grad.horizon=1000, grad.step.size=0.001)
	
    opinion.centralities[i,] <- t(centrality)
  }

for(i in 1:l){
	for(measure in measures)
	{	if(!any(is.na(other.centralities[,measure])))
		{	correlation.values[i,measure] <- cor(opinion.centralities[i,], other.centralities[,measure], method = "spearman")

			# plot ranking differences
		  dfm <- data.frame(number.nodes=c(1:(number.layers*number.nodes)),Ranking.difference=sort(order(opinion.centralities[i,])-order(other.centralities[,measure])))
		  plt <- ggplot(data=dfm, aes(x=number.nodes,y=Ranking.difference)) + geom_point(size=4,colour="steelblue")+ geom_line(size=1,colour="steelblue")+ggtitle(titles.ranking[i,measure])
		  ggsave(plot=plt, file=paste("plots/",mutiplex.network.names[multiplex.index],"/",titles.ranking[i,measure],".pdf",sep=""))
		
		  # opinion centrality histogram 
		  dfm <- data.frame(Centrality=opinion.centralities[i,])
		  plt <- ggplot(data=dfm, aes(x=Centrality)) +  geom_histogram(colour="steelblue", fill="steelblue1", alpha=0.3)+ggtitle(titles.density[i])
		  ggsave(plot=plt, file=paste("plots/",mutiplex.network.names[multiplex.index],"/",titles.density[i],".pdf",sep=""))
	  }
  }
}

for(measure in measures)
{	if(!any(is.na(other.centralities[,measure])) & length(unique(other.centralities[,measure]))>1)
	{	cat("Processing measure ",measure,"\n")
		dfm <- data.frame(p=p.vals,Correlation=correlation.values[,measure])
		plt <- ggplot(data=dfm, aes(x=p,y=Correlation)) + geom_point(size=4,colour="steelblue")+ geom_line(size=1,colour="steelblue")+ggtitle(titles.correlation[measure])
		ggsave(plot=plt, file=paste("plots/",mutiplex.network.names[multiplex.index],"/",titles.correlation[measure],".pdf",sep=""))
	}
}

}
