#############################################################################################
# Functions used to produce all the plots.
#
# Alexandre Reiffers 11/2015
# Vincent Labatut 12/2015
#############################################################################################
library(ggplot2)



#############################################################################################
# Generates a bar plot comparing two centrality measures. The reference measure is used as
# a baseline, and to order the nodes on the x axis. The comparison measure is used to process
# the ranking difference with the reference measure, and the result appears as the bar heights.
#
# vals: values of the measure (a numerical vector).
# measure: name of the measure.
# alpha: parameter used to build the matrix.
# folder: folder in which to generate the plots.
#############################################################################################
measure.histo <- function(vals, measure="Interest Centrality", alpha, folder)
{	data <- data.frame(Centrality=vals)
	plt <- ggplot(data=data, aes(x=Centrality))
	plt <- plt + geom_histogram(colour="steelblue", fill="steelblue1", alpha=0.3)
	plt <- plt + ggtitle(paste(measure," histogram for alpha=",alpha,sep=""))
	plt <- plt + xlab(measure) 
	plt <- plt + ylab("Count")
	
	plot.filename <- paste(folder,"/histo_meas=",measure,"_alpha=",alpha,".pdf",sep="")
#	print(data)	
	pdf(file=plot.filename,bg="white")
	print(plt) #suppressMessages(print(plt))
	dev.off()
}



#############################################################################################
# Generates a line plot of the correlation between two centrality measures.
#
# cor.vals: previously processed correlation values.
# alpha.vals: parameters used to build the matrix.
# measure: name of the alternative measure.
# folder: folder in which to generate the plots.
#############################################################################################
corr.plot <- function(cor.vals, alpha.vals, measure, folder)
{	data <- data.frame(p=p.vals,Correlation=cor.vals)
	plt <- ggplot(data=data, aes(x=alpha.vals,y=Correlation)) 
	plt <- plt + geom_point(size=4,colour="steelblue")
	plt <- plt + geom_line(size=1,colour="steelblue")
	plt <- plt + ggtitle(paste("Spearman correlation with ",measure,sep=""))
	plt <- plt + xlab(paste("alpha",sep="")) 
	plt <- plt + ylab(paste("Spearman correlation",sep=""))
	
	plot.filename <- paste(folder,"/corr_plot_meas=",measure,".pdf",sep="")
	pdf(file=plot.filename,bg="white")
#	print(data)	
	print(plt)
	dev.off()
}



#############################################################################################
# Generates a line plot comparing two centrality measures. For each node, we process the difference 
# between the comparison and the reference measure, then plot them in increasing order. 
#
# ref.vals: values for the reference measure (a numerical vector).
# comp.vals: values for the comparison measure (same).
# ref.measure: name of the reference measure.
# comp.measure: name of the comparison measure.
# alpha: parameter used to build the matrix.
# folder: folder in which to generate the plots.
#############################################################################################
rank.diff.lineplot <- function(ref.vals, comp.vals, ref.measure, comp.measure="Interest Centrality", alpha, folder)
{	data <- data.frame(number.nodes=c(1:number.nodes),Ranking.difference=sort(rank(comp.vals)-rank(ref.vals)))
	plt <- ggplot(data=data, aes(x=number.nodes,y=Ranking.difference)) 
	plt <- plt + geom_point(size=4,colour="steelblue")
	plt <- plt + geom_line(size=1,colour="steelblue")
	plt <- plt + ggtitle(paste("Nodes sorted by rank difference with ",measure," for alpha=",alpha,sep=""))
	plt <- plt + xlab(paste("Nodes ordered by increasing rank difference",sep="")) 
	plt <- plt + ylab(paste(comp.measure," rank - ",ref.measure," rank",sep=""))

	plot.filename <- paste(folder,"/rank_lineplot_meas=",measure,"_alpha=",alpha,".pdf",sep="")
	pdf(file=plot.filename,bg="white")
#	print(data)	
	print(plt)
	dev.off()
}



#############################################################################################
# Generates a bar plot comparing two centrality measures. The reference measure is used as
# a baseline, and to order the nodes on the x axis. The comparison measure is used to process
# the ranking difference with the reference measure, and the result appears as the bar heights.
#
# ref.vals: values for the reference measure (a numerical vector).
# comp.vals: values for the comparison measure (same).
# ref.measure: name of the reference measure.
# comp.measure: name of the comparison measure.
# alpha: parameter used to build the matrix.
# folder: folder in which to generate the plots.
#############################################################################################
rank.diff.barplot <- function(ref.vals, comp.vals, ref.measure, comp.measure="Interest Centrality", alpha, folder)
{	ref.rk <- rank(ref.vals,ties.method="min")
	comp.rk <- rank(comp.vals,ties.method="min")
	diff <- ref.rk - comp.rk
	idx <- order(ref.vals)

	data <- data.frame(
			x=1:length(ref.vals),
			y=diff[idx])
#print(data)	
	plt <- ggplot(data=data, aes(x=x, y=y))
	plt <- plt + geom_bar(stat="identity", colour="steelblue", fill="steelblue1", alpha=0.3)
	plt <- plt + ggtitle(paste("Rank changes for ",comp.measure," vs ", ref.measure," for alpha=",alpha,sep=""))
	plt <- plt + xlab(paste("Nodes ordered by decreasing ",ref.measure,sep="")) 
	plt <- plt + ylab(paste("Rank changes obtained with ",comp.measure,sep=""))
	
	plot.filename <- paste(folder,"/rank_barplot_meas=",measure,"_alpha=",alpha,".pdf",sep="")
	pdf(file=plot.filename,bg="white")
#	print(data)	
	print(plt)
	dev.off()
}
