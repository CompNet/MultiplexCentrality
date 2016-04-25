#############################################################################################
# Functions used to produce all the plots.
#
# Alexandre Reiffers 11/2015
# Vincent Labatut 12/2015
#############################################################################################
library(ggplot2)
library(reshape2)
library(corrplot)



#############################################################################################
# Generates a bar plot comparing two centrality measures. The reference measure is used as
# a baseline, and to order the nodes on the x axis. The comparison measure is used to process
# the ranking difference with the reference measure, and the result appears as the bar heights.
#
# vals: values of the measure (a numerical vector).
# measure: name of the measure.
# alpha: parameter used to build the matrix.
# folder: folder in which to generate the plots.
# formats: format of the generated file ("PDF", "PNG", or both).
#############################################################################################
measure.histo <- function(vals, measure="Opinion Centrality", alpha, folder, formats=c("PDF", "PNG"))
{	data <- data.frame(Centrality=vals)
	plt <- ggplot(data=data, aes(x=Centrality))
	plt <- plt + geom_histogram(colour="steelblue", fill="steelblue1", alpha=0.3)
	plt <- plt + ggtitle(paste(measure," histogram for alpha=",alpha,sep=""))
	plt <- plt + xlab(measure) 
	plt <- plt + ylab("Count")
	
	# create folder
	plot.folder <- paste(folder,"/histograms",sep="")
	dir.create(plot.folder,showWarnings=FALSE,recursive=TRUE)
	
	# record plot
#	print(data)
	for(format in formats)
	{	plot.filename <- paste(plot.folder,"/meas=",measure,"_alpha=",alpha,".",format,sep="")
		if(format=="PDF")
			pdf(file=plot.filename,bg="white")
		else if(format=="PNG")
			png(filename=plot.filename,width=800,height=800,units="px",pointsize=20,bg="white")
		print(plt) #suppressMessages(print(plt))
		dev.off()
	}
}



#############################################################################################
# Generates a line plot of the correlation between two centrality measures.
#
# cor.vals: previously processed correlation values.
# alpha.vals: parameters used to build the matrix.
# measure: name of the alternative measure.
# folder: folder in which to generate the plots.
# formats: format of the generated file ("PDF", "PNG", or both).
#############################################################################################
corr.plot <- function(cor.vals, alpha.vals, measure, folder, formats=c("PDF", "PNG"))
{	data <- data.frame(Alpha=alpha.vals,Correlation=cor.vals)
	plt <- ggplot(data=data, aes(x=Alpha,y=Correlation)) 
#	plt <- plt + geom_point(size=4,colour="steelblue")
	plt <- plt + geom_line(size=1,colour="steelblue")
	plt <- plt + ylim(c(-1,1))
	plt <- plt + ggtitle(paste("Spearman correlation with ",measure,sep=""))
	plt <- plt + xlab(expression(paste("",alpha,sep=""))) 
	plt <- plt + ylab(expression(paste("Spearman correlation ",rho,sep="")))
	
	# create folder
	plot.folder <- paste(folder,"/corr_plots",sep="")
	dir.create(plot.folder,showWarnings=FALSE,recursive=TRUE)
	
	# record plot
#	print(data)
	for(format in formats)
	{	plot.filename <- paste(plot.folder,"/meas=",measure,".",format,sep="")
		if(format=="PDF")
			pdf(file=plot.filename,bg="white")
		else if(format=="PNG")
			png(filename=plot.filename,width=800,height=800,units="px",pointsize=20,bg="white")
		print(plt) #suppressMessages(print(plt))
		dev.off()
	}
}



#############################################################################################
# Generates a line plot of the correlation between the opinion measure and all the other
# considered multiplex centrality measures.
#
# cor.vals: previously processed correlation values.
# alpha.vals: parameters used to build the matrix.
# measures: names of the alternative measures.
# folder: folder in which to generate the plots.
# formats: format of the generated file ("PDF", "PNG", or both).
#############################################################################################
corr.plot.all <- function(cor.vals, alpha.vals, measures, folder, formats=c("PDF", "PNG"))
{	data <- t(cor.vals)
	colnames(data) <- alpha.vals
	data <- as.data.frame(data)
	data <- cbind(measures,data)
	rownames(data) <- NULL
	colnames(data)[1] <- "Measure"
	data <- melt(data, id.vars="Measure", value.name="value", variable.name="Alpha")
	plt <- ggplot(data=data, aes(x=Alpha, y=value, group=Measure, colour=Measure))
#	plt <- plt + geom_point(size=4,colour="steelblue")
	plt <- plt + geom_line(size=1)
	plt <- plt + ylim(c(-1,1))
	plt <- plt + ggtitle(paste("Spearman correlation",sep=""))
	plt <- plt + xlab(expression(paste("",alpha,sep=""))) 
	plt <- plt + ylab(expression(paste("Spearman correlation ",rho,sep="")))
	plt <- plt + scale_x_discrete(breaks=seq(0,100,by=10))
	
	# create folder
	plot.folder <- paste(folder,"/corr_plots",sep="")
	dir.create(plot.folder,showWarnings=FALSE,recursive=TRUE)
	
	# record plot
#	print(data)
	for(format in formats)
	{	plot.filename <- paste(plot.folder,"/allmeasures.",format,sep="")
		if(format=="PDF")
			pdf(file=plot.filename,bg="white")
		else if(format=="PNG")
			png(filename=plot.filename,width=800,height=800,units="px",pointsize=20,bg="white")
		print(plt) #suppressMessages(print(plt))
		dev.off()
	}
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
# formats: format of the generated file ("PDF", "PNG", or both).
#############################################################################################
rank.diff.lineplot <- function(ref.vals, comp.vals, ref.measure, comp.measure="Opinion Centrality", alpha, folder, formats=c("PDF", "PNG"))
{	data <- data.frame(number.nodes=c(1:number.nodes),Ranking.difference=sort(rank(comp.vals)-rank(ref.vals)))
	plt <- ggplot(data=data, aes(x=number.nodes,y=Ranking.difference)) 
	plt <- plt + geom_point(size=4,colour="steelblue")
	plt <- plt + geom_line(size=1,colour="steelblue")
	plt <- plt + ylim(c(-length(ref.vals),length(ref.vals)))
	plt <- plt + ggtitle(paste("Nodes sorted by rank difference with ",measure," for alpha=",alpha,sep=""))
	plt <- plt + xlab(paste("Nodes ordered by increasing rank difference",sep="")) 
	plt <- plt + ylab(paste(comp.measure," rank - ",ref.measure," rank",sep=""))

	# create folder
	plot.folder <- paste(folder,"/rank_lineplots",sep="")
	dir.create(plot.folder,showWarnings=FALSE,recursive=TRUE)
	
	# record plot
#	print(data)
	for(format in formats)
	{	plot.filename <- paste(plot.folder,"/meas=",measure,"_alpha=",alpha,".",format,sep="")
		if(format=="PDF")
			pdf(file=plot.filename,bg="white")
		else if(format=="PNG")
			png(filename=plot.filename,width=800,height=800,units="px",pointsize=20,bg="white")
		print(plt) #suppressMessages(print(plt))
		dev.off()
	}
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
# alpha: parameter used to build the matrix (or possibly NA if none was defined).
# folder: folder in which to generate the plots.
# formats: format of the generated file ("PDF", "PNG", or both).
#############################################################################################
rank.diff.barplot <- function(ref.vals, comp.vals, ref.measure, comp.measure="Opinion Centrality", alpha=NA, folder, formats=c("PDF", "PNG"))
{	ref.rk <- rank(ref.vals,ties.method="min")
	comp.rk <- rank(comp.vals,ties.method="min")
	diff <- comp.rk - ref.rk
	idx <- order(ref.vals, decreasing=TRUE)
	
#TODO for some reason, ggplot sometimes bugs. so we switched back to the standard barplot.	
#	data <- data.frame(
#			x=1:length(ref.vals),
#			y=diff[idx])
#	plt <- ggplot(data=data, aes(x=x, y=y))
#	plt <- plt + geom_bar(stat="identity", colour="steelblue", fill="steelblue1", alpha=0.3)
#	plt <- plt + ggtitle(paste("Rank changes for ",comp.measure," vs ", ref.measure," for alpha=",alpha,sep=""))
#	plt <- plt + ylim(c(-length(ref.vals),length(ref.vals)))
#	plt <- plt + xlab(paste("Nodes ordered by decreasing ",ref.measure,sep="")) 
#	plt <- plt + ylab(paste("Rank changes obtained with ",comp.measure,sep=""))
	
	# create folder
	plot.folder <- paste(folder,"/rank_barplots",sep="")
	dir.create(plot.folder,showWarnings=FALSE,recursive=TRUE)
	
	# record plot
#	print(data)
	for(format in formats)
	{	plot.filename <- paste(plot.folder,"/ref=",ref.measure,"_comp=",comp.measure,sep="")
		if(!is.na(alpha))
			plot.filename <- paste(plot.filename,"_alpha=",alpha,sep="")
		plot.filename <- paste(plot.filename,".",format,sep="")
		
		if(format=="PDF")
			pdf(file=plot.filename,bg="white")
		else if(format=="PNG")
			png(filename=plot.filename,width=800,height=800,units="px",pointsize=20,bg="white")
#		print(plt) #suppressMessages(print(plt))
		col <- c(col2rgb("steelblue1"))/255
		barplot(diff[idx], 
			col=rgb(col[1],col[2],col[3],0.3),
			border="steelblue",
			main=paste("Rank changes for ",comp.measure," vs ", ref.measure," for alpha=",alpha,sep=""),
			ylim=c(-length(ref.vals),length(ref.vals)),
			xlab=paste("Nodes ordered by decreasing ",ref.measure,sep=""),
			ylab=paste("Rank changes obtained with ",comp.measure,sep="")
		)
		dev.off()
	}
}



#############################################################################################
# Generates a plot representing the graph with two centrality measures: the first (ref) is
# represented as the node sizes, the second (comp) as the node colors.
#
# g: graph to plot.
# ref.vals: values for the reference measure (a numerical vector).
# comp.vals: values for the comparison measure (same).
# ref.measure: name of the reference measure.
# comp.measure: name of the comparison measure.
# alpha: parameter used to build the matrix.
# folder: folder in which to generate the plots.
# layout: layout used to plot the graph.
# scale: base size of the nodes.
# formats: format of the generated file ("PDF", "PNG", or both).
#############################################################################################
graph.plot <- function(g, ref.vals, comp.vals, ref.measure, comp.measure="Opinion Centrality", alpha, folder, layout, scale=50, formats=c("PDF", "PNG"))
{	# setup attributes
	if(length(unique(ref.vals))==1)
		V(g)$size <- 5
	else
		V(g)$size <- 5 + (ref.vals-min(ref.vals))/(max(ref.vals)-min(ref.vals)) * scale
	cscale <- colorRamp(c('skyblue3','firebrick3'))
	centrality <- (comp.vals-min(comp.vals))/(max(comp.vals)-min(comp.vals))
	if(all(is.nan(centrality)))
		centrality <- 1
	V(g)$color <- apply(cscale(centrality), 1, function(x) rgb(x[1]/255,x[2]/255,x[3]/255) )
	
	# create folder
	plot.folder <- paste(folder,"/graphs",sep="")
	dir.create(plot.folder,showWarnings=FALSE,recursive=TRUE)
	
	# record plot
#	print(data)
	for(format in formats)
	{	plot.filename <- paste(plot.folder,"/ref=",ref.measure,"_comp=",comp.measure,"_alpha=",alpha,".",format,sep="")
		if(format=="PDF")
			pdf(file=plot.filename,bg="white")
		else if(format=="PNG")
			png(filename=plot.filename,width=800,height=800,units="px",pointsize=20,bg="white")
		plot(g,layout=layout)
		dev.off()
	}
#	print(g)
#	plot(g)
	
	# export network as graphml file
	net.filename <- paste(plot.folder,"/ref=",ref.measure,"_comp=",comp.measure,"_alpha=",alpha,".graphml",sep="")
	write.graph(graph=g,file=net.filename,format="graphml")
}



#############################################################################################
# Plots a correlation matrix as a heat map (also records the matrix as a table).
#
# corr.mat: correlation matrix.
# folder: folder in which to generate the plots.
# formats: format of the generated file ("PDF", "PNG", or both).
#############################################################################################
correlation.plot <- function(corr.mat, folder, formats=c("PDF", "PNG"))
{	# create folder
	plot.folder <- paste(folder,"/corr_plots",sep="")
	dir.create(plot.folder,showWarnings=FALSE,recursive=TRUE)
	
	# record correlation matrix
	out.file <- paste(plot.folder,"/opinion-correlations.csv",sep="")
	write.csv2(corr.mat, file=out.file, row.names=FALSE)

	# remove certain labels (otherwise, can't read the plot axes)
	names <- colnames(corr.mat)
	for(i in 1:4)
	{	idx <- (0:((length(names)-1)/5))*5 + i
		idx <- idx[idx<=length(names)]
		names[idx] <- NA
	}
	colnames(corr.mat) <- names
	rownames(corr.mat) <- names
	
	# plot correlation map
	cor.cols <- colorRampPalette(c("#00007F","blue","#007FFF","cyan","white","yellow","#FF7F00","red","#7F0000"))
	for(format in formats)
	{	plot.filename <- paste(plot.folder,"/opinion-correlations.",format,sep="")
		if(format=="PDF")
			pdf(file=plot.filename,bg="white")
		else if(format=="PNG")
			png(filename=plot.filename,width=800,height=800,units="px",pointsize=20,bg="white")
		corrplot(corr.mat, 
				method="color",		# colors the cells 
				col=cor.cols(20), cl.length=21, 
#				addCoef.col="grey",	# display numerical values in the cells
				addgrid.col=NA,		# remove grid
				tl.col="black",		# color of the axis labels
				tl.cex=0.5
		)
		dev.off()
	}
}
