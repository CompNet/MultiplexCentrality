#############################################################################################
# Functions used to produce all the plots.
#
# Alexandre Reiffers 11/2015
# Vincent Labatut 12/2015
#############################################################################################
library(ggplot2)
library(reshape2)
library(corrplot)
library(plotrix)



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
		if(!is.na(format))
		{	if(format=="PDF")
				pdf(file=plot.filename,bg="white")
			else if(format=="PNG")
				png(filename=plot.filename,width=800,height=800,units="px",pointsize=20,bg="white")
		}
		
		print(plt) #suppressMessages(print(plt))
		
		if(!is.na(format))
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
		if(!is.na(format))
		{	if(format=="PDF")
				pdf(file=plot.filename,bg="white")
			else if(format=="PNG")
				png(filename=plot.filename,width=800,height=800,units="px",pointsize=20,bg="white")
		}
		
		print(plt) #suppressMessages(print(plt))
		
		if(!is.na(format))
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
{	#print(cor.vals)
	data <- t(cor.vals)
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
		if(!is.na(format))
		{	if(format=="PDF")
				pdf(file=plot.filename,bg="white")
			else if(format=="PNG")
				png(filename=plot.filename,width=800,height=800,units="px",pointsize=20,bg="white")
		}
		
		print(plt) #suppressMessages(print(plt))
		
		if(!is.na(format))
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
		if(!is.na(format))
		{	if(format=="PDF")
				pdf(file=plot.filename,bg="white")
			else if(format=="PNG")
				png(filename=plot.filename,width=800,height=800,units="px",pointsize=20,bg="white")
		}
		
		print(plt) #suppressMessages(print(plt))
		
		if(!is.na(format))
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
		
		if(!is.na(format))
		{	if(format=="PDF")
				pdf(file=plot.filename,bg="white")
			else if(format=="PNG")
				png(filename=plot.filename,width=800,height=800,units="px",pointsize=20,bg="white")
		}
		
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
		
		if(!is.na(format))
			dev.off()
	}
}



#############################################################################################
# Combines in a single plots the rank differences for the k most central nodes (according
# to the alternate measure), between the opinion measure and all considered alternate measures.
#
# plot.file: base name of the files to generate.
# all.rank.diff: a list of matrices (one for each measure), each matrix row representing a
#				 different network, and its k cols representing the nodes.
# net.prop: properties of the considered networks.
# formats: format of the generated file ("PDF", "PNG", or both).
#############################################################################################
plot.all.rank.diff <- function(plot.file, all.rank.diff, net.prop, formats)
{	op <- par(mar = c(9.1,4.,4.,2.))
	
	for(measure in names(all.rank.diff))
	{	for(format in formats)
		{	# without normalization
			data <- t(all.rank.diff[[measure]])
			plot.filename <- paste(plot.file,"_meas=",measure,"_abs.",format,sep="")
			if(!is.na(format))
			{	if(format=="PDF")
					pdf(file=plot.filename,bg="white")
				else if(format=="PNG")
					png(filename=plot.filename,width=800,height=800,units="px",pointsize=20,bg="white")
			}
			col <- c(col2rgb("steelblue1"))/255
			barplot(data, 
				col=rgb(col[1],col[2],col[3],0.3),
				border="steelblue",
				main=paste("Rank changes for Opinion measure vs ", measure,sep=""),
#				ylim=c(-length(ref.vals),length(ref.vals)),
				xlab="",
				ylab="Rank changes obtained with Opinion measure",
				las=2,							# labels perpendicular to axis
				beside=TRUE
			)
			title(xlab=paste("Nodes ordered by decreasing ",measure,sep=""),line=7.5,)
			if(!is.na(format))
				dev.off()
			
			# with normalization
			data <- t(all.rank.diff[[measure]]/(net.prop[,"Nodes"]-1))
			plot.filename <- paste(plot.file,"_meas=",measure,"_prop.",format,sep="")
			if(!is.na(format))
			{	if(format=="PDF")
					pdf(file=plot.filename,bg="white")
				else if(format=="PNG")
					png(filename=plot.filename,width=800,height=800,units="px",pointsize=20,bg="white")
			}
			col <- c(col2rgb("steelblue1"))/255
			barplot(data, 
				col=rgb(col[1],col[2],col[3],0.3),
				border="steelblue",
				main=paste("Rank changes for Opinion measure vs ", measure,sep=""),
#				ylim=c(-length(ref.vals),length(ref.vals)),
				xlab="",
				ylab="Normalized rank changes obtained with Opinion measure",
				las=2,							# labels perpendicular to axis
				beside=TRUE
			)
			title(xlab=paste("Nodes ordered by decreasing ",measure,sep=""),line=7.5,)
			if(!is.na(format))
				dev.off()
		}
	}
	
	par(op)
}
plot.all.rank.diff(all.rank.plot.file, all.rank.diff, net.prop, formats=NA)



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
		if(!is.na(format))
		{	if(format=="PDF")
				pdf(file=plot.filename,bg="white")
			else if(format=="PNG")
				png(filename=plot.filename,width=800,height=800,units="px",pointsize=20,bg="white")
		}
		
		plot(g,layout=layout)
		
		if(!is.na(format))
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
	write.csv2(corr.mat, file=out.file)#, row.names=FALSE)

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
		if(!is.na(format))
		{	if(format=="PDF")
				pdf(file=plot.filename,bg="white")
			else if(format=="PNG")
				png(filename=plot.filename,width=800,height=800,units="px",pointsize=20,bg="white")
		}
		
		corrplot(corr.mat, 
				method="color",		# colors the cells 
				col=cor.cols(20), cl.length=21, 
#				addCoef.col="grey",	# display numerical values in the cells
				addgrid.col=NA,		# remove grid
				tl.col="black",		# color of the axis labels
				tl.cex=0.5
		)
		
		if(!is.na(format))
			dev.off()
	}
}



#############################################################################################
# Plots the processing time as a function of some network property (number of nodes, number
# of links).
#
# time.perf: table containing the measured time for all networks (rows) and value of the 
#		     measure parameter alpha (columns).
# net.prop: topological properties of the processed networks.
# plot.file: base name of the plot files.
# dispersion: whether or not to plot dispersion bars.
# formats: format of the generated plot files.
#############################################################################################
plot.time.perf <- function(time.perf, net.prop, plot.file, dispersion=TRUE, formats=c("PDF", "PNG"))
{	for(prop in colnames(net.prop))
	{	# process values to display
		aggr.perf <- apply(X=time.perf*1000,MARGIN=1,FUN=mean)
		disp.perf <- apply(X=time.perf*1000,MARGIN=1,FUN=sd)
			
        if(!all(is.na(aggr.perf)))
		{	for(format in formats)
			{	# open file
				plot.filename <- paste(plot.file,"-",prop,".",format,sep="")
				if(!is.na(format))
				{	if(format=="PDF")
						pdf(file=plot.filename,bg="white")
					else if(format=="PNG")
						png(filename=plot.filename,width=800,height=800,units="px",pointsize=20,bg="white")
				}
				
				# display points
				plot(
					x=net.prop[,prop],		# focus on each net property separately
					y=aggr.perf,			# aggregated durations (over alpha values)
					log="xy",				# logarithmic scales
					col="RED",
					xlab=prop, ylab="Duration in ms"
				)
				# add dispersion bars
				if(dispersion)
				{	dispersion(
						x=net.prop[,prop],
						y=aggr.perf,
						ulim=aggr.perf+disp.perf,
#						llim=sapply(aggr.perf-disp.perf,function(x) max(0,x)),
						llim=aggr.perf-disp.perf,
						col="RED"
					)
				}
    			
				# close file
				if(!is.na(format))
					dev.off()
			}
		}
	}
}



#############################################################################################
# Plots the comparison of processing times (opinion centrality vs. MuxViz).
# 
# plot.file: base name for the generated plots.
# net.prop.file: file containing the network topological properties.
# opinion.time.file: file containing the processing times for the opinion measure.
# other.time.file: file containing the processing times for the other measures (e.g. MuxViz).
# formats: format of the generated plot files.
#############################################################################################
plot.all.time.perf <- function(plot.file, net.prop.file, opinion.time.file, other.time.file, formats=c("PDF","PNG"))
{	# load opinion centrality times
	elapsed.times <- read.csv2(file=opinion.time.file,header=TRUE,row.names=1,check.names=FALSE)
	opinion.times <- apply(X=elapsed.times*1000,MARGIN=1,FUN=mean)
	
	# load muxViz times
	other.times <- read.csv2(file=other.time.file,header=TRUE,row.names=1,check.names=FALSE)*1000
	
	# put everything in the same matrix
	all.times <- cbind(opinion.times,other.times)
	colnames(all.times)[1] <- "Opinion"
	# set <1 values to zero, for logarithmic plotting
	all.times[all.times<1] <- 1
	
	# identify the longest time
	tmp <- all.times
	tmp[is.na(tmp)] <- 0
	pos <- which(tmp == max(tmp), arr.ind = TRUE)
	max.col <- pos[2]
	max.val <- tmp[pos]
	
	# load network properties
	net.prop <- read.csv2(file=net.prop.file,header=TRUE,row.names=1,check.names=FALSE)
	
	# plot
	for(prop in colnames(net.prop))
	{	# order nets depending on the proporty
		idx <- order(net.prop[,prop])
		
		for(format in formats)
		{	# open file
			plot.filename <- paste(plot.file,"-",prop,".",format,sep="")
			if(!is.na(format))
			{	if(format=="PDF")
					pdf(file=plot.filename,bg="white")
				else if(format=="PNG")
					png(filename=plot.filename,width=800,height=800,units="px",pointsize=20,bg="white")
			}
			
			# display points
			plot(
				x=net.prop[idx,prop],		# focus on each net property separately
				y=all.times[idx,max.col],	# aggregated durations (over alpha values)
				log="xy",					# logarithmic scales
				type='n',					# no dot
				ylim=c(1,max.val),
				xlab=prop, ylab="Duration in ms"
			)
			
			# add series
			for(c in 1:ncol(all.times))
			{	lines(
					type="o",
					x=net.prop[idx,prop],
					y=all.times[idx,c],
					col=c
				)
			}
			
			# add legend
			legend(
				"bottomright",
				colnames(all.times),
				lty=1,
				lwd=2.5,
				col=1:ncol(all.times)
			)
				
			# close file
			if(!is.na(format))
				dev.off()
		}
	}
}
