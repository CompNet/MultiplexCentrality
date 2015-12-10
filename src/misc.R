#############################################################################################
# Various secondary functions.
#
# Vincent Labatut 12/2015
#############################################################################################
library(ggplot2)



#############################################################################################
# Loads an Rdata file supposed to contain a single object, then returns it.
# The source code was taken from StackOVerflow user Hong Ooi, see the following URL: 
# http://stackoverflow.com/questions/5577221/how-can-i-load-an-object-into-a-variable-name-that-i-specify-from-an-r-data-file
# 
# filename: name of the file to load.
# returns: the single object contained in the specified file.
#############################################################################################
retrieve.rdata.object <- function(filename)
{	env <- new.env()
	nm <- load(filename, env)[1]
	return(env[[nm]])
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
# returns: an object representing the generated plot.
#############################################################################################
generate.comparison.barplot <- function(ref.vals, comp.vals, ref.measure, comp.measure="Interest Centrality")
{	ref.rk <- rank(ref.vals,ties.method="min")
	comp.rk <- rank(comp.vals,ties.method="min")
	diff <- ref.rk - comp.rk
	idx <- order(ref.vals)

	barplot(height=diff[idx])
	
	data <- data.frame(
			x=1:length(ref.vals),
			y=diff[idx])
	g <- ggplot(data=data, aes(x=x, y=y))
	g <- g + geom_bar(stat="identity")
	g <- g + ggtitle(paste("Rank changes for ",comp.measure," vs ", ref.measure,sep=""))
	g <- g + xlab(paste("Nodes ordered by decreasing ",ref.measure,sep="")) 
	g <- g + ylab(paste("Rank changes obtained with ",comp.measure,sep=""))
#	print(g)
	return(g)
}
