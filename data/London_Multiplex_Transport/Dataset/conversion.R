library("igraph")
folder <- "data/London_Multiplex_Transport/"

edge.file <- paste(folder,"Dataset/london_transport_multiplex.edges",sep="")
edge.list <- as.matrix(read.table(edge.file))

node.file <- paste(folder,"Dataset/london_transport_nodes.txt",sep="")
node.list <- as.matrix(read.table(node.file, header=TRUE))

layer.list <- c("Tube", "Overground", "Railway")

edge.list[,2:3] <- edge.list[,2:3] + 1
node.nbr <- max(edge.list[,2:3])
layer.nbr <- length(layer.list)

london <- list()
for(layer in 1:layer.nbr)
{	#cat("Processing layer",layer,"\n")
	g <- graph.empty(n=node.nbr, directed=FALSE)
	g$name <- layer.list[layer]
	V(g)$name <- node.list[,"nodeLabel"]
	idx <- which(edge.list[,1]==layer)
	flattened <- c(t(edge.list[idx,2:3]))
	g <- add.edges(graph=g, edges=flattened, attr=list(weight=edge.list[idx,4]))
	london[[layer]] <- g
	
	# record for muxviz
	layer.name <- sub(" ", "", layer.list[layer])
	out.file <- paste(folder,"MuxViz/",layer.name,".edgelist",sep="")
	el <- cbind(get.edgelist(g,names=FALSE)-1,E(g)$weight)
	write.table(el,out.file,row.names=FALSE,col.names=FALSE)
	node.names <- cbind(0:(vcount(g)-1),V(g)$name)
	colnames(node.names) <- c("nodeID","nodeLabel")
	out.file <- paste(folder,"MuxViz/",layer.name,".nodes",sep="")
	write.table(node.names,out.file,row.names=FALSE,quote=FALSE)
	cat("D:\\\\London_Multiplex_Transport\\\\MuxViz\\\\",layer.name,".edgelist;",layer.name,";D:\\\\London_Multiplex_Transport\\\\MuxViz\\\\",layer.name,".nodes\n",sep="")
}

# record as R object
print(london)
data.file <- paste(folder,"london.Rdata",sep="")
save(london, file=data.file)
