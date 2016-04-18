library("igraph")
folder <- "data/Drosophila_Multiplex_Genetic/"

edge.file <- paste(folder,"Dataset/drosophila_genetic_multiplex.edges",sep="")
edge.list <- as.matrix(read.table(edge.file))

node.file <- paste(folder,"Dataset/drosophila_genetic_nodes.txt",sep="")
node.list <- as.matrix(read.table(node.file, header=TRUE))

layer.file <- paste(folder,"Dataset/drosophila_genetic_layers.txt",sep="")
layer.list <- as.matrix(read.table(layer.file, header=TRUE))

edge.list[,2:3] <- edge.list[,2:3]
node.nbr <- max(edge.list[,2:3])
layer.nbr <- nrow(layer.list)

Drosophila <- list()
for(layer in 1:layer.nbr)
{	#cat("Processing layer",layer,"\n")
	g <- graph.empty(n=node.nbr, directed=TRUE)
	g$name <- layer.list[layer,2]
	V(g)$name <- node.list[,"nodeLabel"]
	idx <- which(edge.list[,1]==layer)
	flattened <- c(t(edge.list[idx,2:3]))
	g <- add.edges(graph=g, edges=flattened)
	g <- simplify(graph=g, remove.multiple=TRUE)
	Drosophila[[layer]] <- g
	
	# record for muxviz
	res.folder <- paste(folder,"MuxViz/",sep="")
	dir.create(res.folder,showWarnings=FALSE,recursive=TRUE,)
	layer.name <- sub(" ", "", layer.list[layer,2])
	out.file <- paste(res.folder,layer.name,".edgelist",sep="")
	write.graph(g,out.file,format="edgelist")
	node.names <- cbind(0:(vcount(g)-1),V(g)$name)
	colnames(node.names) <- c("nodeID","nodeLabel")
	out.file <- paste(res.folder,layer.name,".nodes",sep="")
	write.table(node.names,out.file,row.names=FALSE,quote=FALSE)
	cat("D:\\\\Drosophila_Multiplex_Genetic\\\\MuxViz\\\\",layer.name,".edgelist;",layer.name,";D:\\\\Drosophila_Multiplex_Genetic\\\\MuxViz\\\\",layer.name,".nodes\n",sep="")
}

# record as R object
print(Drosophila)
data.file <- paste(folder,"Drosophila.Rdata",sep="")
save(Drosophila, file=data.file)
