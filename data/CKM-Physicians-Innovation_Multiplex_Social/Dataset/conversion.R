library("igraph")
folder <- "data/CKM-Physicians-Innovation_Multiplex_Social/"

edge.file <- paste(folder,"Dataset/CKM-Physicians-Innovation_multiplex.edges",sep="")
edge.list <- as.matrix(read.table(edge.file))

node.file <- paste(folder,"Dataset/CKM-Physicians-Innovation_nodes.txt",sep="")
node.list <- as.matrix(read.table(node.file, header=TRUE))

layer.file <- paste(folder,"Dataset/CKM-Physicians-Innovation_layers.txt",sep="")
layer.list <- as.matrix(read.table(layer.file, header=TRUE))

edge.list[,2:3] <- edge.list[,2:3]
node.nbr <- max(edge.list[,2:3])
layer.nbr <- nrow(layer.list)

CKM <- list()
for(layer in 1:layer.nbr)
{	#cat("Processing layer",layer,"\n")
	g <- graph.empty(n=node.nbr, directed=TRUE)
	g$name <- layer.list[layer,2]
	V(g)$name <- paste("Node",0:(node.nbr-1),sep="")
	idx <- which(edge.list[,1]==layer)
	flattened <- c(t(edge.list[idx,2:3]))
	g <- add.edges(graph=g, edges=flattened)
#	g <- simplify(graph=g, remove.multiple=TRUE)
	CKM[[layer]] <- g
	
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
#	cat("D:\\\\CKM-Physicians-Innovation\\\\MuxViz\\\\",layer.name,".edgelist;",layer.name,";D:\\\\CKM-Physicians-Innovation\\\\MuxViz\\\\",layer.name,".nodes\n",sep="")
}

# record as R object
print(CKM)
data.file <- paste(folder,"CKMPI.Rdata",sep="")
save(CKM, file=data.file)
