library("igraph")
folder <- "data/FAO_Multiplex_Trade/"

edge.file <- paste(folder,"Dataset/fao_trade_multiplex.edges",sep="")
edge.list <- as.matrix(read.table(edge.file))

node.file <- paste(folder,"Dataset/fao_trade_nodes.txt",sep="")
node.list <- as.matrix(read.table(node.file, header=TRUE))

layer.file <- paste(folder,"Dataset/fao_trade_layers.txt",sep="")
layer.list <- as.matrix(read.table(layer.file, header=TRUE))

node.nbr <- max(edge.list[,2:3])
layer.nbr <- nrow(layer.list)

FAO <- list()
for(layer in 1:layer.nbr)
{	#cat("Processing layer",layer,"\n")
	g <- graph.empty(n=node.nbr, directed=TRUE)
	g$name <- layer.list[layer,2]
	V(g)$name <- node.list[,"nodeLabel"]
	idx <- which(edge.list[,1]==layer)
	if(length(idx)>1)
	{	flattened <- c(t(edge.list[idx,2:3]))
		g <- add.edges(graph=g, edges=flattened, attr=list(weight=edge.list[idx,4]))
#		g <- simplify(graph=g, remove.multiple=TRUE)
		FAO[[layer]] <- g
		
		# record for muxviz
		res.folder <- paste(folder,"MuxViz/",sep="")
		dir.create(res.folder,showWarnings=FALSE,recursive=TRUE,)
		layer.name <- sub(" ", "", layer.list[layer,2])
		out.file <- paste(res.folder,layer.name,".edgelist",sep="")
		el <- cbind(get.edgelist(g,names=FALSE)-1,E(g)$weight)
		write.table(el,out.file,row.names=FALSE,col.names=FALSE)
		node.names <- cbind(0:(vcount(g)-1),V(g)$name)
		colnames(node.names) <- c("nodeID","nodeLabel")
		out.file <- paste(res.folder,layer.name,".nodes",sep="")
		write.table(node.names,out.file,row.names=FALSE,quote=FALSE)
		cat("D:\\\\FAO_Multiplex_Trade\\\\MuxViz\\\\",layer.name,".edgelist;",layer.name,";D:\\\\FAO_Multiplex_Trade\\\\MuxViz\\\\",layer.name,".nodes\n",sep="")
	}
}

# record as R object
print(FAO)
data.file <- paste(folder,"FAO.Rdata",sep="")
save(FAO, file=data.file)

