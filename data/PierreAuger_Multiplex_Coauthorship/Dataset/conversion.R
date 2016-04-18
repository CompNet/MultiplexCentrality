library("igraph")
folder <- "data/PierreAuger_Multiplex_Coauthorship/"

edge.file <- paste(folder,"Dataset/pierreauger_multiplex.edges",sep="")
edge.list <- as.matrix(read.table(edge.file))

layer.file <- paste(folder,"Dataset/pierreauger_layers.txt",sep="")
layer.list <- as.matrix(read.table(layer.file, header=TRUE))

edge.list[,2:3] <- edge.list[,2:3]
node.nbr <- max(edge.list[,2:3])
layer.nbr <- nrow(layer.list)

PierreAuger <- list()
for(layer in 1:layer.nbr)
{	#cat("Processing layer",layer,"\n")
	g <- graph.empty(n=node.nbr, directed=FALSE)
	g$name <- layer.list[layer,2]
	V(g)$name <- paste("Node",0:(node.nbr-1),sep="")
	idx <- which(edge.list[,1]==layer)
	flattened <- c(t(edge.list[idx,2:3]))
	g <- add.edges(graph=g, edges=flattened, attr=list(weight=edge.list[idx,4]))
#	g <- simplify(graph=g, remove.multiple=TRUE)
	PierreAuger[[layer]] <- g
	
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
	cat("D:\\\\PierreAuger_Multiplex_Coauthorship\\\\MuxViz\\\\",layer.name,".edgelist;",layer.name,";D:\\\\PierreAuger_Multiplex_Coauthorship\\\\MuxViz\\\\",layer.name,".nodes\n",sep="")
}

# record as R object
print(PierreAuger)
data.file <- paste(folder,"PierreAuger.Rdata",sep="")
save(PierreAuger, file=data.file)
