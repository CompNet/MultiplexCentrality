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
{	cat("Processing layer",layer,"\n")
	g <- graph.empty(n=node.nbr, directed=FALSE)
	g$name <- layer.list[layer]
	V(g)$name <- node.list[,"nodeLabel"]
	idx <- which(edge.list[,1]==layer)
	flattened <- c(t(edge.list[idx,2:3]))
	g <- add.edges(graph=g, edges=flattened, attr=list(weight=edge.list[idx,4]))
	london[[layer]] <- g
}

print(london)
data.file <- paste(folder,"london.Rdata",sep="")
save(london, file=data.file)
