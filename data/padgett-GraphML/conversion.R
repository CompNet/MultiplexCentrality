library("igraph")
folder <- "data/padgett-GraphML/"

for(file in c("PADGB","PADGM"))
{   in.file <- paste(folder,file,".GraphML",sep="")
    g <- read.graph(in.file,format="graphml")
    
    out.file <- paste(folder,file,".edgelist",sep="")
    write.graph(g,out.file,format="edgelist")
    
    node.names <- cbind(0:(vcount(g)-1),V(g)$name)
    colnames(node.names) <- c("nodeID","nodeLabel")
    out.file <- paste(folder,file,".nodes",sep="")
    write.table(node.names,out.file,row.names=FALSE,quote=FALSE)
}
