library("igraph")
folder <- "data/wolfe-GraphML/"

for(file in c("WOLFK","WOLFN"))
{   in.file <- paste(folder,file,".GraphML",sep="")
    g <- read.graph(in.file,format="graphml")
    
    if(!is.directed(g))
        g <- as.directed(graph=g, mode="mutual") 
    if(!is.weighted(g))
        E(g)$weight <- 1
    
    V(g)$name <- paste("V",1:vcount(g),sep="")    

    out.file <- paste(folder,file,".edgelist",sep="")
    el <- cbind(get.edgelist(g,names=FALSE)-1,E(g)$weight)
    write.table(el,out.file,row.names=FALSE,col.names=FALSE)
    
    node.names <- cbind(0:(vcount(g)-1),V(g)$name)
    colnames(node.names) <- c("nodeID","nodeLabel")
    out.file <- paste(folder,file,".nodes",sep="")
    write.table(node.names,out.file,row.names=FALSE,quote=FALSE)
}
