library("igraph")
folder <- "D:/Users/Vincent/Documents/Travail/Ecrits/_Projets/Alex/2. Centralité réseaux multiplexes/thuroff-GraphML/"

for(file in c("THURA","THURM"))
{   in.file <- paste(folder,file,".GraphML",sep="")
    g <- read.graph(in.file,format="graphml")
    
    if(!is.directed(g))
        g <- as.directed(graph=g, mode="mutual") 
    
    out.file <- paste(folder,file,".edgelist",sep="")
    write.graph(g,out.file,format="edgelist")
    
    node.names <- cbind(0:(vcount(g)-1),V(g)$name)
    colnames(node.names) <- c("nodeID","nodeLabel")
    out.file <- paste(folder,file,".nodes",sep="")
    write.table(node.names,out.file,row.names=FALSE,quote=FALSE)
}
