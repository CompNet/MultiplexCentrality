library("igraph")
folder <- "D:/Users/Vincent/Documents/Travail/Ecrits/_Projets/Alex/2. Centralité réseaux multiplexes/sampson-GraphML/"

for(file in c("SAMPDES","SAMPDLK","SAMPES","SAMPIN","SAMPLK3","SAMPNIN","SAMPNPR","SAMPPR"))
{   in.file <- paste(folder,file,".GraphML",sep="")
    g <- read.graph(in.file,format="graphml")
    
    out.file <- paste(folder,file,".edgelist",sep="")
    el <- cbind(get.edgelist(g,names=FALSE)-1,E(g)$weight)
    write.table(el,out.file,row.names=FALSE,col.names=FALSE)
    
    node.names <- cbind(0:(vcount(g)-1),V(g)$name)
    colnames(node.names) <- c("nodeID","nodeLabel")
    out.file <- paste(folder,file,".nodes",sep="")
    write.table(node.names,out.file,row.names=FALSE,quote=FALSE)
}
