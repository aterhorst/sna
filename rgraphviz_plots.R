#####################################################
#                                                   #
#      R script to create fancy network graphs      #
#            using the Rgraphviz package            #
#                                                   #
#####################################################
library(devtools)

source_url("https://bioconductor.org/biocLite.R")
biocLite("NetPathMiner")

layout.by.attr <- function(graph, wc, cluster.strength=1,layout=layout.auto) {  
  g <- graph.edgelist(get.edgelist(graph)) # create a lightweight copy of graph w/o the attributes.
  E(g)$weight <- 1
  
  attr <- cbind(id=1:vcount(g), val=wc)
  g <- g + vertices(unique(attr[,2])) + igraph::edges(unlist(t(attr)), weight=cluster.strength)
  
  l <- layout(g, weights=E(g)$weight)[1:vcount(graph),]
  return(l)
}


la = layoutVertexByAttr(knowledge_net, knowledge_net$Org, cluster.strength=10, layout=layout.kamada.kawai)

plot.igraph(knowledge_net, vertex.color=knowledge_net$Org, layout=la)
