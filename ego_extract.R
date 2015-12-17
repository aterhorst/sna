library(igraph)

# extract egonets

list <- graph.neighborhood(knowledge_net,1,1)
sub <- subgraphlist[[1]]
plot(sub)