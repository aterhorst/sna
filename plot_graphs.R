
library(igraph)

setwd("d:/Andrew/ownCloud/Innovation Network Analysis/Case studies/HF") # Home PC

# png("knowledge.png", width = 960, height = 480, units = "px")

lo <- layout.fruchterman.reingold(knowledge_net)


par(mfcol = c(1,3), mar = c(6,1,6,1))


plot(knowledge_net, edge.arrow.size = 0.2, vertex.color = "red", 
     vertex.label = V(knowledge_net)$label,
     layout = lo,
     main = "Knowledge \nProvider Network")
     
plot(knowledge_net, edge.arrow.size = 0.2, vertex.color = "light blue", 
     vertex.label = V(knowledge_net)$label,
     layout = lo,
     main = "Explicit Knowledge \nProvider Network")

plot(tacit_knowledge_net, edge.arrow.size = 0.2, vertex.color = "green", 
     vertex.label = V(knowledge_net)$label,
     layout = lo,
     main = "Tacit Knowledge \nProvider Network")


dev.print(device = png, width = 1440, height = 640, units = "px", "knowledge_provider.png")

