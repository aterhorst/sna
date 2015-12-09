
library(igraph)
library(RColorBrewer)

setwd("~/ownCloud/Innovation Network Analysis/Case studies/HF") # MacBook
# setwd("d:/Andrew/ownCloud/Innovation Network Analysis/Case studies/HF") # Home PC

# png("knowledge.png", width = 960, height = 480, units = "px")

lo <- layout.fruchterman.reingold.grid(knowledge_net,
                                       repulserad=vcount(knowledge_net)^100, 
                                       area=vcount(knowledge_net)^20) # fix layout
org <- factor(V(knowledge_net)$Org) # extract organisations
cols <- c("light green", "yellow","orange","red","lightblue","violet","pink") # assign colours to orgs
par(mfcol = c(3,3), mar = c(2,1,2,1))

# 1
plot(knowledge_net, edge.arrow.size = 0.1, vertex.color = cols[as.numeric(org)], 
     vertex.label = V(knowledge_net)$label,
     edge.color = "black",
     layout = lo,
     main = "Knowledge Ties")

# 2     
plot(knowledge_net, edge.arrow.size = 0.1, vertex.color = cols[as.numeric(org)], 
     vertex.label = V(knowledge_net)$label,
     edge.color = "black",
     layout = lo,
     main = "Explicit Knowledge Ties")

# 3
plot(tacit_knowledge_net, edge.arrow.size = 0.1, vertex.color = cols[as.numeric(org)], 
     vertex.label = V(knowledge_net)$label,
     edge.color = "black",
     layout = lo,
     main = "Tacit Knowledge Ties")

# 4
plot(idea_net, edge.arrow.size = 0.1, vertex.color = cols[as.numeric(org)], 
    vertex.label = V(knowledge_net)$label,
    edge.color = "black",
    layout = lo,
    main = "Idea Generation Ties")

# 5
plot(real_net, edge.arrow.size = 0.1, vertex.color = cols[as.numeric(org)], 
     vertex.label = V(knowledge_net)$label,
     edge.color = "black",
     layout = lo,
     main = "Idea Realisation Ties")

# 6
plot(affect_trust_net, edge.arrow.size = 0.1, vertex.color = cols[as.numeric(org)], 
     vertex.label = V(knowledge_net)$label,
     edge.color = "black",
     layout = lo,
     main = "Affect-Based Trust Ties")

# 7
plot(cog_trust_net, edge.arrow.size = 0.1, vertex.color = cols[as.numeric(org)], 
     vertex.label = V(knowledge_net)$label,
     edge.color = "black",
     layout = lo,
     main = "Cognition-Based Trust Ties")

# 8
plot(prior_net, edge.arrow.size = 0.1, vertex.color = cols[as.numeric(org)], 
     vertex.label = V(knowledge_net)$label,
     edge.color = "black",
     layout = lo,
     main = "Prior Relationships")

# 9
plot(boss_net, edge.arrow.size = 0.1, vertex.color = cols[as.numeric(org)], 
     vertex.label = V(knowledge_net)$label,
     edge.color = "black",
     layout = lo,
     main = "Reporting Ties")

# legend(1,1,legend=levels(org),col=cols, pch = 16, cex=1.25)

dev.print(device = png, width = 2048, height = 2048, units = "px", "networks.png")

