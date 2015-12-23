library(igraph)
library(RColorBrewer)
library(devtools)
library(NetPathMiner) # downloaded from bioconducter

setwd("~/ownCloud/Innovation Network Analysis/Case studies/HF") # MacBook
# setwd("d:/Andrew/ownCloud/Innovation Network Analysis/Case studies/HF") # Home PC

# source_url("https://raw.githubusercontent.com/aterhorst/sna/master/pre_process.R", sha1 = NULL) # pre-process data

lo <- layout.kamada.kawai(knowledge_net,
     repulserad=vcount(knowledge_net)^4,area=vcount(knowledge_net)^3.5) # fix layout

org <- factor(V(knowledge_net)$Org) # extract organisations

cols <- c("light green", "yellow","orange","red","lightblue","violet","pink") # assign colours to orgs


par(mfcol = c(3,3), mar = c(1,1,9,1)) # create 3x3 plot layout


# 1
plot(knowledge_net, edge.arrow.size = 0.05, vertex.color = cols[as.numeric(org)], 
     vertex.label = V(knowledge_net)$label,
     edge.color = "black",
     layout = lo)
title("Knowledge Sharing", cex.main = 2)

# 2     
plot(knowledge_net, edge.arrow.size = 0.05, vertex.color = cols[as.numeric(org)], 
     vertex.label = V(knowledge_net)$label,
     edge.color = "black",
     layout = lo)
title("Explicit Knowledge Sharing", cex.main = 2)

# 3
plot(tacit_knowledge_net, edge.arrow.size = 0.05, vertex.color = cols[as.numeric(org)], 
     vertex.label = V(knowledge_net)$label,
     edge.color = "black",
     layout = lo)
title("Tacit Knowledge Sharing", cex.main = 2)

# 4
plot(idea_net, edge.arrow.size = 0.05, vertex.color = cols[as.numeric(org)], 
    vertex.label = V(knowledge_net)$label,
    edge.color = "black",
    layout = lo)
title("Idea Generation", cex.main = 2)

# 5
plot(real_net, edge.arrow.size = 0.05, vertex.color = cols[as.numeric(org)], 
     vertex.label = V(knowledge_net)$label,
     edge.color = "black",
     layout = lo)
title("Idea Implementation", cex.main = 2)

# 6
plot(affect_trust_net, edge.arrow.size = 0.05, vertex.color = cols[as.numeric(org)], 
     vertex.label = V(knowledge_net)$label,
     edge.color = "black",
     layout = lo)
title("Affect-Based Trust", cex.main = 2)

# 7
plot(cog_trust_net, edge.arrow.size = 0.05, vertex.color = cols[as.numeric(org)], 
     vertex.label = V(knowledge_net)$label,
     edge.color = "black",
     layout = lo)
title("Cognition-Based Trust", cex.main = 2)


# 8
plot(prior_net, edge.arrow.size = 0.05, vertex.color = cols[as.numeric(org)], 
     vertex.label = V(knowledge_net)$label,
     edge.color = "black",
     layout = lo)
title("Prior Relationships", cex.main = 2)


# 9
plot(report_to_net, edge.arrow.size = 0.05, vertex.color = cols[as.numeric(org)], 
     vertex.label = V(knowledge_net)$label,
     edge.color = "black",
     layout = lo)
title("Reports To", cex.main = 2)



title("SOCIAL NETWORKS - CASE STUDY 1", outer = TRUE, line = -2, cex.main = 2.25)


# legend(1,1,legend=levels(org),col=cols, pch = 16, cex=1.25)

dev.print(device = png, width = 2048, height = 2048, units = "px", "networks.png")

