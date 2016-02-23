#####################################################
#                                                   #
#           R script to generate igraph             #
#                    panel plot                     #
#                                                   #
#####################################################

library(igraph)
library(RColorBrewer)
library(devtools)

#setwd("~/ownCloud/Innovation Network Analysis/Case studies/HF") # MacBook
 setwd("d:/Andrew/ownCloud/Innovation Network Analysis/Case studies/HF") # Home PC

# source_url("https://raw.githubusercontent.com/aterhorst/sna/master/pre_process.R", sha1 = NULL) # pre-process data


lo <- layout_with_sugiyama(knowledge.provider.net, hgap = 0.5, maxiter = 500)

employer <- factor(V(knowledge.provider.net)$employer) # extract employer organisations

cols <- c("light green", "yellow","orange","red",
          "lightblue","violet","pink", "aquamarine") # assign colours to employer
vs <- 6
as <- 0.005

par(mfcol = c(3,3), mar = c(1,1,9,1)) # create 3x3 plot layout

#png(filename = "network.png", width = 2048, height = 2048, units = "px")

# 1
plot(knowledge.provider.net, edge.arrow.size = as, 
     vertex.color = cols[employer], 
     vertex.size = vs,
     vertex.label = V(knowledge.provider.net)$vertex.id,
     edge.color = "black",
     edge.width = E(knowledge.provider.net)$tacit*5,
     layout = lo)
title("Knowledge Sharing", cex.main = 2)

# 2     
plot(explicit.knowledge.provider.net, edge.arrow.size = as, 
     vertex.color = cols[employer], 
     vertex.size = vs,
     vertex.label = V(explicit.knowledge.provider.net)$vertex.id,
     edge.color = "black",
     layout = lo)
title("Explicit Knowledge Sharing", cex.main = 2)

# 3
plot(tacit.knowledge.provider.net, edge.arrow.size = as, 
     vertex.color = cols[employer], 
     vertex.size = vs,
     vertex.label = V(tacit.knowledge.provider.net)$vertex.id,
     edge.color = "black",
     layout = lo)
title("Tacit Knowledge Sharing", cex.main = 2)

# 4
plot(idea.generation.net, edge.arrow.size = as,
     vertex.color = cols[employer], 
     vertex.size = vs,
     vertex.label = V(idea.generation.net)$vertex.id,
     edge.color = "black",
     layout = lo)
title("Idea Generation", cex.main = 2)

# 5
plot(idea.realisation.net, edge.arrow.size = as, 
     vertex.color = cols[employer],
     vertex.size = vs,
     vertex.label = V(idea.realisation.net)$vertex.id,
     edge.color = "black",
     layout = lo)
title("Idea Realisation", cex.main = 2)

# 6
plot(affect.based.trust.net, edge.arrow.size = as, 
     vertex.color = cols[employer],
     vertex.size = vs,
     vertex.label = V(affect.based.trust.net)$vertex.id,
     edge.color = "black",
     layout = lo)
title("Affect-Based Trust", cex.main = 2)

# 7
plot(cognition.based.trust.net, edge.arrow.size = as, 
     vertex.color = cols[employer],
     vertex.size = vs,
     vertex.label = V(cognition.based.trust.net)$vertex.id,
     edge.color = "black",
     layout = lo)
title("Cognition-Based Trust", cex.main = 2)


# 8
plot(prior.relationship.net, edge.arrow.size = as, 
     vertex.color = cols[employer],
     vertex.size = vs,
     vertex.label = V(prior.relationship.net)$vertex.id,
     edge.color = "black",
     layout = lo)
title("Prior Relationships", cex.main = 2)


# 9
plot(report.to.net, edge.arrow.size = as, 
     vertex.color = cols[employer],
     vertex.size = vs,
     vertex.label = V(report.to.net)$vertex.id,
     edge.color = "black",
     layout = lo)
title("Reports To", cex.main = 2)



title("SOCIAL NETWORKS - CASE STUDY 2", outer = TRUE, line = -2, cex.main = 2.25)


# legend(1,1,legend=levels(employer),col=cols, pch = 16, cex=1.25)

dev.print(device = png, width = 4096, height = 4096, units = "px", "networks.png")

