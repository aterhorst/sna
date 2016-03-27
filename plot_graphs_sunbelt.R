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


lo <- layout.fruchterman.reingold(knowledge.provider.net)

# lo <- layout.drl(knowledge.provider.net, use.seed = FALSE, 
#                  seed = matrix(runif(vcount(knowledge.provider.net) * 2), 
#                  ncol = 2), options = list(edge.cut=1, init.interactions=1, simmer.attraction=0), 
#                 fixed = NULL, dim = 2)
 
 
employer <- factor(V(knowledge.provider.net)$employer) # extract employer organisations

cols <- c("light green", "yellow","orange","red",
          "lightblue","violet","pink", "aquamarine") # assign colours to employer

vs <- 5 # vertex symbol size
as <- 0.005 # edge arrow size
vl <- 4 # vertex label size
ts <- 6 #subtitle size
ms <- 10 # main title size

par(mfcol = c(1,3), mar = c(1,1,3,1), oma = c(2,2,4,2)) # create 3x3 plot layout



# 1
plot(knowledge.provider.net, edge.arrow.size = as, 
     vertex.color = cols[employer], 
     vertex.size = (1.5/V(knowledge.provider.net)$constraint)*vs,
     vertex.label.cex = vl, 	 
     vertex.label = V(knowledge.provider.net)$vertex.id,
     edge.color = "black",
     edge.width = E(knowledge.provider.net)$tacit*5,
     layout = lo)
title("Knowledge Sharing", cex.main = ts)

# 2     
plot(explicit.knowledge.net, edge.arrow.size = as, 
     vertex.color = cols[employer], 
     vertex.size = na.omit(1.5/V(explicit.knowledge.net)$constraint)*vs,
     vertex.label.cex = vl, 
     vertex.label = V(explicit.knowledge.net)$vertex.id,
     edge.color = "black",
     layout = lo)
title("Explicit Knowledge Sharing", cex.main = ts)

# 3
plot(tacit.knowledge.net, edge.arrow.size = as, 
     vertex.color = cols[employer], 
     vertex.size = (1.5/V(tacit.knowledge.net)$constraint)*vs,
     vertex.label.cex = vl, 
     vertex.label = V(tacit.knowledge.net)$vertex.id,
     edge.color = "black",
     layout = lo)
title("Tacit Knowledge Sharing", cex.main = ts)


title("CASE STUDY 1", outer = TRUE, line = 2, cex.main = ms)


# legend(1,1,legend=levels(employer),col=cols, pch = 16, cex=1.25)

dev.print(device = png, width = 6000, height = 1500, units = "px", "networks.png")

