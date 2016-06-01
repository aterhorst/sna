library(GGally)
library(network)
library(sna)
library(intergraph)
library(ggplot2)
library(ggnet)
library(ggnetwork)
library(RColorBrewer)
library(randomcoloR)
library(gridExtra)
library(cowplot)

# set working directory

# Case study 1

# setwd("~/ownCloud/Innovation Network Analysis/Case studies/HF") # MacBook
# setwd("d:/Andrew/ownCloud/Innovation Network Analysis/Case studies/HF") # Home PC
# setwd("c:/Users/ter053/ownCloud/Innovation Network Analysis/Case studies/HF") # work PC

# Case study 2

# setwd("~/ownCloud/Innovation Network Analysis/Case studies/AMR") # MacBook
# setwd("d:/Andrew/ownCloud/Innovation Network Analysis/Case studies/AMR") # Home PC
# setwd("c:/Users/ter053/ownCloud/Innovation Network Analysis/Case studies/AMR") # work PC

# Case study 3

setwd("~/ownCloud/Innovation Network Analysis/Case studies/GIHH") # MacBook
# setwd("d:/Andrew/ownCloud/Innovation Network Analysis/Case studies/AMR") # Home PC
# setwd("c:/Users/ter053/ownCloud/Innovation Network Analysis/Case studies/AMR") # work PC


graph.list <- c("knowledge.provider.net", "tacit.knowledge.net", "explicit.knowledge.net", 
                "idea.generation.net", "idea.realisation.net", "affect.based.trust.net", 
                "cognition.based.trust.net", "prior.relationship.net", "report.to.net")

for (g in graph.list){
  eval(parse(text = paste0('load("', g,'.rda")')))
  eval(parse(text = paste0(g,'.sna <- asNetwork(', g,')')))
}

#Create a custom color scale

library(randomcoloR)
n <- 23 # number of employers (varies)
col.scale <- distinctColorPalette(n)

x = gplot.layout.fruchtermanreingold(knowledge.provider.net.sna, NULL)
ns <- 6
ls <- 2
ag <- 0.65
as <- 3
es <- 0.1
ec <- "grey50"

p1 <- ggnet2(knowledge.provider.net.sna, mode = x[,1:2], node.size = ns, label = "vertex.id", label.size = ls, color = "employer", palette = col.scale, arrow.size = as, arrow.gap = ag, edge.size = es, edge.color = ec)
p2 <- ggnet2(explicit.knowledge.net.sna, mode = x[,1:2], node.size = ns, label = "vertex.id", label.size = ls, color = "employer", palette = col.scale, arrow.size = as, arrow.gap = ag, edge.size = es, edge.color = ec)
p3 <- ggnet2(tacit.knowledge.net.sna, mode = x[,1:2], node.size = ns, label = "vertex.id", label.size = ls, color = "employer", palette = col.scale, arrow.size = as, arrow.gap = ag, edge.size = es, edge.color = ec)
p4 <- ggnet2(idea.generation.net.sna, mode = x[,1:2], node.size = ns, label = "vertex.id", label.size = ls, color = "employer", palette = col.scale, arrow.size = as, arrow.gap = ag, edge.size = es, edge.color = ec)
p5 <- ggnet2(idea.realisation.net.sna, mode = x[,1:2], node.size = ns, label = "vertex.id", label.size = ls, color = "employer", palette = col.scale, arrow.size = as, arrow.gap = ag, edge.size = es, edge.color = ec)
p6 <- ggnet2(affect.based.trust.net.sna, mode = x[,1:2], node.size = ns, label = "vertex.id", label.size = ls, color = "employer", palette = col.scale, arrow.size = as, arrow.gap = ag, edge.size = es, edge.color = ec)
p7 <- ggnet2(cognition.based.trust.net.sna, mode = x[,1:2], node.size = ns, label = "vertex.id", label.size = ls, color = "employer", palette = col.scale, arrow.size = as, arrow.gap = ag, edge.size = es, edge.color = ec)
p8 <- ggnet2(prior.relationship.net.sna, mode = x[,1:2], node.size = ns, label = "vertex.id", label.size = ls, color = "employer", palette = col.scale, arrow.size = as, arrow.gap = ag, edge.size = es, edge.color = ec)
p9 <- ggnet2(report.to.net.sna, mode = x[,1:2], node.size = ns, label = "vertex.id",label.size = ls, color = "employer", palette = col.scale, arrow.size = as, arrow.gap = ag, edge.size = es, edge.color = ec)

# common plotting parameters
b = theme(panel.background = element_rect(color = "grey50"))
z = guides(color = FALSE)

# plot networks in a grid layout

pdf(file = "networks.pdf", width= 18, height = 12, useDingbats=F) 

gridExtra::grid.arrange(p1 + z + ggtitle("Knowledge Provider") + b,
                        p2 + z + ggtitle("Explict Knowledge Provider") + b,
                        p3 + z + ggtitle("Tacit Knowledge Provider") + b,
                        p4 + z + ggtitle("Idea Generation") + b,
                        p5 + z + ggtitle("Idea Realisation") + b,
                        p6 + z + ggtitle("Affect-based Trust") + b,
                        p7 + z + ggtitle("Cognition-based Trust") + b,
                        p8 + z + ggtitle("Prior Relationship") + b,
                        p9 + z + ggtitle("Reports To") + b,
                        nrow = 3)


dev.off()
