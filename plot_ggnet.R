#####################################################
#                                                   #
#    R script to generate network visualisations    #
#                   using ggnet                     #
#               Version 2016-09-16                  #      
#                                                   #
#####################################################

# Load required libraries.

# library(GGally)
library(network)
library(sna)
library(intergraph)
library(ggplot2)
library(ggnet)
library(randomcoloR)
library(gridExtra)
library(ggthemes)

# set working directory

# Case 1

# setwd("~/ownCloud/Innovation Network Analysis/Case studies/HF") # MacBook
setwd("d:/Andrew/ownCloud/Innovation Network Analysis/Case studies/HF") # Home PC
# setwd("c:/Users/ter053/ownCloud/Innovation Network Analysis/Case studies/HF") # work PC

# Case 2

# setwd("~/ownCloud/Innovation Network Analysis/Case studies/AMR") # MacBook
# setwd("d:/Andrew/ownCloud/Innovation Network Analysis/Case studies/AMR") # Home PC
# setwd("c:/Users/ter053/ownCloud/Innovation Network Analysis/Case studies/AMR") # work PC

# Case 3

# setwd("~/ownCloud/Innovation Network Analysis/Case studies/GIHH") # MacBook
 setwd("d:/Andrew/ownCloud/Innovation Network Analysis/Case studies/GIHH") # Home PC
# setwd("c:/Users/ter053/ownCloud/Innovation Network Analysis/Case studies/GIHH") # work PC

 # Load sna files created using pre-process.R script.
 
graph.list <- c("knowledge.provider.net", "tacit.knowledge.net", "explicit.knowledge.net", 
                "idea.generation.net", "idea.realisation.net", "affect.based.trust.net", 
                "cognition.based.trust.net", "prior.relationship.net", "report.to.net")

for (g in graph.list){
  eval(parse(text = paste0('load("', g,'.rda")')))
  eval(parse(text = paste0(g,'.sna <- asNetwork(', g,')')))
}


# Create a custom color scale.

library(randomcoloR)
n <- 18 # number of employers (varies according to case study)
col.scale <- distinctColorPalette(n)

# Fix plot parameters.

x = gplot.layout.kamadakawai(knowledge.provider.net.sna, NULL) # use densest network. 

ns <- 6
ls <- 2
ag <- 0
as <- 3
es <- 0.1
ec <- "grey50"




# Create plot objects.

p1 <- ggnet2(knowledge.provider.net.sna, mode = x[,1:2], 
             node.size = (1 / knowledge.provider.net.sna %v% "constraint")^2, 
             label = "vertex.id", label.size = ls, color = "employer", 
             palette = col.scale, arrow.size = as, arrow.gap = ag, 
             edge.size = es, edge.color = ec)

p2 <- ggnet2(explicit.knowledge.net.sna, mode = x[,1:2], 
             node.size = (1 / explicit.knowledge.net.sna %v% "constraint")^2, 
             label = "vertex.id", label.size = ls, color = "employer", 
             palette = col.scale, arrow.size = as, arrow.gap = ag, 
             edge.size = es, edge.color = ec)

p3 <- ggnet2(tacit.knowledge.net.sna, mode = x[,1:2], 
             node.size = (1 / tacit.knowledge.net.sna %v% "constraint")^2, 
             label = "vertex.id", label.size = ls, color = "employer", 
             palette = col.scale, arrow.size = as, arrow.gap = ag, 
             edge.size = es, edge.color = ec)

# Set theme.

# b = theme_economist() 
# b = theme_economist_white()
# b = theme_few()
b = theme(panel.background = element_rect(color = "black"))
z = guides(color = FALSE)


# plot networks in a grid layout

pdf(file = "networks.pdf", width= 18, height = 6, useDingbats=F) 

gridExtra::grid.arrange(p1 + z + ggtitle("All Knowledge") + b,
                        p2 + z + ggtitle("Predominantly Explict Knowledge") + b,
                        p3 + z + ggtitle("Predominantly Tacit Knowledge") + b,
                        nrow = 1)


dev.off()
