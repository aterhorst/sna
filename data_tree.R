#####################################################
#                                                   #
#      R script to create data processing tree      #
#               Version 2016-09-23                  #      
#                                                   #
#####################################################

library(plyr)
library(dplyr)
library(readxl)
library(igraph)
library(randomcoloR)
library(devtools) # so we can use source_url

# Set working directory.

setwd("~/ownCloud/Innovation Network Analysis/Case studies") # MacBook
# setwd("d:/Andrew/ownCloud/Innovation Network Analysis/Case studies") # Home PC
# setwd("c:/Users/ter053/ownCloud/Innovation Network Analysis/Case studies") # work PC

# Import data.

links <- read_excel("Data_Tree.xlsx", sheet = 1)
items <- read_excel("Data_Tree.xlsx", sheet = 2)

# Create graph.


data.tree <- graph.data.frame(links, items, directed = F)

# data.tree <- make_ego_graph(data.tree, order = 4, nodes = V(data.tree)[74], mode = "in", mindist = 0)
  
# Create tree diagram.

## Fix layout.

l = layout_as_tree(data.tree, root = V(data.tree)[74])

## Rotate layout 90 degrees.

rad <- pi/2
center <- c(mean(range(l[,1])), mean(range(l[,2])))
phi <- atan2(l[,2]-center[2], l[,1]-center[1])
r   <- sqrt((l[,1]-center[1])**2 + (l[,2]-center[2])**2)
l[,1] <- r * sin(phi)
l[,2] <- r * cos(phi)


# Plot tree.

plot(data.tree, layout = l, vertex.shape = "none", vertex.label.cex = 0.75,
     vertex.label.family="Arial")





