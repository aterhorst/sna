# required libraries

library(plyr)
library(dplyr)
library(igraph)

# set working directory

# setwd("~/ownCloud/Innovation Network Analysis/Case studies/HF") # MacBook
setwd("d:/Andrew/ownCloud/Innovation Network Analysis/Case studies/HF") # Home PC

# read edge list

edge_all <- read.csv("surveydata(7).csv", header = T) # read relationships worksheet from ona surveys

# read node attribute data

nodes <- read.csv("surveydata_nodes.csv", header = T) # read node worksheet from ona surveys

# assign names to node ids

edge_all$from <- with(node_summary, FakeName[match(edge_all$from, id)])
edge_all$to <- with(node_summary, FakeName[match(edge_all$to, id)])
edge_all$ownernode <- with(node_summary, FakeName[match(edge_all$ownernode, id)])


# extract networks

edge_knowledge <- filter(edge_all, relationship_set_knowledge_sharing == 1) # extract knowledge received from ties
edge_ideation <- filter(edge_all, relationship_set_idea_generation == 1) # extract idea generation with ties
edge_realisation <- filter(edge_all, relationship_idea_realisation == 1) # extract idea realisation with ties
edge_affect_trust <- filter(edge_all, relationship_set_affectbased_trust == 1) # extract affect-based trust ties
edge_cognition_trust <- filter(edge_all, relationship_set_cognitionbased_trust == 1) # extract cognition-based trust ties
edge_prior_relationships <- filter(edge_all, relationship_set_prior_relationships == 1) # extract prior relationship with ties
edge_boss <- filter(edge_all, relationship_set_managers == 1) # extract manager/supervisor ties

# knowledge ties

edge_knowledge$Codified <- 10 - edge_knowledge$Codified # reverse score level of documented knowledge

edge_knowledge$tacit <- round(rowMeans(subset(edge_knowledge, 
  select = c(Codified,Complexity,Observability)), na.rm = TRUE)/10, digits = 2) # compute level of tacitness between 0 and 1

edge_knowledge <- subset(edge_knowledge, select = c(from, to, ownernode, tacit)) # purge unwanted columns - knowledge sharing edge list

## generate tacit & explicit knowledge networks

tacit_level <- quantile(edge_knowledge$tacit) # compute quantiles
edge_tacit_knowledge <- filter(edge_knowledge, tacit >= tacit_level[4]) # filter predominantly tacit knowledge sharing ties
edge_explicit_knowledge <- filter(edge_knowledge, tacit <= tacit_level[2]) # filter predominantly explicit knowledge sharing ties
edge_mixed_knowledge <- filter(edge_knowledge, tacit > tacit_level[2] & tacit < tacit_level[4]) # filter mixed knowledge sharing ties

# create graph from edge list

knowledge_net <- graph.data.frame(edge_knowledge)
source("https://raw.githubusercontent.com/aterhorst/sna/master/reverse_direction.R")
knowledge_net <- graph.reverse(knowledge_net) # apply function to knowledge sharing ties
knowledge_net <- simplify(knowledge_net, remove.multiple = F, remove.loops = )

# add vertex attributes

# create plot

plot(knowledge_net, edge.arrow.size = .4)
dev.print(device = png, width = 960, height = 960, units = "px", "knowledge_net.png")


