#####################################################
#                                                   #
#     R script to generate ideation adjacency       #
#          matrix for ingestion into PNet           #
#                                                   #
#####################################################

library(plyr)
library(dplyr)
library(readxl)
library(igraph)
library(MASS)

# set working directory

# setwd("~/ownCloud/Innovation Network Analysis/Case studies/HF") # MacBook
setwd("d:/Andrew/ownCloud/Innovation Network Analysis/Case studies/HF") # Home PC

# load workbook, select sheet with relationships

edge_all <- read_excel("surveydata.xlsx", sheet = 2)


# extract relationships

## edge_knowledge <- filter(edge_all, relationship_set_knowledge_sharing == 1) # extract knowledge received from ties

edge_idea <- filter(edge_all, relationship_set_idea_generation == 1) # extract idea generation with ties

## edge_realisation <- filter(edge_all, relationship_idea_realisation == 1) # extract idea realisation with ties
## edge_affect_trust <- filter(edge_all, relationship_set_affectbased_trust == 1) # extract affect-based trust ties
## edge_cognition_trust <- filter(edge_all, relationship_set_cognitionbased_trust == 1) # extract cognition-based trust ties
## edge_prior_relationships <- filter(edge_all, relationship_set_prior_relationships == 1) # extract prior relationship with ties
## edge_boss <- filter(edge_all, relationship_set_managers == 1) # extract manager/supervisor ties


# create graph from edge list

idea_net <- graph.data.frame(edge_idea)
idea_net <- simplify(idea_net, remove.multiple = F, remove.loops = TRUE)

# create check plot

plot(idea_net, edge.arrow.size = .4)

# create adjacency matrix

adj_idea <- get.adjacency(idea_net, type = "both", names = FALSE)
write.matrix(adj_idea, file = "ideation_net.txt")

