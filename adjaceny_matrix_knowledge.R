#####################################################
#                                                   #
#     R script to generate knowledge provider       #
#     adjacency matrix for ingestion into PNet      #
#                                                   #
#####################################################

library(plyr)
library(dplyr)
library(readxl)
library(igraph)
library(MASS)
library(source.gist)
library(devtools)

# set working directory

setwd("~/ownCloud/Innovation Network Analysis/Case studies/HF") # MacBook
# setwd("d:/Andrew/ownCloud/Innovation Network Analysis/Case studies/HF") # Home PC

# read in relationships sheet from onasurvey downloaded workbook

edge_all <- read_excel("surveydata.xlsx", sheet = 2) 


# extract relationships

edge_knowledge <- filter(edge_all, relationship_set_knowledge_sharing == 1) # extract knowledge received from ties

## edge_ideation <- filter(edge_all, relationship_set_idea_generation == 1) # extract idea generation ties
## edge_realisation <- filter(edge_all, relationship_idea_realisation == 1) # extract idea realisation ties
## edge_affect_trust <- filter(edge_all, relationship_set_affectbased_trust == 1) # extract affect-based trust ties
## edge_cognition_trust <- filter(edge_all, relationship_set_cognitionbased_trust == 1) # extract cognition-based trust ties
## edge_prior_relationships <- filter(edge_all, relationship_set_prior_relationships == 1) # extract prior relationship ties
## edge_boss <- filter(edge_all, relationship_set_managers == 1) # extract manager/supervisor ties


# create graph from edge list

knowledge_net <- graph.data.frame(edge_knowledge)
source_url("https://raw.githubusercontent.com/aterhorst/sna/master/reverse_direction.R") # function to reverse ties
knowledge_net <- graph.reverse(knowledge_net) # fix direction of knowledge provider ties
knowledge_net <- simplify(knowledge_net, remove.multiple = F, remove.loops = TRUE)

# create check plot

plot(knowledge_net, edge.arrow.size = .4)

# create adjacency matrix

adj_knowledge <- get.adjacency(knowledge_net, type = "both", names = FALSE)
write.matrix(adj_knowledge, file = "knowledge_net.txt")

