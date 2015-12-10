#####################################################
#                                                   #
#            R script to generate standard          #
#                 networks statistics               #
#                                                   #
#####################################################

library(igraph)
library(network)
library(intergraph)

# set working directory

setwd("~/ownCloud/Innovation Network Analysis/Case studies/HF") # MacBook
# setwd("d:/Andrew/ownCloud/Innovation Network Analysis/Case studies/HF") # Home PC

# pre-process data

source_url("https://raw.githubusercontent.com/aterhorst/sna/master/pre_process.R")

# create list of graphs to be crunched

graph_list <- c("knowledge_net", "tacit_knowledge_net", "explicit_knowledge_net", 
           "idea_net", "real_net", "affect_trust_net", "cog_trust_net", "prior_net", 
           "boss_net")

# compute standard network statistics for each network 

for (g in graph_list){
  eval(parse(text = paste0('V(', g, ')$comm <- membership(optimal.community(', g,'))'))) # modularity score
  eval(parse(text = paste0('V(', g, ')$degree <- degree(', g, ', mode = "all")'))) # no. of ties
  eval(parse(text = paste0('V(', g, ')$indegree <- degree(', g, ', mode = "in")'))) # no. of incoming ties
  eval(parse(text = paste0('V(', g, ')$outdegree <- degree(', g, ', mode = "out")'))) # no. of outgoing ties
  eval(parse(text = paste0('V(', g, ')$closeness <- centralization.closeness(', g, ')$res'))) # central nodes = lower total distance from all other nodes
  eval(parse(text = paste0('V(', g, ')$betweenness <- centralization.betweenness(', g, ')$res'))) # number of times node acts as a bridge along the shortest path between two other nodes.
  eval(parse(text = paste0('V(', g, ')$eigen <- centralization.evcent(', g, ')$vector'))) # measure of the influence of a node in a network
  eval(parse(text = paste0('V(', g, ')$constraint <- constraint(', g, ')'))) # constraint is higher if ego has less, or mutually stronger related (more redundant) contacts.
  }

# copy standard network statistics to dataframe
