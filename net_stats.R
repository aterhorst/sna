#####################################################
#                                                   #
#            R script to generate standard          #
#                 networks statistics               #
#                                                   #
#####################################################

library(igraph)
library(devtools)
library(gtools)

# set working directory

# MAC <-c("~/ownCloud/Innovation Network Analysis/Case studies/HF") # MacBook
# PC <- c("d:/Andrew/ownCloud/Innovation Network Analysis/Case studies/HF") # Home PC
# 
# choice <- ask("Which computer are you using (PC = 1, MAC = 2): ")
# choice <- as.numeric(choice)
# 
# if (choice == 1) eval(parse(text = paste0('setwd("', PC,'")'))) else eval(parse(text = paste0('setwd("', MAC,'")')))


# pre-process data

source_url("https://raw.githubusercontent.com/aterhorst/sna/master/pre_process.R", sha1 = NULL)

# create list of graphs to be crunched

graph_list <- c("knowledge.provider.net", "tacit.knowledge.provider.net", "explicit.knowledge.provider.net", 
           "idea.generation.net", "idea.realisation.net", "affect.based.trust.net", "cognition.based.trust.net", "prior.relationship.net", 
           "report.to.net")

# compute standard network statistics for each network 

for (g in graph_list){
  eval(parse(text = paste0('V(', g, ')$comm <- membership(optimal.community(', g,'))'))) # modularity score
  eval(parse(text = paste0('V(', g, ')$degree <- degree(', g, ', mode = "all")'))) # no. of ties
  eval(parse(text = paste0('V(', g, ')$indegree <- degree(', g, ', mode = "in")'))) # no. of incoming ties
  eval(parse(text = paste0('V(', g, ')$outdegree <- degree(', g, ', mode = "out")'))) # no. of outgoing ties
  eval(parse(text = paste0('V(', g, ')$closeness <- centralization.closeness(', g, ')$res'))) # central nodes = lower total distance from all other nodes
  eval(parse(text = paste0('V(', g, ')$betweenness <- centralization.betweenness(', g, ')$res'))) # number of times node acts as a bridge along the shortest path between two other nodes.
  eval(parse(text = paste0('V(', g, ')$eigen <- centralization.evcent(', g, ')$vector'))) # measure of the influence of a node in a network
  eval(parse(text = paste0('V(', g, ')$constraint <- constraint(', g, ')'))) # higher the constraint, the fewer the opportunities to broker
  eval(parse(text = paste0('node_att_', g, ' <- vertex.attributes(', g,')'))) # record stats into new data frame
  eval(parse(text = paste0('write.csv(node_att_', g, ', "node_att_', g,'.csv")'))) # write out csv file
  }


# compute assortativity coefficient for each network 

assortativity <- data.frame("network" = character(),"assort_coeff" = numeric(), stringsAsFactors = FALSE)  # create empty data frame

for (i in graph_list){
  eval(parse(text = paste0('assort <- round(assortativity.nominal(', i,', factor(V(', i,')$Org), directed = TRUE), digits = 4)'))) # calculate assortativity
  assortativity[nrow(assortativity) + 1,] <- c(i,assort) # add row
  }

# export data to csv

write.csv(assortativity, "assortativity.csv")
write.csv(node_summary, "nodesummary.csv")



