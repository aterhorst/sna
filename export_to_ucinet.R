#####################################################
#                                                   #
#          R script to do export networks           #
#              into UCINet dl format                #
#                 Version 20170728                  #
#                                                   #
#####################################################





# restart R

.rs.restartR()
rm(list=ls())

# load libraries

library(igraph)
library(tnet)

# set working directory

setwd("~/ownCloud/Innovation Network Analysis/Quantitative Data/Case 2")

# load and convert igraph objects saved as .rda files

graph.list <- c("knowledge.provider.net", "tacit.knowledge.net", "explicit.knowledge.net", 
                "idea.generation.net", "idea.realisation.net", "affect.based.trust.net", 
                "cognition.based.trust.net", "prior.relationship.net", "report.to.net")

for (g in graph.list){
  eval(parse(text = paste0('load("', g,'.rda")')))
}

# convert to tnet format

net <- get.edgelist(tacit.knowledge.net, names=FALSE)
if(ncol(net)==2)
  net <- cbind(net, 1)
net <- as.tnet(net, type="weighted one-mode tnet")

# export to ucinet

tnet_ucinet(net,"tacit_knowledge")
