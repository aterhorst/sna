#####################################################
#                                                   #
#    R script to convert igraph network objects     #
#             into sna network objects              #
#                                                   #
#####################################################


# pre-process data if required

# source_url("https://raw.githubusercontent.com/aterhorst/sna/master/pre_process.R", sha1 = NULL) # pre-process data

# load libraries

library(intergraph) # required to convert igraph objects into network objects used in statnet
library(sna)

# unload igraph library

source_url("https://raw.githubusercontent.com/aterhorst/sna/master/detach.R", sha1 = NULL) # invoke detach function
detach_package("igraph", TRUE)

# set working directory

## Case 1

# setwd("~/ownCloud/Innovation Network Analysis/Quantitative Data/Case 1") # MacBook
setwd("d:/Andrew/ownCloud/Innovation Network Analysis/Quantitative Data/case 1") # Home PC
# setwd("c:/Users/ter053/ownCloud/Innovation Network Analysis/Quantitative Data/case 1") # work PC

## Case 2

#setwd("~/ownCloud/Innovation Network Analysis/Quantitative Data/Case 2") # MacBook
# setwd("d:/Andrew/ownCloud/Innovation Network Analysis/Quantitative Data/Case 2") # Home PC
# setwd("c:/Users/ter053/ownCloud/Innovation Network Analysis/Quantitative Data/Case 2") # work PC

## Case 3

# setwd("~/ownCloud/Innovation Network Analysis/Quantitative Data/Case 3") # MacBook
# setwd("d:/Andrew/ownCloud/Innovation Network Analysis/Quantitative Data/Case 3") # Home PC
# setwd("c:/Users/ter053/ownCloud/Innovation Network Analysis/Quantitative Data/Case 3") # work PC


# load and convert igraph objects saved as .rda files

graph.list <- c("knowledge.provider.net", "tacit.knowledge.provider.net", "explicit.knowledge.provider.net", 
                "idea.generation.net", "idea.realisation.net", "affect.based.trust.net", 
                "cognition.based.trust.net", "prior.relationship.net", "report.to.net")

for (g in graph.list){
  eval(parse(text = paste0('load("', g,'.rda")')))
  eval(parse(text = paste0(g,'.sna <- asNetwork(', g,')')))
}
# write converted graph objects to files

graph.list <- c("knowledge.provider.net.sna", "tacit.knowledge.provider.net.sna", "explicit.knowledge.provider.net.sna", 
                "idea.generation.net.sna", "idea.realisation.net.sna", "affect.based.trust.net.sna", 
                "cognition.based.trust.net.sna", "prior.relationship.net.sna", "report.to.net.sna")

for (g in graph.list){
   eval(parse(text = paste0('save(', g, ', file = "', g,'.rda")'))) # save as R data file
}

