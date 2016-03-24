
library(sna)

# set working directory

# setwd("~/ownCloud/Innovation Network Analysis/Case studies/HF") # MacBook
# setwd("d:/Andrew/ownCloud/Innovation Network Analysis/Case studies/HF") # Home PC
setwd("c:/Users/ter053/ownCloud/Innovation Network Analysis/Case studies/HF") # work PC

# load saved sna objects

graph.list <- c("knowledge.provider.net.sna", "tacit.knowledge.net.sna", "explicit.knowledge.net.sna", 
                "idea.generation.net.sna", "idea.realisation.net.sna", "affect.based.trust.net.sna", 
                "cognition.based.trust.net.sna", "prior.relationship.net.sna", "report.to.net.sna")

for (i in graph.list){
  eval(parse(text = paste0('load("', g,'.rda")')))
}

# combine sna objects

g <- array(dim = c(8,18,18))

g[1,,] <- tacit