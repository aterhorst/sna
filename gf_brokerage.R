# set working directory

# Case study 1

# setwd("~/ownCloud/Innovation Network Analysis/Case studies/HF") # MacBook
# setwd("d:/Andrew/ownCloud/Innovation Network Analysis/Case studies/HF") # Home PC
setwd("c:/Users/ter053/ownCloud/Innovation Network Analysis/Case studies/HF") # work PC

# Case study 2

# setwd("~/ownCloud/Innovation Network Analysis/Case studies/AMR") # MacBook
# setwd("d:/Andrew/ownCloud/Innovation Network Analysis/Case studies/AMR") # Home PC
# setwd("c:/Users/ter053/ownCloud/Innovation Network Analysis/Case studies/AMR") # work PC

library(network)
library(sna)


graph.list <- c("knowledge.provider.net.sna", "explicit.knowledge.net.sna", "tacit.knowledge.net.sna", "idea.generation.net.sna", "idea.realisation.net.sna")

for (g in graph.list){
  eval(parse(text = paste0('load("', g,'.rda")')))
}


for (g in graph.list){
  eval(parse(text = paste0('gf.', g,' <- brokerage(', g,', knowledge.provider.net.sna%v%"employer")')))
}
