
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
  eval(parse(text = paste0('load("', i,'.rda")')))
  eval(parse(text = paste0(i,' <- as.sociomatrix.sna(',i,')')))
}

# combine sna objects

g <- array(dim = c(7,18,18))

g[1,,] <- knowledge.provider.net.sna
g[2,,] <- idea.generation.net.sna
g[3,,] <- idea.realisation.net.sna
g[4,,] <- affect.based.trust.net.sna
g[5,,] <- cognition.based.trust.net.sna
g[6,,] <- prior.relationship.net.sna
g[7,,] <- report.to.net.sna

# run qap test

q.12 <- qaptest(g, gcor, g1 = 1, g2 =2)

nl <- netlm(knowledge.provider.net.sna, g, mode = "digraph", nullhyp = "qap")
summary(nl)
