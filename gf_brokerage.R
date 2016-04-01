# set working directory

# Case study 1

setwd("~/ownCloud/Innovation Network Analysis/Case studies/HF") # MacBook
# setwd("d:/Andrew/ownCloud/Innovation Network Analysis/Case studies/HF") # Home PC
setwd("c:/Users/ter053/ownCloud/Innovation Network Analysis/Case studies/HF") # work PC

# Case study 2

setwd("~/ownCloud/Innovation Network Analysis/Case studies/AMR") # MacBook
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



# synthesize data

source_url("https://gist.githubusercontent.com/dfalster/5589956/raw/5f9cb9cba709442a372c2e7621679a5dd9de1e28/addNewData.R", sha1 = NULL)
allowedVars <- c("employer")

kp <- as.data.frame(gf.knowledge.provider.net.sna$raw.nli[,1:5])
kp$name <- rownames(kp)
kp$net <- "all knowledge"
rownames(kp) <- NULL
kp <- addNewData("lookupTable.csv", kp, allowedVars) # add descriptive fields

ekp <- as.data.frame(gf.explicit.knowledge.net.sna$raw.nli[,1:5])
ekp$name <- rownames(ekp)
ekp$net <- "explicit knowledge"
rownames(ekp) <- NULL
ekp <- addNewData("lookupTable.csv", ekp, allowedVars) # add descriptive fields

tkp <- as.data.frame(gf.tacit.knowledge.net.sna$raw.nli[,1:5])
tkp$name <- rownames(tkp)
tkp$net <- "tacit knowledge"
rownames(tkp) <- NULL
tkp <- addNewData("lookupTable.csv", tkp, allowedVars) # add descriptive fields

ig <- as.data.frame(gf.idea.generation.net.sna$raw.nli[,1:5])
ig$name <- rownames(ig)
ig$net <- "idea generation"
rownames(ig) <- NULL
ig <- addNewData("lookupTable.csv", ig, allowedVars) # add descriptive fields

ir <- as.data.frame(gf.idea.realisation.net.sna$raw.nli[,1:5])
ir$name <- rownames(ir)
ir$net <- "idea realisation"
rownames(ir) <- NULL
ir <- addNewData("lookupTable.csv", ir, allowedVars) # add descriptive fields

gf <- rbind(kp,ekp,tkp,ig,ir)

# plot graphs


ggplot(as.data.frame(gf.knowledge.provider.net.sna$raw.nli), 
       aes(x=factor(gf.knowledge.provider.net.sna$raw.nli[,1:5])))+
  geom_bar()


b <- c("w_I","w_O","b_IO","b_OI","b_O")

par(mfrow = c(5,1), mar = c(2, 1, 2, 1), oma = c(2,2,4,2) )

barplot(gf.knowledge.provider.net.sna$raw.nli[,b], 
        col = "black", ylim=c(0,200))
title("brokerage of general knowledge", line = -1)

barplot(gf.explicit.knowledge.net.sna$raw.nli[,b], 
        col = "black", ylim=c(0,100))
title("brokerage of predominantly explicit knowledge", line = -1)

barplot(gf.tacit.knowledge.net.sna$raw.nli[,b], 
        col = "black", ylim=c(0,100))
title("brokerage of predominantly tacit knowledge", line = -1)

barplot(gf.idea.generation.net.sna$raw.nli[,b], 
        col = "black", ylim=c(0,100))
title("brokerage of ideas", line = -1)

barplot(gf.idea.realisation.net.sna$raw.nli[,b], 
        col = "black", ylim=c(0,100))
title("brokerage of solutions", line = -1)

title("CASE STUDY 1", outer = TRUE, line = 2, cex.main = 2)