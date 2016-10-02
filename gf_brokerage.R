#####################################################
#                                                   #
#               R script to perform                 #
#        Gould-Fernandez brokerage analysis         #
#               Version 2016-10-01                  #
#                                                   #
#####################################################

# Load requisite libraries.

library(network)
library(sna)
library(intergraph) 
library(devtools)
library(ggplot2)
library(dplyr)
library(reshape)
library(ggthemes)


# Set working directory.

## Case 1.

# setwd("~/ownCloud/Innovation Network Analysis/Quantitative Data/HF") # MacBook
# setwd("d:/Andrew/ownCloud/Innovation Network Analysis/Quantitative Data/HF") # Home PC
# setwd("c:/Users/ter053/ownCloud/Innovation Network Analysis/Quantitative Data/HF") # work PC

# Case 2.

# setwd("~/ownCloud/Innovation Network Analysis/Quantitative Data/AMR") # MacBook
# setwd("d:/Andrew/ownCloud/Innovation Network Analysis/Quantitative Data/AMR") # Home PC
# setwd("c:/Users/ter053/ownCloud/Innovation Network Analysis/Quantitative Data/AMR") # work PC

# Case 3.

# setwd("~/ownCloud/Innovation Network Analysis/Quantitative Data/GIHH") # MacBook
setwd("d:/Andrew/ownCloud/Innovation Network Analysis/Quantitative Data/GIHH") # Home PC
# setwd("c:/Users/ter053/ownCloud/Innovation Network Analysis/Quantitative Data/GIHH") # work PC

# Load pre-processed data saved as .rda files.

# Load and convert igraph objects saved as .rda files.

graph.list.1 <- c("knowledge.provider.net", "tacit.knowledge.net", "explicit.knowledge.net", 
                "idea.generation.net", "idea.realisation.net", "affect.based.trust.net", 
                "cognition.based.trust.net", "prior.relationship.net", "report.to.net")

for (g in graph.list.1){
  eval(parse(text = paste0('load("', g,'.rda")')))
  eval(parse(text = paste0(g,'.sna <- asNetwork(', g,')')))
}

# Write converted graph objects to .rda files.

graph.list.2 <- c("knowledge.provider.net.sna", "tacit.knowledge.net.sna", "explicit.knowledge.net.sna", 
                "idea.generation.net.sna", "idea.realisation.net.sna", "affect.based.trust.net.sna", 
                "cognition.based.trust.net.sna", "prior.relationship.net.sna", "report.to.net.sna")

for (g in graph.list.2){
  eval(parse(text = paste0('save(', g, ', file = "', g,'.rda")'))) # save as R data file
}

# Perform G-F analysis.

graph.list.3 <- c("knowledge.provider.net.sna", "explicit.knowledge.net.sna", "tacit.knowledge.net.sna", "idea.generation.net.sna")

for (g in graph.list.3){
  eval(parse(text = paste0('gf.', g,' <- brokerage(', g,', knowledge.provider.net.sna%v%"employer")')))
}



# Synthesize data.

source_url("https://gist.githubusercontent.com/dfalster/5589956/raw/5f9cb9cba709442a372c2e7621679a5dd9de1e28/addNewData.R", sha1 = NULL)
allowedVars <- c("employer")

kp <- as.data.frame(gf.knowledge.provider.net.sna$raw.nli[,1:5])
kp$name <- rownames(kp)
kp$net <- "all knowledge"
rownames(kp) <- NULL
kp <- addNewData("lookupTable.csv", kp, allowedVars) # add descriptive fields

ekp <- as.data.frame(gf.explicit.knowledge.net.sna$raw.nli[,1:5])
ekp$name <- rownames(ekp)
ekp$net <- "explicit component > 50%"
rownames(ekp) <- NULL
ekp <- addNewData("lookupTable.csv", ekp, allowedVars) # add descriptive fields

tkp <- as.data.frame(gf.tacit.knowledge.net.sna$raw.nli[,1:5])
tkp$name <- rownames(tkp)
tkp$net <- "tacit component > 50%"
rownames(tkp) <- NULL
tkp <- addNewData("lookupTable.csv", tkp, allowedVars) # add descriptive fields

ig <- as.data.frame(gf.idea.generation.net.sna$raw.nli[,1:5])
ig$name <- rownames(ig)
ig$net <- "idea generation"
rownames(ig) <- NULL
ig <- addNewData("lookupTable.csv", ig, allowedVars) # add descriptive fields

gf <- rbind(kp,ekp,tkp,ig) # summary of gould-fernandez

write.csv(gf, file = "gould-fernandez.csv", row.names = FALSE)

# Plot graphs.

melted <- melt(gf, id.vars = c("name","employer","net"))
melted$net <- factor(melted$net, levels = c("all knowledge", "explicit component > 50%", "tacit component > 50%", "idea generation"))


ggplot(melted, aes(variable,value,fill = name)) +
  geom_bar(stat="identity") +
  facet_grid(.~net) +
  theme_fivethirtyeight() +
  theme(legend.position="none")
  


