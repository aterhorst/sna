#####################################################
#                                                   #
#          R script to export network and           #
#      attribute data for ingestion into MPNet      #
#                                                   #
#####################################################

# check libraries

library(igraph)
library(devtools)
library(MASS)

# pre-process data

source_url("https://raw.githubusercontent.com/aterhorst/sna/master/pre_process.R", sha1 = NULL) # create graphs

# create list of graphs to be exported

graph_list <- c("knowledge_net", "tacit_knowledge_net", "explicit_knowledge_net", 
                "idea_net", "real_net", "affect_trust_net", "cog_trust_net", "prior_net", 
                "report_to_net")

# generate adjacency matrices

for (g in graph_list){
  eval(parse(text = paste0('adj_', g, ' <- get.adjacency(', g, ', type = "both", names = FALSE)')))
  eval(parse(text = paste0('write.matrix(adj_', g, ', file = "', g,'.txt")'))) # write out data file
}

# generate node attribute tables

continuous_data <- subset(node_summary, select = c(4,9:22)) # select columns with continuous data
write.table(continuous_data, "continuous_data.txt", row.names = FALSE, col.names = TRUE, sep = "\t", quote = FALSE)

categorical_data <- subset(node_summary, select = c(5:8,24)) # select columns with categorical data
write.table(categorical_data, "categorical_data.txt", row.names = FALSE, col.names = TRUE, sep = "\t", quote = FALSE)

binary_data <- subset(node_summary, select = c(3))
write.table(binary_data, "binary_data.txt", row.names = FALSE, col.names = TRUE, sep = "\t", quote = FALSE)


