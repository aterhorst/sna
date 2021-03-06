#####################################################
#                                                   #
#          R script to export network and           #
#      attribute data for ingestion into MPNet      #
#               Version 2016-09-23                  #
#                                                   #
#####################################################

# Load requisite libraries.

library(igraph)
library(devtools)
library(MASS)
library(Matrix)

# Pre-process data if necessary. Be aware of working directory. 

# source_url("https://raw.githubusercontent.com/aterhorst/sna/master/pre_process.R", sha1 = NULL) # create graphs.

# Delete vertices with NA values (remove isolates). Condition on two attributes: age, work location.

graph.list <- c("knowledge.provider.net", "tacit.knowledge.net", "explicit.knowledge.net", 
                "idea.contributor.net", "idea.transformer.net", "affect.based.trust.net", 
                "cognition.based.trust.net", "prior.relationship.net", "report.to.net")

for(g in graph.list){
  eval(parse(text = paste0(g, ' <- delete.vertices(', g,', is.na(V(',g,')$age) | is.na(V(',g,')$work.location))')))
  }

# Create and export adjacency matrix for each network.

for (g in graph.list){
  eval(parse(text = paste0('adj.', g, ' <- get.adjacency(', g, ', type = "both", names = FALSE)')))
  eval(parse(text = paste0('write.matrix(adj.', g, ', file = "', g,'.txt")'))) # write out data file
}

# Generate node attribute files.

continuous.data <- na.omit(subset(node.summary, select = c(age, work.experience,current.job.tenure, education.level,
                                                   personality.openness, personality.conscientiousness,
                                                   personality.agreeableness, self.efficacy, identification.collab,
                                                   identification.org, identification.group,
                                                   controlled.motivation, autonomous.motivation))) # select columns with continuous data
write.table(continuous.data, "continuous_data.txt", row.names = FALSE, col.names = TRUE, sep = "\t", quote = FALSE)

categorical.data <- na.omit(subset(node.summary, select = c(work.location, broad.education.field, 
                                                            occupation.class, employer))) # select columns with categorical data
write.table(categorical.data, "categorical_data.txt", row.names = FALSE, col.names = TRUE, sep = "\t", quote = FALSE)

binary.data <- na.omit(subset(node.summary, select = c(gender)))
write.table(binary.data, "binary_data.txt", row.names = FALSE, col.names = TRUE, sep = "\t", quote = FALSE)

# create dyadic covariate file.

fn <- "dyadic_covariates.txt" 
cat("", file = fn) # create empty file

for (g in graph.list){
  eval(parse(text = paste0('df.', g, ' <- as.data.frame(as.matrix(adj.', g,'))')))
  eval(parse(text = paste0('cat("', g,'\n", file = fn, append = TRUE)')))
  eval(parse(text = paste0('write.table(df.', g,', fn, append = TRUE, sep = "\t", row.names = FALSE, col.names = FALSE)')))
}

# End.