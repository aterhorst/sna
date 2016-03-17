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
library(Matrix)

# pre-process data

source_url("https://raw.githubusercontent.com/aterhorst/sna/master/pre_process.R", sha1 = NULL) # create graphs

# create list of graphs to be exported

graph.list <- c("knowledge.provider.net", "tacit.knowledge.net", "explicit.knowledge.net", 
                "idea.generation.net", "idea.realisation.net", "affect.based.trust.net", 
                "cognition.based.trust.net", "prior.relationship.net", "report.to.net")

# create and export adjacency matrix for each network

for (g in graph.list){
  eval(parse(text = paste0('adj.', g, ' <- get.adjacency(', g, ', type = "both", names = FALSE)')))
  eval(parse(text = paste0('write.matrix(adj.', g, ', file = "', g,'.txt")'))) # write out data file
}

# generate node attribute tables

continuous.data <- subset(node.summary, select = c(age,work.experience,current.job.tenure,
                                                   personality.openness, personality.conscientiousness,
                                                   personality.agreeableness, job.competence, job.autonomy,
                                                   creative.self.efficacy, amotivation, extrinsic.regulation.social,
                                                   extrinsic.regulation.material,introjected.regulation,
                                                   identified.regulation, intrinsic.motivation)) # select columns with continuous data
write.table(continuous.data, "continuous_data.txt", row.names = FALSE, col.names = TRUE, sep = "\t", quote = FALSE)

categorical.data <- subset(node.summary, select = c(work.location, education.level, education.field,
                                                    occupation.class)) # select columns with categorical data
write.table(categorical.data, "categorical_data.txt", row.names = FALSE, col.names = TRUE, sep = "\t", quote = FALSE)

binary.data <- subset(node.summary, select = c(gender))
write.table(binary.data, "binary_data.txt", row.names = FALSE, col.names = TRUE, sep = "\t", quote = FALSE)

# create dyadic covariate file

fn <- "dyadic_covariates.txt" 
cat("", file = fn) # create empty file

for (g in graph.list){
  eval(parse(text = paste0('df.', g, ' <- as.data.frame(as.matrix(adj.', g,'))')))
  eval(parse(text = paste0('cat("', g,'\n", file = fn, append = TRUE)')))
  eval(parse(text = paste0('write.table(df.', g,', fn, append = TRUE, sep = "\t", row.names = FALSE, col.names = FALSE)')))
}