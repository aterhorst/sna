#####################################################
#                                                   #
#      R script to generate igraph networks         #
#  from raw .xlsx file downloaded from onasurveys   #
#               Version 2016-10-23                  #      
#                                                   #
#####################################################

library(plyr)
library(dplyr)
library(readxl)
library(igraph)
library(rgexf)
library(devtools) # so we can use source_url

# Set working directory.

## Case 1

# setwd("~/ownCloud/Innovation Network Analysis/Quantitative Data/Case 1") # MacBook
# setwd("d:/Andrew/ownCloud/Innovation Network Analysis/Quantitative Data/case 1") # Home PC
# setwd("c:/Users/ter053/ownCloud/Innovation Network Analysis/Quantitative Data/case 1") # work PC

## Case 2

#setwd("~/ownCloud/Innovation Network Analysis/Quantitative Data/Case 2") # MacBook
setwd("d:/Andrew/ownCloud/Innovation Network Analysis/Quantitative Data/Case 2") # Home PC
# setwd("c:/Users/ter053/ownCloud/Innovation Network Analysis/Quantitative Data/Case 2") # work PC

## Case 3

# setwd("~/ownCloud/Innovation Network Analysis/Quantitative Data/Case 3") # MacBook
setwd("d:/Andrew/ownCloud/Innovation Network Analysis/Quantitative Data/Case 3") # Home PC
# setwd("c:/Users/ter053/ownCloud/Innovation Network Analysis/Quantitative Data/Case 3") # work PC


# Import survey data.

## Read in node data from onasurvey downloaded workbook.

nodes <- read_excel("surveydata.xlsx", sheet = 1)
fn <- "temp.csv"
write.csv(nodes, file = fn, row.names = FALSE)
nodes <- read.csv(fn)
if (file.exists(fn)) file.remove(fn) # clean up garbage

## Read in relationships from onasurvey downloaded workbook.

edge.all <- read_excel("surveydata.xlsx", sheet = 2) 


# Clean up node data. Extract numeric values, rename attributes.

nodes$Age <- as.numeric(gsub("([0-9]*).*","\\1",nodes$Age)) # extract years only
nodes$Experience <- as.numeric(gsub("([0-9]*).*","\\1",nodes$Experience)) # extract years only
nodes$Tenure <- as.numeric(gsub("([0-9]*).*","\\1",nodes$Tenure)) # extract years only
nodes <- plyr::rename(nodes, c("Gender"="gender", "Age" = "age", "Location" = "work.location", 
                               "Education" = "education.level", "BroadEducationField" = "broad.education.field", 
                               "Occupation1" = "occupation.class", "Occupation" = "occupation.class", 
                               "Experience" = "work.experience", "Tenure" = "current.job.tenure", "Identity1" = "identification.org",
                               "Identity2" = "identification.group", "Identity3" = "identification.collab"))

nodes$education.level <- as.numeric(nodes$education.level)

# Totalise scale items.

## Reverse score specific survey items.

nodes$Openness2 <- 10 - nodes$Openness2 # reverse openness scale item 2
nodes$Conscientiousness1 <- 10 - nodes$Conscientiousness1 # reverse conscientious scale item 1
nodes$Agreeableness2 <- 10 - nodes$Agreeableness2 # reverse agreeableness scale item 2

## Aggregate survey items. Rescale aggregated items between 0 and 1.

nodes$personality.openness <- round((rowMeans(subset(nodes, select = c(Openness1,Openness2)), na.rm = TRUE)-1)/9, digits = 2) # openness
nodes$personality.conscientiousness <- round((rowMeans(subset(nodes, select = c(Conscientiousness1,Conscietiousness2)), na.rm = TRUE)-1)/9, digits = 2) # consceintiousness
nodes$personality.agreeableness <- round((rowMeans(subset(nodes, select = c(Agreeableness1,Agreeableness2)), na.rm = TRUE)-1)/9, digits = 2) # agreeableness
nodes$job.competence <- round((rowMeans(subset(nodes, select = c(Competence1,Competence2,Competence3)), na.rm = TRUE)-1)/9, digits = 2) # job competence
nodes$self.determination <- round((rowMeans(subset(nodes, select = c(SelfDetermination1,SelfDetermination2,SelfDetermination3)), na.rm = TRUE)-1)/9, digits = 2) # self determination
nodes$creative.self.efficacy <- round((rowMeans(subset(nodes, select = c(Creativity1,Creativity2,Creativity3,Creativity4)), na.rm = TRUE)-1)/9, digits = 2) # creativie self-efficacy
nodes$amotivation <- round((rowMeans(subset(nodes, select = c(Amotivation1,Amotivation2,Amotivation3)), na.rm = TRUE)-1)/9, digits = 2) # amotivation
nodes$extrinsic.regulation.social <- round((rowMeans(subset(nodes, select = c(ExtrinsicRegulationSocial1,ExtrinsicRegulationSocial2,ExtrinsicRegulationSocial3)), na.rm = TRUE)-1)/9, digits = 2) # extrinsic regulation - social
nodes$extrinsic.regulation.material <- round((rowMeans(subset(nodes, select = c(ExtrinsicRegulationMaterial1,ExtrinsicRegulationMaterial2,ExtrinsicRegulationMaterial3)), na.rm = TRUE)-1)/9, digits = 2) # extrinsic regulation material
nodes$introjected.regulation <- round((rowMeans(subset(nodes, select = c(IntrojectedRegulation1,IntrojectedRegulation2,IntrojectedRegulation3,IntrojectedRegulation4)), na.rm = TRUE)-1)/9, digits = 2) # introjected regulation
nodes$identified.regulation <- round((rowMeans(subset(nodes, select = c(IdentifiedRegulation1,IdentifiedRegulation2,IdentifiedRegulation3)), na.rm = TRUE)-1)/9, digits = 2) # identified regulation
nodes$intrinsic.motivation <- round((rowMeans(subset(nodes, select = c(IntrinsicMotivation1,IntrinsicMotivation2,IntrinsicMotivation3)), na.rm = TRUE)-1)/9, digits = 2) # intrinsic motivation
nodes$identification.org <- round((nodes$identification.org - 1)/9, digits = 2) # identification with organisation
nodes$identification.group <- round((nodes$identification.group - 1)/9, digits = 2) # identification with group
nodes$identification.collab <- round((nodes$identification.collab - 1)/9, digits = 2) # identification with collaboration

## Summarise aggregated survey items.

nodes$controlled.motivation <- round(rowMeans(subset(nodes, select = c(extrinsic.regulation.material,extrinsic.regulation.social,introjected.regulation))), digits = 2)
nodes$autonomous.motivation <- round(rowMeans(subset(nodes, select = c(identified.regulation,intrinsic.motivation))), digits = 2)
nodes$self.efficacy <- nodes$controlled.motivation <- round(rowMeans(subset(nodes, select = c(self.determination,job.competence,creative.self.efficacy))), digits = 2)

## Remove unwanted columns now that we have aggregated/summarised scores.

node.summary <- subset(nodes, select=-c(3:4,13:16,18:34,36:49,51,55:63)) # drop unwanted columns using column numbers
node.summary$vertex.id <- node.summary$id # duplicate id for future labelling purposes.

## Add employer organisation using look-up table.

source_url("https://gist.githubusercontent.com/dfalster/5589956/raw/5f9cb9cba709442a372c2e7621679a5dd9de1e28/addNewData.R", sha1 = NULL)
allowedVars <- c("employer")
node.summary <- addNewData("lookupTable.csv", node.summary, allowedVars) # add descriptive fields

# Generate knowledge provider ties.

## Extract knowledge provider data.

edge.knowledge <- filter(edge.all, relationship_set_knowledge_sharing == 1) # extract knowledge provider ties
edge.knowledge[11:13] <- lapply(edge.knowledge[11:13], as.numeric) # convert to numeric values
edge.knowledge$Codified <- 10 - edge.knowledge$Codified # reverse score level of documented knowledge
edge.knowledge$tacit <- round((rowMeans(subset(edge.knowledge, select = c(Codified,Complexity,Observability), na.rm = TRUE))-1)/9, digits = 2) # compute level of tacitness between 0 and 1
edge.knowledge <- subset(edge.knowledge, select = c(from, to, tacit)) # purge unwanted columns - knowledge sharing edge list

## Filter edge lists by level of tacitness.

edge.tacit.knowledge <- filter(edge.knowledge, tacit > 0.5) # filter predominantly tacit knowledge sharing ties
edge.explicit.knowledge <- filter(edge.knowledge, tacit < 0.5)

# Generate knowledge provider graphs from ties, nodes (directionality must still be fixed). 

knowledge.provider.net <- graph.data.frame(edge.knowledge, node.summary, directed = TRUE)
tacit.knowledge.net <- graph.data.frame(edge.tacit.knowledge, node.summary, directed = TRUE)
explicit.knowledge.net <- graph.data.frame(edge.explicit.knowledge, node.summary, directed = TRUE)

# Generate other edge lists.

edge.idea.contributor <- filter(edge.all, relationship_set_idea_generation == 1) # extract idea generation with ties
edge.idea.contributor <- subset(edge.idea.contributor, select = c(from, to)) # purge unwanted columns
edge.idea.transformer <- filter(edge.all, relationship_set_idea_realisation == 1) # extract idea realisation with ties
edge.idea.transformer <- subset(edge.idea.transformer, select = c(from, to)) # purge unwanted columns
edge.affect.based.trust <- filter(edge.all, relationship_set_affectbased_trust == 1) # extract affect-based trust ties
edge.affect.based.trust <- subset(edge.affect.based.trust, select = c(from, to)) # purge unwanted columns
edge.cognition.based.trust <- filter(edge.all, relationship_set_cognitionbased_trust == 1) # extract cognition-based trust ties
edge.cognition.based.trust <- subset(edge.cognition.based.trust, select = c(from, to)) # purge unwanted columns
edge.prior.relationship <- filter(edge.all, relationship_set_prior_relationships == 1) # extract prior relationship with ties
edge.prior.relationship <- subset(edge.prior.relationship, select = c(from, to)) # purge unwanted columns, select = c(from, to)) # purge unwanted columns
edge.report.to <- filter(edge.all, relationship_set_managers == 1) # extract manager/supervisor ties
edge.report.to <- subset(edge.report.to, select = c(from, to)) # purge unwanted columns

# Generate other graphs.

idea.contributor.net <- graph.data.frame(edge.idea.contributor, node.summary, directed = TRUE) # ideation network
idea.transformer.net <- graph.data.frame(edge.idea.transformer, node.summary, directed = TRUE) # idea realisation network 
affect.based.trust.net <- graph.data.frame(edge.affect.based.trust, node.summary, directed = TRUE) # affect-based trust network
cognition.based.trust.net <- graph.data.frame(edge.cognition.based.trust, node.summary, directed = TRUE) # cognition-based trust network
prior.relationship.net <- graph.data.frame(edge.prior.relationship, node.summary, directed = TRUE) # prior relationships network
report.to.net <- graph.data.frame(edge.report.to, node.summary, directed = TRUE) # manager network

# Reverse direction of ties.

source_url("https://raw.githubusercontent.com/aterhorst/sna/master/reverse_direction.R", sha1 = NULL) # function to reverse ties
knowledge.provider.net <- graph.reverse(knowledge.provider.net) # fix direction of knowledge provider ties
tacit.knowledge.net <- graph.reverse(tacit.knowledge.net) # fix direction of knowledge provider ties
explicit.knowledge.net <- graph.reverse(explicit.knowledge.net)
idea.contributor.net <- graph.reverse(idea.contributor.net)


# Simplify graphs (remove multiple edges).

graph.list <- c("knowledge.provider.net", "tacit.knowledge.net", "explicit.knowledge.net", 
                "idea.contributor.net", "idea.transformer.net", "affect.based.trust.net", 
                "cognition.based.trust.net", "prior.relationship.net", 
                "report.to.net")

for (g in graph.list){
  eval(parse(text = paste0(g, ' <- simplify(', g,', remove.multiple = FALSE, remove.loops = TRUE)')))
}

# Compute standard network statistics for each network.

for (g in graph.list){
  eval(parse(text = paste0('V(', g, ')$degree <- degree(', g, ', mode = "all")'))) # no. of ties
  eval(parse(text = paste0('V(', g, ')$in.degree <- degree(', g, ', mode = "in")'))) # no. of incoming ties
  eval(parse(text = paste0('V(', g, ')$out.degree <- degree(', g, ', mode = "out")'))) # no. of outgoing ties
  eval(parse(text = paste0('V(', g, ')$closeness.centrality <- centralization.closeness(', g, ')$res'))) # central nodes = lower total distance from all other nodes
  eval(parse(text = paste0('V(', g, ')$betweenness.centrality <- centralization.betweenness(', g, ')$res'))) # number of times node acts as a bridge along the shortest path between two other nodes.
  eval(parse(text = paste0('V(', g, ')$eigen.vector.centrality <- centralization.evcent(', g, ')$vector'))) # measure of the influence of a node in a network
  eval(parse(text = paste0('V(', g, ')$constraint <- constraint(', g, ')'))) # higher the constraint, the fewer the opportunities to broker
}

# Replace NaN values in constraint measures.

for (g in graph.list){
  eval(parse(text = paste0('V(', g, ')$constraint <- replace(V(', g, ')$constraint,is.nan(V(', g, ')$constraint),1)')))
}

# Write pre-processed data to files.

## Create and write out GEPHI files.

for (g in graph.list){
  eval(parse(text = paste0(g,'.gexf <- igraph.to.gexf(', g,')')))
  eval(parse(text = paste0('f <- file("', g,'.gexf")')))
  eval(parse(text = paste0('writeLines(', g,'.gexf$graph, con = f)')))
  eval(parse(text = paste0('on.exit(close(f))')))
}

## Write out .csv and .rda files.

for (g in graph.list){
  eval(parse(text = paste0(g,'.vertex.attributes <- get.vertex.attribute(', g,')')))
  eval(parse(text = paste0('write.csv(', g,'.vertex.attributes, file = "', g,'.csv", row.names = FALSE)')))
  eval(parse(text = paste0('save(', g, ', file = "', g,'.rda")'))) # save as R data file
}

# End.