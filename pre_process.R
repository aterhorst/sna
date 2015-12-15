#####################################################
#                                                   #
#      R script to generate networks from raw       #
#       xlsx files downloaded from onasurveys       #
#                                                   #
#####################################################

library(plyr)
library(dplyr)
library(readxl)
library(igraph)
library(MASS)
library(devtools)

# import data

setwd("~/ownCloud/Innovation Network Analysis/Case studies/HF") # MacBook
# setwd("d:/Andrew/ownCloud/Innovation Network Analysis/Case studies/HF") # Home PC


## import nodes

nodes <- read_excel("surveydata.xlsx", sheet = 1)
fn <- "temp.csv"
write.csv(nodes, file = fn, row.names = FALSE)
nodes <- read.csv(fn)
if (file.exists(fn)) file.remove(fn) # clean up garbage

## fix nodes

nodes$Age <- as.numeric(gsub("([0-9]*).*","\\1",nodes$Age)) # extract years only
nodes$Experience <- as.numeric(gsub("([0-9]*).*","\\1",nodes$Experience)) # extract years only
nodes$Tenure <- as.numeric(gsub("([0-9]*).*","\\1",nodes$Tenure)) # extract years only
colnames(nodes)[10] <- "Occupation" # correct mislabeled column 

## totalize scale items

### reverse specific survey items
nodes$Openness2 <- 10 - nodes$Openness2 # reverse openness scale item 2
nodes$Conscientiousness1 <- 10 - nodes$Conscientiousness1 # reverse conscientious scale item 1
nodes$Agreeableness2 <- 10 - nodes$Agreeableness2 # reverse agreeableness scale item 2

### aggregate survey items. Rescale aggregated items between 0 and 1.

nodes$Openness <- round((rowMeans(subset(nodes, select = c(Openness1,Openness2)), na.rm = TRUE)-1)/9, digits = 2) # openness
nodes$Conscietiousness <- round((rowMeans(subset(nodes, select = c(Conscientiousness1,Conscietiousness2)), na.rm = TRUE)-1)/9, digits = 2) # consceintiousness
nodes$Agreeableness <- round((rowMeans(subset(nodes, select = c(Agreeableness1,Agreeableness2)), na.rm = TRUE)-1)/9, digits = 2) # agreeableness
nodes$Competence <- round((rowMeans(subset(nodes, select = c(Competence1,Competence2,Competence3)), na.rm = TRUE)-1)/9, digits = 2) # job competence
nodes$SelfDetermination <- round((rowMeans(subset(nodes, select = c(SelfDetermination1,SelfDetermination2,SelfDetermination3)), na.rm = TRUE)-1)/9, digits = 2) # self determination
nodes$Creativity <- round((rowMeans(subset(nodes, select = c(Creativity1,Creativity2,Creativity3,Creativity4)), na.rm = TRUE)-1)/9, digits = 2) # creativie self-efficacy
nodes$Amotivation <- round((rowMeans(subset(nodes, select = c(Amotivation1,Amotivation2,Amotivation3)), na.rm = TRUE)-1)/9, digits = 2) # amotivation
nodes$ExtrinsicRegulationSocial <- round((rowMeans(subset(nodes, select = c(ExtrinsicRegulationSocial1,ExtrinsicRegulationSocial2,ExtrinsicRegulationSocial3)), na.rm = TRUE)-1)/9, digits = 2) # extrinsic regulation - social
nodes$ExtrinsicRegulationMaterial <- round((rowMeans(subset(nodes, select = c(ExtrinsicRegulationMaterial1,ExtrinsicRegulationMaterial2,ExtrinsicRegulationMaterial3)), na.rm = TRUE)-1)/9, digits = 2) # extrinsic regulation material
nodes$IntrojectedRegulation <- round((rowMeans(subset(nodes, select = c(IntrojectedRegulation1,IntrojectedRegulation2,IntrojectedRegulation3,IntrojectedRegulation4)), na.rm = TRUE)-1)/9, digits = 2) # introjected regulation
nodes$IdentifiedRegulation <- round((rowMeans(subset(nodes, select = c(IdentifiedRegulation1,IdentifiedRegulation2,IdentifiedRegulation3)), na.rm = TRUE)-1)/9, digits = 2) # identified regulation
nodes$IntrinsicMotivation <- round((rowMeans(subset(nodes, select = c(IntrinsicMotivation1,IntrinsicMotivation2,IntrinsicMotivation3)), na.rm = TRUE)-1)/9, digits = 2) # intrinsic motivation

## remove unwanted columns now we have totalized scores

# column_names <- as.data.frame(names(nodes)) # get numbered list of columns
# column_names # lists column names with corresponding column numbers

node_summary <- subset(nodes, select=-c(3:4,13:51)) # drop unwanted columns using column numbers
node_summary$label <- as.character(node_summary$id)

# add organisation

source_url("https://gist.githubusercontent.com/dfalster/5589956/raw/5f9cb9cba709442a372c2e7621679a5dd9de1e28/addNewData.R", sha1 = NULL)
allowedVars <- c("Org")
node_summary <- addNewData("lookupTable.csv", node_summary, allowedVars) # add descriptive fields

# col_names <- as.data.frame(names(node_summary))
# col_names
# head(node_summary[,c(2,23,24)],18)

# generate knowledge provider ties

edge_all <- read_excel("surveydata.xlsx", sheet = 2) # read in relationships sheet from onasurvey downloaded workbook
edge_knowledge <- filter(edge_all, relationship_set_knowledge_sharing == 1) # extract knowledge provider ties

edge_knowledge[11:13] <- lapply(edge_knowledge[11:13], as.numeric)
edge_knowledge$Codified <- 10 - edge_knowledge$Codified # reverse score level of documented knowledge
edge_knowledge$tacit <- round((rowMeans(subset(edge_knowledge, select = c(Codified,Complexity,Observability), na.rm = TRUE))-1)/9, digits = 2) # compute level of tacitness between 0 and 1
edge_knowledge <- subset(edge_knowledge, select = c(from, to, tacit)) # purge unwanted columns - knowledge sharing edge list

edge_tacit_knowledge <- filter(edge_knowledge, tacit >= 0.5) # filter predominantly tacit knowledge sharing ties
edge_explicit_knowledge <- filter(edge_knowledge, tacit < 0.5) # filter predominantly explicit knowledge sharing ties

# generate knowledge provider graph from ties, nodes

knowledge_net <- graph.data.frame(edge_knowledge, node_summary, directed = TRUE)
tacit_knowledge_net <- graph.data.frame(edge_tacit_knowledge, node_summary, directed = TRUE)
explicit_knowledge_net <- graph.data.frame(edge_explicit_knowledge, node_summary, directed = TRUE)

# reverse direction of ties 

source_url("https://raw.githubusercontent.com/aterhorst/sna/master/reverse_direction.R", sha1 = NULL) # function to reverse ties
knowledge_net <- graph.reverse(knowledge_net) # fix direction of knowledge provider ties
tacit_knowledge_net <- graph.reverse(tacit_knowledge_net) # fix direction of knowledge provider ties
explicit_knowledge_net <- graph.reverse(explicit_knowledge_net) # fix direction of knowledge provider ties

# generate other edge lists

edge_ideation <- filter(edge_all, relationship_set_idea_generation == 1) # extract idea generation with ties
edge_ideation <- subset(edge_ideation, select = c(from, to)) # purge unwanted columns
edge_realisation <- filter(edge_all, relationship_set_idea_realisation == 1) # extract idea realisation with ties
edge_realisation <- subset(edge_realisation, select = c(from, to)) # purge unwanted columns
edge_affect_trust <- filter(edge_all, relationship_set_affectbased_trust == 1) # extract affect-based trust ties
edge_affect_trust <- subset(edge_affect_trust, select = c(from, to)) # purge unwanted columns
edge_cognition_trust <- filter(edge_all, relationship_set_cognitionbased_trust == 1) # extract cognition-based trust ties
edge_cognition_trust <- subset(edge_cognition_trust, select = c(from, to)) # purge unwanted columns
edge_prior_relationships <- filter(edge_all, relationship_set_prior_relationships == 1) # extract prior relationship with ties
edge_prior_relationships <- subset(edge_prior_relationships, select = c(from, to)) # purge unwanted columns, select = c(from, to)) # purge unwanted columns
edge_report_to <- filter(edge_all, relationship_set_managers == 1) # extract manager/supervisor ties
edge_report_to <- subset(edge_report_to, select = c(from, to)) # purge unwanted columns

# generate other graphs

idea_net <- graph.data.frame(edge_ideation, node_summary, directed = TRUE) # ideation network
real_net <- graph.data.frame(edge_realisation, node_summary, directed = TRUE) # idea realisation network 
affect_trust_net <- graph.data.frame(edge_affect_trust, node_summary, directed = TRUE) # affect-based trust network
cog_trust_net <- graph.data.frame(edge_cognition_trust, node_summary, directed = TRUE) # cognition-based trust network
prior_net <- graph.data.frame(edge_prior_relationships, node_summary, directed = TRUE) # prior relationships network
report_to_net <- graph.data.frame(edge_report_to, node_summary, directed = TRUE) # manager network

# create multiplex graph by stacking edge lists

merged_edges <- rbind(edge_knowledge[,1:2], edge_ideation, edge_realisation, edge_affect_trust, 
      edge_cognition_trust, edge_prior_relationships, edge_report_to)
multiplex <- graph.data.frame(merged_edges, directed = TRUE) 
multiplex <- simplify(multiplex, remove.multiple = FALSE, remove.loops = TRUE)
E(multiplex)$weight <- count.multiple(multiplex)

# simplify graphs

knowledge_net <- simplify(knowledge_net, remove.multiple = FALSE, remove.loops = TRUE)
tacit_knowledge_net <- simplify(tacit_knowledge_net, remove.multiple = FALSE, remove.loops = TRUE)
explicit_knowledge_net <- simplify(explicit_knowledge_net, remove.multiple = FALSE, remove.loops = TRUE)
idea_net <- simplify(idea_net, remove.multiple = FALSE, remove.loops = TRUE)
real_net <- simplify(real_net, remove.multiple = FALSE, remove.loops = TRUE)
affect_trust_net <- simplify(affect_trust_net, remove.multiple = FALSE, remove.loops = TRUE)
cog_trust_net <- simplify(cog_trust_net, remove.multiple = FALSE, remove.loops = TRUE)
prior_net <- simplify(prior_net, remove.multiple = FALSE, remove.loops = TRUE)
report_to_net <- simplify(report_to_net, remove.multiple = FALSE, remove.loops = TRUE)

# export graphs

# write.graph(knowledge_net, "knowledge_net.gml", "gml") # use in gephi or visone

