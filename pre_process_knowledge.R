#####################################################
#                                                   #
#     R script to generate knowledge provider       #
#     adjacency matrix for ingestion into PNet      #
#                                                   #
#####################################################

library(plyr)
library(dplyr)
library(readxl)
library(igraph)
library(MASS)
library(devtools)

# set working directory

# setwd("~/ownCloud/Innovation Network Analysis/Case studies/HF") # MacBook
setwd("d:/Andrew/ownCloud/Innovation Network Analysis/Case studies/HF") # Home PC

# import data

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
# generate knowledge provider ties



edge_all <- read_excel("surveydata.xlsx", sheet = 2) # read in relationships sheet from onasurvey downloaded workbook
edge_knowledge <- filter(edge_all, relationship_set_knowledge_sharing == 1) # extract knowledge provider ties

edge_knowledge[11:13] <- lapply(edge_knowledge[11:13], as.numeric)
edge_knowledge$Codified <- 10 - edge_knowledge$Codified # reverse score level of documented knowledge
edge_knowledge$tacit <- round((rowMeans(subset(edge_knowledge, select = c(Codified,Complexity,Observability), na.rm = TRUE))-1)/9, digits = 2) # compute level of tacitness between 0 and 1
edge_knowledge <- subset(edge_knowledge, select = c(from, to, tacit)) # purge unwanted columns - knowledge sharing edge list

edge_tacit_knowledge <- filter(edge_knowledge, tacit >= 0.5) # filter predominantly tacit knowledge sharing ties
edge_explicit_knowledge <- filter(edge_knowledge, tacit < 0.5) # filter predominantly explicit knowledge sharing ties

# generate graph from ties, nodes

knowledge_net <- graph.data.frame(edge_knowledge, node_summary, directed = TRUE)
tacit_knowledge_net <- graph.data.frame(edge_tacit_knowledge, node_summary, directed = TRUE)
explicit_knowledge_net <- graph.data.frame(edge_explicit_knowledge, node_summary, directed = TRUE)

# reverse direction of ties 

source_url("https://raw.githubusercontent.com/aterhorst/sna/master/reverse_direction.R") # function to reverse ties
knowledge_net <- graph.reverse(knowledge_net) # fix direction of knowledge provider ties
tacit_knowledge_net <- graph.reverse(tacit_knowledge_net) # fix direction of knowledge provider ties
explicit_knowledge_net <- graph.reverse(explicit_knowledge_net) # fix direction of knowledge provider ties

# simplify graphs

knowledge_net <- simplify(knowledge_net, remove.multiple = FALSE, remove.loops = TRUE)
tacit_knowledge_net <- simplify(tacit_knowledge_net, remove.multiple = FALSE, remove.loops = TRUE)
explicit_knowledge_net <- simplify(explicit_knowledge_net, remove.multiple = FALSE, remove.loops = TRUE)

# export graphs

write.graph(knowledge_net, "knowledge_net.gml", "gml") # use in gephi or visone
