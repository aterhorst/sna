#################################################
#                                               #
#     R script to create co-author networks     #
#             Version 2017-06-09                #
#                                               #
#################################################

library(readxl)
library(igraph)
library(plyr)
library(dplyr)
library(stringr)
library(purrr)
library(randomcoloR)
library(reshape)
library(MASS)
library(Matrix)


setwd("/OSM/MEL/DPS_OI_Network/work/ownCloud/Co-author Network")

# read in CSIRO people data

## people info

groups <- read_excel("people_places.xlsx") # extracted from PeopleServ.csiro.au

## compute level in hierachy

reportto <- as.data.frame(subset(groups, select = c("ManagerPersonnelNumber", "PersonnelNumber"))) # report to dyads
g <- graph_from_data_frame(reportto)
rank <- data.frame(PersonnelNumber = names(shortest.paths(g)[,'00022937']), RankHierarchy = shortest.paths(g)[,'00022937']+1) # 00022937 is CEO
rank$PersonnelNumber <- as.character(rank$PersonnelNumber)
groups <- full_join(groups,rank, by = "PersonnelNumber")

## remove garbage columns from groups

groups <- subset(groups, select = c("FullName","ManagerName", "PersonnelNumber", "BusinessUnitCode", "LocationCode", "RankHierarchy"))
groups$BusinessUnitCode <- as.integer(groups$BusinessUnitCode)

## add organisational detail

org <- read_excel("org_units.xlsx") # extracted from OrgUnitServ.csiro.au
org <- subset(org, select = c("DepartmentCode", "Name", "LineOfBusiness"))
colnames(org)[colnames(org) == "DepartmentCode"] <- "BusinessUnitCode"
org$BusinessUnitCode <- as.integer(org$BusinessUnitCode)

## add work location info

loc <- read_excel("csiro_location.xlsx") # extracted from LocationServ.csiro.au
loc <- subset(loc, select = c("Code","City","Country","PostCode"))
colnames(loc)[colnames(loc) == "Code"] <- "LocationCode"

## combine people, org, location info

groups <- left_join(groups,org, by = "BusinessUnitCode")
groups <- left_join(groups,loc, by = "LocationCode")

## focus on science areas (core business)

extract <- c("IS", "NF") # science areas
groups <- filter(groups, LineOfBusiness %in% extract) # subset according to science areas
groups <- cbind(id = 1:nrow(groups), groups) # index rows

## fix crappy bu names

groups$Name[groups$Name == "CSIRO ASTRONOMY & SPACE SCIENCE"] <- "ASTRONOMY & SPACE SCIENCE"
groups$Name[groups$Name == "NATL COLLECTIONS & MARINE INFRASTRUCTURE"] <- "NATIONAL COLLECTIONS & MARINE INFRASTRUCTURE"


# import publication data

man <- na.omit(read_excel("ePublish Manuscripts.xls")) # extracted from ePublish.csiro.au
man$`Publisher Notification Date` <- as.Date(man$`Publisher Notification Date`)

# extract date range(s) - need to repeat everything from here on for each date range.

man <- filter(man, man$`Publisher Notification Date` >= "2013-01-01" & man$`Publisher Notification Date` <= "2013-12-31")

# create ragged edge dataframe

aut <- str_split_fixed(man$Author, ";", n = 160) # max number of co-authors

# reverse lastname, firstname (thanks to Alex Whan)

rev_name <- function(string, pattern = ", ") {paste(rev(unlist(strsplit(string, pattern))), collapse = " ")} 
aut <- aut %>% as_data_frame %>% map_df(map, rev_name) %>%  map_df(unlist)
aut <- as.data.frame(t(apply(aut,1,function(x) gsub("+ "," ",x)))) # fix double white space
aut <- as.data.frame(t(apply(aut,1,function(x) gsub(" +"," ",x)))) # fix leading white space

# trim to 20 co-authors max (arbitrary limit)

aut <- as.data.frame(aut[,1:20])

# create dyads

## match authors to groups (thanks to Alec Stephenson)

nauthors <- t(apply(aut, 1, function(x) match(x, groups$FullName)))

## generate a clean list of co-authors

ind <- which(!is.na(t(nauthors)),arr.ind=TRUE)[,2:1]
colnames(ind) <- c("row","col")
trow <- table(ind[,"row"]) # counts number of co-authors per paper
trow1 <- as.numeric(names(trow[trow==1])) # remove single authors
ind <- ind[!(ind[,"row"] %in% trow1),] # is a clean list
ind <- cbind(ind, author = nauthors[ind]) # assign numeric value representing authors and co-authors (from nauthors)
links <- tapply(ind[,"author"], ind[,"row"], function(x) t(combn(x, 2))) # generate list of dyads per paper
links <- do.call(rbind, links) # generate global list of dyads
links <- t(apply(links,1,sort)) # order each dyad low to high (first sort)
links <- links[order(links[,1],links[,2]),]  # order dyads according to increasing nauthor (second sort)
links <- links[!duplicated(links),] # take out duplicate dyads (co-authors with more than one paper)
links <- matrix(as.character(links), ncol=2) # convert dyads to edge matrix

# undirected graph generated from edge matrix

require(igraph)
g <- graph.edgelist(links, directed=FALSE) 

# generate attribute info

bu_code <- factor(groups$BusinessUnitCode[as.numeric(V(g)$name)])
bu_name<- factor(groups$Name[as.numeric(V(g)$name)])
location_id <- factor(groups$LocationCode[as.numeric(V(g)$name)])
co_author <- factor(groups$FullName[as.numeric(V(g)$name)])
org_rank <- factor(groups$RankHierarchy[as.numeric(V(g)$name)])

# assign attributes to vertices

V(g)$author <- as.character(co_author) # name of co-author
V(g)$buname <- as.character(bu_name) # bu name
V(g)$bucode <- as.character(bu_code) # bu code
V(g)$location <- as.character(location_id) # place of work
V(g)$rank <- as.character(org_rank) # level in hierarchy

# sanity check (check aut and nauthor to see things make sense with what igraph reports below)

ego(g,1,nodes = V(g)$author == "Stuart Day", "all") # see immediate alters connected to ego - first check
ego(g,1,nodes = V(g)$author == "Raphaele Blanchi", "all") # ditto - second check

# plotting

## extract giant component

cl <- clusters(g) 
gc <- induced.subgraph(g, which(cl$membership == which.max(cl$csize)))

## configure display parameters

bus <- factor(V(g)$buname) 
n <- max(unlist(as.integer(bu_name))) 
gc() # garbage collection
col.scale <- randomColor(n, hue = "random", luminosity = "bright") 
V(gc)$size <- degree(gc)/5
legend <- factor(V(gc)$buname)
lo <- layout_with_fr(gc)

## generate graph

pdf("co-author.pdf",width=15,height=15) #call the pdf writer

plot(gc, vertex.color = col.scale[bus], 
     vertex.label=NA, 
     vertex.size=V(gc)$size, edge.width=0.8, layout= lo)

title(main = "CSIRO co-authorship network 2013",cex.main=2)
legend(0.55,1.1,legend=levels(legend),col=col.scale[bus], pch = 16, cex=0.8)
dev.off() #close the device


# node statistics

## centrality 
metrics <- data.frame(
  name = V(g)$author,
  bu = V(g)$bucode,
  loc = V(g)$location,
  deg = degree(g), # degree
  btw = betweenness(g), # betweenness 
  clo = closeness(g), # closeness
  eig = evcent(g)$vector) # eigenvector centrality
  
## homophily

# work in progress. idea is to calculate heterogeneity (diversity).

# export as gml file to display in Gephi

write.graph(g, "co-author.gml", "gml")
write.graph(g, "pajek.txt", "pajek")


# export to MPNet

adjmatrix <- get.adjacency(g)
write.matrix(adjmatrix, file = "coauthor_adj_mpnet.txt")

actor_attributes <- as.data.frame(get.vertex.attribute(g))

continuous_dat <- subset(actor_attributes, select = "rank")
categorical_dat <- subset(actor_attributes, select = c("bu", "location"))
write.table(continuous_dat, "continuous_data.txt", row.names = FALSE, col.names = TRUE, sep = "\t", quote = FALSE)
write.table(categorical_dat, "categorical_data.txt", row.names = FALSE, col.names = TRUE, sep = "\t", quote = FALSE)



