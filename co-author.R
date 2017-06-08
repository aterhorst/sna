library(readxl)
library(igraph)
library(plyr)
library(dplyr)
library(stringr)
library(purrr)


setwd("D:/Andrew/ownCloud/Co-author Network")

# read in CSIRO people data

groups <- read_excel("people_places.xlsx")
groups <- subset(groups, select = c("FullName","UserName", "BusinessUnitCode", "LocationCode"))
groups$BusinessUnitCode <- as.integer(groups$BusinessUnitCode)

org <- read_excel("org_units.xlsx")
org <- subset(org, select = c("DepartmentCode", "Abbreviation", "LineOfBusiness"))
colnames(org)[colnames(org)=="DepartmentCode"] <- "BusinessUnitCode"
org$BusinessUnitCode <- as.integer(org$BusinessUnitCode)


groups <- full_join(groups,org, by = "BusinessUnitCode")
extract <- c("IS", "NF")
groups <- filter(groups, LineOfBusiness %in% extract)
groups <- cbind(id = 1:nrow(groups), groups)

# import publication data

man <- na.omit(read_excel("ePublish Manuscripts.xls"))
man$`Publisher Notification Date` <- as.Date(man$`Publisher Notification Date`)

# extract data range

man_2012_13 <- filter(man, man$`Publisher Notification Date` >= "2012-01-01" & man$`Publisher Notification Date` <= "2013-12-31")

# create ragged edge dataframe

aut_2012_13 <- str_split_fixed(man_2012_13$Author, ";", n = 21)

# reverse lastname, firstname

rev_name <- function(string, pattern = ", ") {paste(rev(unlist(strsplit(string, pattern))), collapse = " ")} 

aut_2012_13 <- aut_2012_13 %>%  as_data_frame %>%  map_df(map, rev_name) %>%  map_df(unlist)

# trim to 20 co-authors max

aut_2012_13 <- as.data.frame(aut_2012_13[,1:20])


# match authors to groups

nauthors <- t(apply(aut_2012_13, 1, function(x) match(x, groups$FullName)))

# generate a clean list of co-authors

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

snet <- graph.edgelist(links, directed=FALSE) 

# generate attribute info

bu <- factor(groups$BusinessUnitCode[as.numeric(V(snet)$name)])
loc <- factor(groups$LocationCode[as.numeric(V(snet)$name)])
nme <- factor(groups$FullName[as.numeric(V(snet)$name)])

# assign attributes to vertices

V(snet)$author <- as.character(nme)
V(snet)$bu <- as.character(div)
V(snet)$location <- as.character(loc)

# export as gml file

write.graph(snet, "co-author.gml", "gml")

