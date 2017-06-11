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
library(ggmap)
library(RColorBrewer)
library(reshape)
library(MASS)
library(Matrix)


setwd("/OSM/MEL/DPS_OI_Network/work/ownCloud/Co-author Network")

# read in CSIRO people data

## people info

groups <- read_excel("people_places.xlsx") # extracted from PeopleServ.csiro.au
groups <- subset(groups, select = c("FullName","UserName", "BusinessUnitCode", "LocationCode"))
groups$BusinessUnitCode <- as.integer(groups$BusinessUnitCode)
colnames(groups)[colnames(groups) == "UserName"] <- "Ident"

## organisational detail

org <- read_excel("org_units.xlsx") # extracted from OrgUnitServ.csiro.au
org <- subset(org, select = c("DepartmentCode", "Abbreviation", "LineOfBusiness"))
colnames(org)[colnames(org) == "DepartmentCode"] <- "BusinessUnitCode"
org$BusinessUnitCode <- as.integer(org$BusinessUnitCode)

## work location

loc <- read_excel("csiro_location.xlsx") # extracted from LocationServ.csiro.au
loc <- subset(loc, select = c("Code","City","Country","PostCode"))
colnames(loc)[colnames(loc) == "Code"] <- "LocationCode"

## combine people, org, location info

groups <- full_join(groups,org, by = "BusinessUnitCode")
groups <- full_join(groups,loc, by = "LocationCode")
extract <- c("IS", "NF") # science areas
groups <- filter(groups, LineOfBusiness %in% extract) # subset according to science areas
groups <- cbind(id = 1:nrow(groups), groups) # index rows

## geocode locations

# groups$location <- with(groups, paste0(Country," postcode ", PostCode))
# groups$geocode <- geocode(groups$location, sensor = FALSE, output = "latlon", source = "google")

# import publication data

man <- na.omit(read_excel("ePublish Manuscripts.xls")) # extracted from ePublish.csiro.au
man$`Publisher Notification Date` <- as.Date(man$`Publisher Notification Date`)

# extract data range

man <- filter(man, man$`Publisher Notification Date` >= "2013-01-01" & man$`Publisher Notification Date` <= "2013-12-31")

# create ragged edge dataframe

aut <- str_split_fixed(man$Author, ";", n = 160) # max number of co-authors

# reverse lastname, firstname (thanks to Alex Whan)

rev_name <- function(string, pattern = ", ") {paste(rev(unlist(strsplit(string, pattern))), collapse = " ")} 
aut <- aut %>%  as_data_frame %>%  map_df(map, rev_name) %>%  map_df(unlist)
aut <- as.data.frame(t(apply(aut,1,function(x) gsub("+ "," ",x)))) # fix double white space
aut <- as.data.frame(t(apply(aut,1,function(x) gsub(" +"," ",x)))) # fix leading white space

# trim to 20 co-authors max (arbitrary limit)

aut <- as.data.frame(aut[,1:20])

# # individual publications stats
# 
# simplify <- melt(aut, id.vars=0)
# pubtots <- simplify %>% group_by(value) %>% summarise(count = n())
# colnames(pubtots)[colnames(pubtots) == "value"] <- "FullName"
# colnames(pubtots)[colnames(pubtots) == "count"] <- "Publications"
# pubtots$FullName <- as.character(pubtots$FullName)
# groups <- left_join(groups,pubtots, by = "FullName")

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

g <- graph.edgelist(links, directed=FALSE) 

# generate attribute info

bu <- factor(groups$BusinessUnitCode[as.numeric(V(g)$name)])
place <- factor(groups$LocationCode[as.numeric(V(g)$name)])
person <- factor(groups$FullName[as.numeric(V(g)$name)])
# prod <- factor(groups$Publications[as.numeric(V(g)$name)])

# assign attributes to vertices

V(g)$author <- as.character(person) # name of person
V(g)$bu <- as.character(bu) # bu name
V(g)$location <- as.character(place) # place of work
# V(g)$publications <- as.character(prod) # how many publications produced in period

# sanity check

ego(g,1,nodes = V(g)$author == "Stuart Day", "all") # check against nauthors
ego(g,1,nodes = V(g)$author == "Raphaele Blanchi", "all")

# export as gml file

write.graph(g, "co-author.gml", "gml")

# plotting

## extract giant component

cl <- clusters(g) 
gc <- induced.subgraph(g, which(cl$membership == which.max(cl$csize)))

## configure display parameters

V(gc)$size <- degree(gc)/5
div <- factor(V(gc)$bu)
cols <- c("yellow","orange","red","darkred","blue","darkblue","darkgreen","black","lightblue","violet","pink")

## generate graph

pdf("co-author.pdf",width=15,height=15) #call the pdf writer
plot(gc,vertex.color = cols[as.numeric(bu)], vertex.label=NA, vertex.size=V(gc)$size, edge.width=0.8, layout=layout.kamada.kawai)
# title(main = "CSIRO co-authorship network 2011-12",cex.main=2.5)
legend(1,1,legend=levels(div),col=cols, pch = 16, cex=1.25)
dev.off() #close the device

# export to MPNet

adjmatrix <- get.adjacency(g)
write.matrix(adjmatrix, file = "coauthor_adj_mpnet.txt")

actor_attributes <- as.data.frame(get.vertex.attribute(g))

# continuous_dat <- subset(actor_attributes, select = "publications")
categorical_dat <- subset(actor_attributes, select = c("bu", "location"))
# write.table(continuous_dat, "continuous_data.txt", row.names = FALSE, col.names = TRUE, sep = "\t", quote = FALSE)
write.table(categorical_dat, "categorical_data.txt", row.names = FALSE, col.names = TRUE, sep = "\t", quote = FALSE)



