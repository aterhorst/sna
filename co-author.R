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
library(ggmap)
library(MASS)
library(Matrix)


setwd("/OSM/MEL/DPS_OI_Network/work/ownCloud/Co-author Network")

# read in CSIRO people data

## people info

groups_now <- read_excel("data/people_places.xlsx") # extracted from PeopleServ.csiro.au
groups_old <- read.csv("data/people_2013.csv", as.is = c(TRUE,FALSE), header = TRUE)

## compute level in current hierachy

reportto <- as.data.frame(subset(groups_now, select = c("ManagerPersonnelNumber", "PersonnelNumber"))) # report to dyads
g <- graph_from_data_frame(reportto)
rank <- data.frame(PersonnelNumber = names(shortest.paths(g)[,'00022937']), RankHierarchy = shortest.paths(g)[,'00022937']+1) # 00022937 is CEO
rank$PersonnelNumber <- as.character(rank$PersonnelNumber)
groups_now <- full_join(groups_now,rank, by = "PersonnelNumber")

## extract cohort that has survived since 2013

groups <- groups_now[which(groups_now$FullName %in% groups_old$name),]


## remove garbage columns from groups

groups <- subset(groups, select = c("FullName", "PersonnelNumber", "BusinessUnitCode", "LocationCode","WorkAreaCode", "RankHierarchy"))
groups$BusinessUnitCode <- as.integer(groups$BusinessUnitCode)

## add organisational detail

org <- read_excel("data/org_units.xlsx") # extracted from OrgUnitServ.csiro.au
org <- subset(org, select = c("DepartmentCode", "Name", "LineOfBusiness"))
colnames(org)[colnames(org) == "DepartmentCode"] <- "BusinessUnitCode"
org$BusinessUnitCode <- as.integer(org$BusinessUnitCode)

## add work location info

loc <- read_excel("data/csiro_location.xlsx") # extracted from LocationServ.csiro.au
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

## create gecode field

groups$CountryPostCode <- with(groups, paste0(Country, " postcode ", PostCode))

# import publication data

man <- na.omit(read_excel("data/ePublish Manuscripts.xls")) # extracted from ePublish.csiro.au
man$`Publisher Notification Date` <- as.Date(man$`Publisher Notification Date`)

# identify productivity stars

prod1 <- filter(man, man$`Publisher Notification Date` >= "2014-01-01" & man$`Publisher Notification Date` <= "2016-12-31")
prod2 <- str_split_fixed(man$Author, ";", n = 160) # max number of co-authors

rev_name <- function(string, pattern = ", ") {paste(rev(unlist(strsplit(string, pattern))), collapse = " ")} 

prod2 <- prod2 %>% as_data_frame %>% map_df(map, rev_name) %>%  map_df(unlist)
prod2 <- as.data.frame(t(apply(prod2,1,function(x) gsub("+ "," ",x)))) # fix double white space
prod2 <- as.data.frame(t(apply(prod2,1,function(x) gsub(" +"," ",x)))) # fix leading white space

prod2 <- prod2[!is.na(prod2)]

prod3 <- as.data.frame(table(prod2))
colnames(prod3)[colnames(prod3) == "prod2"] <- "FullName"
prod3$FullName <- as.character(prod3$FullName)
groups <- left_join(groups,prod3, by = "FullName")


# extract date range(s) - need to repeat everything from here on for each date range.

man <- filter(man, man$`Publisher Notification Date` >= "2016-01-01" & man$`Publisher Notification Date` <= "2016-12-31")

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

buc <- factor(groups$BusinessUnitCode[as.numeric(V(g)$name)])
bun <- factor(groups$Name[as.numeric(V(g)$name)])
loc <- factor(groups$LocationCode[as.numeric(V(g)$name)])
workplace <- factor(groups$WorkAreaCode[as.numeric(V(g)$name)])
coauthor <- factor(groups$FullName[as.numeric(V(g)$name)])
orgrank <- factor(groups$RankHierarchy[as.numeric(V(g)$name)])
pn <- factor(groups$PersonnelNumber[as.numeric(V(g)$name)])
prod <- factor(groups$Freq[as.numeric(V(g)$name)])
pc <- factor(groups$CountryPostCode[as.numeric(V(g)$name)])

# assign attributes to vertices

V(g)$employee <- as.character(coauthor) # name of co-author
V(g)$employee_id <- as.character(pn) # person identifier
V(g)$bu <- as.character(bun) # bu name
V(g)$bu_id <- as.character(buc) # bu code
V(g)$location_id <- as.character(loc) # place of work
V(g)$building_id <- as.character(workplace) # building 
V(g)$geocode <- as.character(pc) # geocode variable
V(g)$org_rank <- as.numeric(as.character(orgrank)) # level in hierarchy
V(g)$prod <- as.numeric(as.character(prod)) # total publications 2013-16
V(g)$degree <- degree(g)
V(g)$closeness <- closeness(g)
V(g)$betweenness <- betweenness(g)
V(g)$evcent <- evcent(g)$vector
V(g)$constraint <- constraint(g) # burt's constraint measure
V(g)$evbrokerage <- ifelse(betweenness(g) != 0, 
                           ((betweenness(g)*2)+(vcount(g)-1))/degree(g), 
                           betweenness(g)) # everett-valente brokerage

# sanity check (check aut and nauthor to see things make sense with what igraph reports below)

ego(g,1,nodes = V(g)$author == "Stuart Day", "all") # see immediate alters connected to ego - first check
ego(g,1,nodes = V(g)$author == "Raphaele Blanchi", "all") # ditto - second check

# Extract vertex attributes 

metrics <- as.data.frame(get.vertex.attribute(g), stringsAsFactors = F)

# Get geographic coordinates.

name.place <- metrics[,c(1,2,8)]

name.place$coordinate <- geocode(name.place$geocode, sensor = FALSE, output = "latlon", source = "google")

dat <- as.data.frame(as.list(name.place[,c(2,5)]))

# Following code courtesy of Bangyou Zheng:

sphericalDistance <- function (lat1, lon1, lat2, lon2)
{
  lon1 <- lon1 * pi/180
  lat1 <- lat1 * pi/180
  lon2 <- lon2 * pi/180
  lat2 <- lat2 * pi/180
  dLat <- lat2 - lat1
  dLon <- lon1 - lon2
  a <- sin(dLat/2) * sin(dLat/2) + cos(lat1) * cos(lat2) *
    sin(dLon/2) * sin(dLon/2)
  c <- 2 * atan2(sqrt(a), sqrt(1 - a))
  d <- 6371 * c
  return(d)
}

edge.dat <- dat %>%
  # Create two new columns with the same ids
  mutate(id1 = id, id2 = id) %>%
  # expand into all combinations of names
  expand(id1, id2) %>%
  # Remove name1 equals to name2 2
  filter(id1 != id2) %>%
  # Merge the original data.frame for lon and lat in column name1
  left_join(dat, by = c('id1' = 'id')) %>%
  rename(lon1 = coordinate.lon, lat1 = coordinate.lat) %>%
  # Merge the original data.frame for lon and lat in column name2
  left_join(dat, by = c('id2' = 'id')) %>%
  rename(lon2 = coordinate.lon, lat2 = coordinate.lat) %>%
  # Calculate the distance
  mutate(distance = sphericalDistance(lat1, lon1, lat2, lon2))

# Create distance matrix.

edge.dat <- subset(edge.dat, select = c(id1, id2, distance)) 

# Compute log distance.

edge.dat$log.dist <- log1p(edge.dat$distance) 

# Create network object using iGraph.

edge.net <- graph.data.frame(edge.dat, directed = T)



# plotting

## extract giant component

cl <- clusters(g) 
gc <- induced.subgraph(g, which(cl$membership == which.max(cl$csize)))

## configure display parameters

bus <- factor(V(g)$bu_id) 
n <- max(unlist(as.integer(bus))) 
gc() # garbage collection
col.scale <- randomColor(n, hue = "random", luminosity = "bright") 

V(gc)$size <- V(gc)$prod/max(V(gc)$prod) * 5 # productivity stars
V(gc)$size <- V(gc)$evbrokerage/max(V(gc)$evbrokerage) * 5 # relational stars
V(gc)$size <- (V(gc)$evbrokerage/max(V(gc)$evbrokerage))*(V(gc)$prod/max(V(gc)$prod))*10 # super stars

legend <- factor(V(gc)$bu_id)

lo <- layout_with_kk(gc)

## generate graph

pdf("co-author_2016r.pdf",width=15,height=15) #call the pdf writer

plot(gc, vertex.color = col.scale[bus], 
     vertex.label=NA, 
     vertex.size=V(gc)$size, edge.width=0.8, layout= lo)

title(main = "Reltional Stars\n2016", cex.main=2)
legend("topright",legend=levels(legend),col=col.scale[bus], pch = 16, cex=0.8, title = "Business Unit", box.lty=0)
# box(lty = 'solid', lwd = box_line,  col = 'black')
text(-1, 1.00, labels = paste0('nodes = ', vcount(g)), adj = c(0,0), cex = 0.8)
text(-1, 0.975, labels = paste0('edges = ', ecount(g)), adj = c(0,0), cex = 0.8)
text(-1, 0.95, labels = paste0('density = ', round(edge_density(g),4)), adj = c(0,0), cex = 0.8)
text(-1, 0.925, labels = paste0('assortativity = ', round(assortativity_nominal(g,V(g)$bu_id),3)), adj = c(0,0), cex = 0.8)

dev.off() #close the device


# export vertex attributes

write.csv(metrics, "2016_vertex_attr.csv", row.names = F)

# save as.RDA file

save(g, file = "2016_co-author_net.rda")

# export as gml file to display in Gephi

write.graph(g, "2013_co-author.gml", "gml")


# export to MPNet

adjmatrix <- get.adjacency(g)
write.matrix(adjmatrix, file = "2016_coauthor_adj_mpnet.txt")

actor_attributes <- as.data.frame(get.vertex.attribute(g))

continuous_dat <- subset(actor_attributes, select = c("org_rank", "prod"))
categorical_dat <- subset(actor_attributes, select = c("bu_id", "location_id","building_id"))
write.table(continuous_dat, "2016_continuous_data.txt", row.names = FALSE, col.names = TRUE, sep = "\t", quote = FALSE)
write.table(categorical_dat, "2016_categorical_data.txt", row.names = FALSE, col.names = TRUE, sep = "\t", quote = FALSE)


