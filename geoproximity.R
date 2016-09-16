#####################################################
#                                                   #
#            R script to compute geographic         #
#             distance between people in a          #
#                       network                     #
#                                                   #
#####################################################



library(ggmap)
library(readxl)
library(sets)
library(dplyr)
library(tidyr)
library(ggplot2)
library(reshape2)
library(devtools)
library(heatmaply)
library(gplots)


# set working directory

setwd("~/ownCloud/Innovation Network Analysis/Case studies") # MacBook
# setwd("d:/Andrew/ownCloud/Innovation Network Analysis/Case studies/AMR") # Home PC
# setwd("c:/Users/ter053/ownCloud/Innovation Network Analysis/Case studies/AMR") # work PC

# import excel file

node.summary.all <- read_excel("node_summary_all.xlsx", sheet = 1)
node.summary.all$location <- with(node.summary.all, paste0(country," postcode ", post.code))

name.place <- node.summary.all[,c(1,3,30)]
name.place <- subset(name.place, name.place$case_no == 2)
name.place$coordinate <- geocode(name.place$location, sensor = FALSE, output = "latlon", source = "google")

dat <- as.data.frame(as.list(name.place[,c(2,4)]))

# following code courtesy of Bangyou Zheng:

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
  # Create two new columns with the same names
  mutate(name1 = name, name2 = name) %>%
  # expand into all combinations of names
  expand(name1, name2) %>%
  # Remove name1 equals to name2 2
  filter(name1 != name2) %>%
  # Merge the original data.frame for lon and lat in column name1
  left_join(dat, by = c('name1' = 'name')) %>%
  rename(lon1 = coordinate.lon, lat1 = coordinate.lat) %>%
  # Merge the original data.frame for lon and lat in column name2
  left_join(dat, by = c('name2' = 'name')) %>%
  rename(lon2 = coordinate.lon, lat2 = coordinate.lat) %>%
  # Calculate the distance
  mutate(distance = sphericalDistance(lat1, lon1, lat2, lon2))

# create distance matrix

edge.dat <- subset(edge.dat, select = c(name1, name2, distance)) 

edge.dat$local <- ifelse(edge.dat$distance < 50, 1, 0) # distance category
edge.dat$region <- ifelse(edge.dat$distance >= 50 & edge.dat$distance < 250, 1, 0) # distance category
edge.dat$national <- ifelse(edge.dat$distance >= 250 & edge.dat$distance < 2500, 1, 0) # distance category
edge.dat$international <- ifelse(edge.dat$distance >= 2500, 1, 0) # distance category

edge.net <- graph.data.frame(edge.dat, directed = T)

# create matrices

edge.matrix <- get.adjacency(edge.net, sparse = F, attr = "distance", type = "upper", names = F) # absolute geodistance

edge.matrix.local <- get.adjacency(edge.net, sparse = F, attr = "local", type = "upper", names = F)
edge.matrix.region <- get.adjacency(edge.net, sparse = F, attr = "region", type = "upper", names = F)
edge.matrix.national <- get.adjacency(edge.net, sparse = F, attr = "national", type = "upper", names = F)
edge.matrix.international <- get.adjacency(edge.net, sparse = F, attr = "international", type = "upper", names = F)


# write out proximity matrix

write.matrix(edge.matrix, file = "geoproximity.txt", sep = "\t")
write.matrix(edge.matrix.local, file = "close.proximity.txt", sep = "\t")
write.matrix(edge.matrix.region, file = "nearby.proximity.txt", sep = "\t")
write.matrix(edge.matrix.national, file = "far.proximity.txt", sep = "\t")
write.matrix(edge.matrix.international, file = "very.far.proximity.txt", sep = "\t")

# create heatmap

# use gplots


heatmap.2(
  edge.matrix,
  Rowv = F,
  Colv = F,
  symm = T,
  trace = "none",
  dendrogram = 'none', 
  scale = 'none',
  density.info = "none")

# use ggplot2

melted <- melt(edge.matrix)
n <- 25

ggplot(data = melted, aes(x=Var1, y=Var2, fill=value)) +
  geom_tile() +
  scale_x_continuous(breaks = c(1:n)) +
  scale_y_continuous(breaks = c(1:n)) +
  scale_fill_viridis(name="Spherical\nDistance (km)", begin = 0.1, end = 0.9) +
  theme(
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    panel.grid.major = element_blank(),
    panel.border = element_blank(),
    panel.background = element_blank(),
    axis.ticks = element_blank())

