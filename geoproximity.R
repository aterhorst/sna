#####################################################
#                                                   #
#            R script to compute geographic         #
#         distance between people in a network      #
#                   Version 20161006                #
#                                                   #
#####################################################

# Load requisite libraries.

library(ggmap)
library(readxl)
library(dplyr)
library(tidyr)
library(ggplot2)
library(reshape2)
library(igraph)
library(ggthemes)
library(MASS)
library(Matrix)
library(scales)



# Set working directory.

# setwd("~/ownCloud/Innovation Network Analysis/Quantitative Data") # MacBook
setwd("d:/Andrew/ownCloud/Innovation Network Analysis/Quantitative Data") # Home PC
# setwd("c:/Users/ter053/ownCloud/Innovation Network Analysis/Quantitative Data") # work PC

# Import excel file.

node.summary.all <- read_excel("node_summary_all.xlsx", sheet = 1)
node.summary.all$location <- with(node.summary.all, paste0(country," postcode ", post.code))

name.place <- node.summary.all[,c(1,2,3,30)]
name.place <- subset(name.place, name.place$case_no == 3) # subset specific cases

# Get geographic coordinates.

name.place$coordinate <- geocode(name.place$location, sensor = FALSE, output = "latlon", source = "google")

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

# Option 1 - set threshold.

# edge.dat$scale.dist <- ifelse(edge.dat$distance >= 2000, 2000, edge.dat$distance) # distance category
# 
# edge.dat$scale.dist <- rescale(edge.dat$scale.dist, to = c(0,1), from = range(edge.dat$scale.dist))

# Option 2 - use log distance.

edge.dat$log.dist <- log1p(edge.dat$distance) # convert to metres, add constant to get +ve log values.

# Create network object using iGraph.

edge.net <- graph.data.frame(edge.dat, directed = T)

# Generate adjacency matrix using iGraph.

edge.matrix <- get.adjacency(edge.net, sparse = F, attr = "log.dist", type = "upper", names = F) # absolute geodistance

# Write out proximity matrix.

write.matrix(edge.matrix, file = "geoproximity.txt", sep = "\t")

# Create heatmap plot.

# With gplots ...(ugly)

heatmap.2(
  edge.matrix,
  Rowv = F,
  Colv = F,
  symm = T,
  trace = "none",
  dendrogram = 'none', 
  scale = 'none',
  density.info = "none")

# With ggplot2 ...(beautiful)

melted <- melt(edge.matrix)
n <- nrow(dat)

ggplot(data = melted, aes(x=Var1, y=Var2, fill=value)) +
  geom_tile() +
  scale_x_continuous(breaks = c(1:n)) +
  scale_y_continuous(breaks = c(1:n)) +
  theme_fivethirtyeight() +
  scale_fill_continuous(name="LOG SPHERICAL\nDISTANCE (m)") +
  theme(axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    legend.text = element_text(size = 8, angle = 45),
    legend.text.align = 1,
    panel.grid.major = element_blank(),
    panel.border = element_blank(),
    panel.background = element_blank(),
    axis.ticks = element_blank())

