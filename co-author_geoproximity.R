
library(reshape)
library(ggmap)
library(igraph)

# read in saved vertex attribute info

place <- read.csv("2016_vertex_attr.csv", header = T)

# generate geographic coordinates.

place <- place[,c(1,2,8)] # subset attributes

place$coordinate <- geocode(place$geocode, sensor = FALSE, output = "latlon", source = "google")

dat <- as.data.frame(as.list(place[,c(1,4)]))

write.csv(dat, "author_2016_geocodes.csv")

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

# Create distance matrix.

edge.dat <- subset(edge.dat, select = c(name1, name2, distance)) 

# Compute log distance.

edge.dat$log.dist <- log1p(edge.dat$distance) 

# Create network object using iGraph.

edge.net <- graph.data.frame(edge.dat, directed = T)