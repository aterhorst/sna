
library(reshape)
library(ggmap)
library(dplyr)
library(tidyr)

# read in saved vertex attribute info

place <- read.csv("2016_vertex_attr.csv", header = T, stringsAsFactors = F)

# generate geographic coordinates.

place <- subset(place, select = c("name", "employee", "geocode")) # subset attributes

place$coordinate <- geocode(place$geocode, sensor = FALSE, output = "latlon", source = "google")

dat <- as.data.frame(as.list(place[,c(1,4)]))
dat$name <- as.character(dat$name)

write.csv(dat, "author_2016_geocodes.csv", row.names = F)

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
  # Remove name1 equals to name2
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

require(igraph)

edge.dat <- subset(edge.dat, select = c(name1, name2, distance)) 

# Compute log distance.

edge.dat$log.dist <- log1p(edge.dat$distance) 

# Create network object using iGraph.

edge.net <- graph.data.frame(edge.dat, directed = T)

# With ggplot2 ...(beautiful)

edge.matrix.km <- get.adjacency(edge.net, sparse = F, attr = "distance", type = "upper", names = T) 
melted <- melt(edge.matrix.km)


melted$Var1 <- factor(melted$Var1)
melted$Var2 <- factor(melted$Var2)
myPalette <- colorRampPalette(rev(brewer.pal(11 , "Spectral")), space="Lab")
s <- seq(0, max(edge.dat$distance), 4000)
# l <- c("0 km", "5000 km", "10000 km", "15000km")

zp1 <- ggplot(melted,
              aes(x = Var2, y = Var1, fill = value))
zp1 <- zp1 + geom_tile()
zp1 <- zp1 + scale_fill_gradientn(colours = myPalette(100), name="SPHERICAL DISTANCE (km) ", breaks = s)
zp1 <- zp1 + scale_x_discrete(expand = c(0, 0))
zp1 <- zp1 + scale_y_discrete(expand = c(0, 0))
zp1 <- zp1 + coord_equal()
zp1 <- zp1 + theme_fivethirtyeight()
zp1 <- zp1 + theme(axis.title.x = element_blank(), axis.title.y = element_blank(),
                   legend.text = element_text(size = 12),
                   legend.text.align = 0.5,
                   axis.text.x = element_text(size = 12),
                   axis.text.y = element_text(size = 12))
zp1 <- zp1 + guides(fill = guide_colorbar(barwidth = 15, barheight = 1))

print(zp1)

