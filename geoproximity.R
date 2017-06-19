#####################################################
#                                                   #
#            R script to compute geographic         #
#         distance between people in a network      #
#                   Version 20161022                #
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
library(RColorBrewer)



# Set working directory.

setwd("~/ownCloud/Innovation Network Analysis/Quantitative Data") # MacBook
setwd("d:/Andrew/ownCloud/Innovation Network Analysis/Quantitative Data") # Home PC
# setwd("c:/Users/ter053/ownCloud/Innovation Network Analysis/Quantitative Data") # work PC

# Import excel file.

node.summary.all <- read_excel("node_summary_all.xlsx", sheet = 1)
node.summary.all$location <- with(node.summary.all, paste0(country," postcode ", post.code))

name.place <- node.summary.all[,c(1,2,3,30)]
name.place <- subset(name.place, name.place$case_no == 1) # subset specific cases

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

# Compute log distance.

edge.dat$log.dist <- log1p(edge.dat$distance) 

# Create network object using iGraph.

edge.net <- graph.data.frame(edge.dat, directed = T)
save(edge.net,file = "co-author_distance.rda")

# Generate adjacency matrix using iGraph.

edge.matrix.export <- get.adjacency(edge.net, sparse = F, attr = "log.dist", type = "upper", names = F) # absolute geodistance

# Write out proximity matrix.

write.matrix(edge.matrix.export, file = "log.spherical.distance.txt", sep = "\t")

# Create heatmap plot.


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

dev.print(device = pdf, width = 10, height = 10.75, "sph_distance.pdf")

library(lattice)
library(gplots)

col <- colorRampPalette(c("#BB4444", "#EE9988", "#FFFFFF", "#77AADD", "#4477AA"))
YlOrBr <- c("#FFFFD4", "#FED98E", "#FE9929", "#D95F0E", "#993404")


levelplot(edge.matrix, scales=list(tck=0, x=list(rot=0)),
          col.regions = colorRampPalette(YlOrBr, space = "Lab"),
#          colorkey=list(space="bottom"),
main="Spherical Distance Between Actors",
          xlab=NULL, ylab=NULL)

