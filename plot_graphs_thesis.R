#####################################################
#                                                   #
#           R script to generate igraph             #
#           knowledge provider networks             #
#             (all, explicit, tacit)                #
#            node sized by constraint               #
#               Version 2016-10-16                  #
#                                                   #
#####################################################

# Load requisite libraries.

library(igraph)
library(RColorBrewer)
library(devtools)
library(randomcoloR)
library(extrafont)

# Set working directory.

## Case 1

# setwd("~/ownCloud/Innovation Network Analysis/Quantitative Data/Case 1") # MacBook
setwd("d:/Andrew/ownCloud/Innovation Network Analysis/Quantitative Data/case 1") # Home PC
# setwd("c:/Users/ter053/ownCloud/Innovation Network Analysis/Quantitative Data/case 1") # work PC

## Case 2

#setwd("~/ownCloud/Innovation Network Analysis/Quantitative Data/Case 2") # MacBook
setwd("d:/Andrew/ownCloud/Innovation Network Analysis/Quantitative Data/Case 2") # Home PC
# setwd("c:/Users/ter053/ownCloud/Innovation Network Analysis/Quantitative Data/Case 2") # work PC

## Case 3

# setwd("~/ownCloud/Innovation Network Analysis/Quantitative Data/Case 3") # MacBook
setwd("d:/Andrew/ownCloud/Innovation Network Analysis/Quantitative Data/Case 3") # Home PC
# setwd("c:/Users/ter053/ownCloud/Innovation Network Analysis/Quantitative Data/Case 3") # work PC

# Load and convert igraph objects saved as .rda files.

graph.list <- c("knowledge.provider.net", "tacit.knowledge.net", "explicit.knowledge.net", 
                  "idea.contributor.net", "idea.transformer.net", "affect.based.trust.net", 
                  "cognition.based.trust.net", "prior.relationship.net", "report.to.net")

for (g in graph.list){
  eval(parse(text = paste0('load("', g,'.rda")')))
  eval(parse(text = paste0(g, ' <- delete.vertices(', g,', is.na(V(',g,')$age) | is.na(V(',g,')$work.location))')))
}



# source_url("https://raw.githubusercontent.com/aterhorst/sna/master/pre_process.R", sha1 = NULL) # pre-process data

# Fix layout.

lo2 <- as.matrix(read.csv("graph_layout.csv")) # saved layout file
# 

lo <- layout_with_kk(knowledge.provider.net)

# lo <- layout.drl(knowledge.provider.net, use.seed = FALSE, 
#                   seed = matrix(runif(vcount(knowledge.provider.net) * 2),ncol = 2), 
#                   options = list(edge.cut=1, init.interactions=1, simmer.attraction=0), 
#                   fixed = NULL, dim = 2)

tkplot(explicit.knowledge.net, layout = lo2) # adjust in tkplot
lo2 = tkplot.getcoords(2) # grab coordinates from tkplot

write.csv(lo2, file = "graph_layout.csv", row.names = F)
 
# Set graphing parameters.

employer <- factor(V(knowledge.provider.net)$employer) # extract employer organisations

## Create a custom color scale.

n <- max(unlist(as.integer(employer))) # number of employers (varies according to case study)
col.scale <- distinctColorPalette(n) # generate colour scale based on number of employers

windowsFonts(Arial=windowsFont("TT Arial"))

## Set sizes.

scalar <- 3 # vertex symbol size
arrow_size <- 0.075 # edge arrow size
label_size <- 5 # vertex label size
title_size <- 10 #subtitle size
box_line <- 4
shift_title <- 5
anno_size <- 4


## Specify layout.

par(mfcol = c(1,3), mar = c(1,1,12,1), oma = c(2,2,2,2), bg = "white") # create 1x3 plot layout

# Plot graphs.

## 1
     
plot(explicit.knowledge.net, edge.arrow.size = arrow_size, 
     vertex.color = col.scale[employer], 
     vertex.size = (1/V(explicit.knowledge.net)$constraint)*scalar,
     vertex.label.family="Arial",
     vertex.label.cex = label_size, 
     vertex.label = V(explicit.knowledge.net)$vertex.id,
     edge.color = "gray25",
     layout = lo2)
title("Explicit Knowledge Provider", cex.main = title_size, line = shift_title)
box(lty = 'solid', lwd = box_line,  col = 'black')
text(0.90, 1.00, labels = paste0('nodes = ', vcount(explicit.knowledge.net)), adj = c(0,0), cex = anno_size)
text(0.90, 0.90, labels = paste0('edges = ', ecount(explicit.knowledge.net)), adj = c(0,0), cex = anno_size)
text(0.90, 0.80, labels = paste0('density = ', round(edge_density(explicit.knowledge.net),2)), adj = c(0,0), cex = anno_size)


## 2
plot(tacit.knowledge.net, edge.arrow.size = arrow_size, 
     vertex.color = col.scale[employer], 
     vertex.size = (1/V(tacit.knowledge.net)$constraint)*scalar,
     vertex.label.family="Arial",
     vertex.label.cex = label_size, 
     vertex.label = V(tacit.knowledge.net)$vertex.id,
     edge.color = "gray25",
     layout = lo2)
title("Tacit Knowledge Provider", cex.main = title_size, line = shift_title)
box(lty = 'solid', lwd = box_line,  col = 'gray25')
text(0.9, 1.00, labels = paste0('nodes = ', vcount(tacit.knowledge.net)), adj = c(0,0), cex = anno_size)
text(0.9, 0.90, labels = paste0('edges = ', ecount(tacit.knowledge.net)), adj = c(0,0), cex = anno_size)
text(0.9, 0.80, labels = paste0('density = ', round(edge_density(tacit.knowledge.net),2)), adj = c(0,0), cex = anno_size)

## 3
plot(idea.contributor.net, edge.arrow.size = arrow_size,
     vertex.color = col.scale[employer], 
     vertex.size = (1/V(idea.contributor.net)$constraint)*scalar,
     vertex.label.family="Arial",
     vertex.label.cex = label_size, 	 
     vertex.label = V(idea.contributor.net)$vertex.id,
     edge.color = "gray25",
     layout = lo2)
title("Idea Contributor", cex.main = title_size, line = shift_title)
box(lty = 'solid', lwd = box_line,  col = 'black')
text(0.90, 1.00, labels = paste0('nodes = ', vcount(idea.contributor.net)), adj = c(0,0), cex = anno_size)
text(0.90, 0.90, labels = paste0('edges = ', ecount(idea.contributor.net)), adj = c(0,0), cex = anno_size)
text(0.90, 0.80, labels = paste0('density = ', round(edge_density(idea.contributor.net),2)), adj = c(0,0), cex = anno_size)



dev.print(device = png, width = 6000, height = 2000, units = "px", "thesis_networks.png")

