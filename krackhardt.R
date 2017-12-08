###################################################
#                                                 #
#             Krackhardt ERGM example             #
#                Version 20171205                 #
#                                                 #
###################################################

# Load library

library(igraph)

# Load data

data(kracknets, package = "NetData")

# Create graphs

## Create edgelists

reports <- subset(reports_to_data_frame, reports_to_data_frame$reports_to_tie == "1")[,1:2]
friends <- subset(friendship_data_frame, friendship_data_frame$friendship_tie == "1")[,1:2]
advice <- subset(advice_data_frame, advice_data_frame$advice_tie == "1")[,1:2]

attributes = cbind(1:length(attributes[,1]), attributes)

## Generate graphs

reports_grph <- graph_from_data_frame(reports, vertices = attributes, directed = T)
friends_grph <- graph_from_data_frame(friends, vertices = attributes, directed = T)
advice_grph <- graph_from_data_frame(advice, vertices = attributes, directed = T)

# Plot graphs

par(mfcol = c(1,3))

lo <- layout_with_kk(reports_grph)
as <- 0.1 # arrow size
ts <- 4
vl <- 3

plot(reports_grph, edge.arrow.size = as,
     layout = lo,
     vertex.label.cex = vl,
     vertex.color = V(reports_grph)$DEPT,
     edge.color = "dark grey")
title("Reports to", cex.main = ts)

plot(friends_grph, edge.arrow.size = as,
     layout = lo,
     vertex.label.cex = vl,
     vertex.color = V(friends_grph)$DEPT,
     edge.color = "dark grey")
title("Friends with", cex.main = ts)

plot(advice_grph, edge.arrow.size = as,
     layout = lo,
     vertex.label.cex = vl,
     vertex.color = V(advice_grph)$DEPT,
     edge.color = "dark grey")
title("Seeks advice from", cex.main = ts)

dev.print(device = png, width = 2000, height = 800, units = "px", "~/ownCloud/Thesis/Images/krackhardt.png")

# Export data for analysis in MPNet







