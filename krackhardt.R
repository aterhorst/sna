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

## adjacency matrices

require(MASS)

reports_adj <- get.adjacency(reports_grp, type = "both", names = FALSE)
friends_adj <- get.adjacency(friends_grph, type = "both", names = FALSE)
advice_adj <- get.adjacency(advice_grph, type = "both", names = FALSE)

write.matrix(reports_adj, file = "~/ownCloud/Krackhardt/MPNET/reports_to_net.txt")
write.matrix(friends_adj, file = "~/ownCloud/Krackhardt/MPNET/friends_with_net.txt")
write.matrix(advice_adj, file = "~/ownCloud/Krackhardt/MPNET/seeks_advice_from_net.txt")

## create dyadic covariate file.

fn <- "~/ownCloud/Krackhardt/MPNET/dyadic_covariates.txt" 
cat("", file = fn) # create empty file

reports_df <- as.data.frame(as.matrix(reports_adj))
friends_df <- as.data.frame(as.matrix(friends_adj))
advice_df <- as.data.frame(as.matrix(advice_adj))

cat("reports_to\n", file = fn, append = TRUE)
write.table(reports_df, fn, append = TRUE, sep = "\t", row.names = FALSE, col.names = FALSE)

cat("friends_with\n", file = fn, append = TRUE)
write.table(friends_df, fn, append = TRUE, sep = "\t", row.names = FALSE, col.names = FALSE)

cat("seeks_advice_from\n", file = fn, append = TRUE)
write.table(advice_df, fn, append = TRUE, sep = "\t", row.names = FALSE, col.names = FALSE)


## attribute tables

continuous.data <- subset(attributes, select = c(AGE,TENURE,LEVEL)) # select columns with continuous data
categorical.data <- subset(attributes, select = DEPT) # select columns with categorical data

write.table(continuous.data, "~/ownCloud/Krackhardt/MPNET/continuous_data.txt", row.names = FALSE, col.names = TRUE, sep = "\t", quote = FALSE)
write.table(categorical.data, "~/ownCloud/Krackhardt/MPNET/categorical_data.txt", row.names = FALSE, col.names = TRUE, sep = "\t", quote = FALSE)





