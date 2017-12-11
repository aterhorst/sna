

library(igraph)

# Create adjaceny matrix

B <- matrix(c(0,0,1,0,1,0,1,0,0,1,0,0,0,0,0,1,0,1,1,0,1,0,0,0,0,0,1,0,0,0,0,0,1,0,0,0),nrow = 6,ncol = 6)
W <- matrix(c(0,0,2,0,1,0,3,0,0,4.5,0,0,0,0,0,2,0,1,1,0,6,0,0,0,0,0,2,0,0,0,0,0,1,0,0,0),nrow = 6,ncol = 6)

# Create graph object

UG <- graph_from_adjacency_matrix(B, mode = "undirected")
DG <- graph_from_adjacency_matrix(B, mode = "directed")
WG <- graph_from_adjacency_matrix(W, mode = "directed", weighted = T)

# Plot graphs

as <- 0.15 # edge arrow size
ls <- 2 # vertex label size
ts <- 3 #title size
ew <- 1
sh <- 2
an <- 2



lo <- layout_with_kk(DG)

par(mfcol = c(1,3))

plot(UG, vertex.color = "light blue", 
     vertex.label.cex = ls,
     edge.color = "gray25",
     edge.width = ew,
     layout = lo,
     axes = F)
title(sub = "(a)", cex.sub = ts, line = sh)
text(0.9, -0.6, labels = "vertex", adj = c(0,0), cex = an)
text(0, -1, labels = "edge", adj = c(0,0), cex = an)
arrows(0.2,-0.9,0.45,-0.675,length=0.1,angle=20)
arrows(1,-0.65,0.775,-0.925,length=0.1,angle=20)


plot(DG, vertex.color = "light blue", 
     vertex.label.cex = ls,
     edge.arrow.size = as,
     edge.color = "gray25",
     edge.width = ew,
     layout = lo)
title(sub = "(b)", cex.sub = ts, line = sh)

plot(WG, vertex.color = "light blue",
     vertex.label.cex = ls,
     edge.arrow.size = as,
     edge.color = "gray25",
     edge.width = E(WG)$weight*0.6,
     layout = lo)
title(sub = "(c)", cex.sub = ts, line = sh)



dev.print(device = png, width = 1400, height = 400, units = "px", "~/ownCloud/Thesis/Images/example_networks.png")

 