# Statistical analysis of CSIRO co-author network

library(igraph)
library(VGAM)

# read graph

setwd("C:/Users/ter053/ownCloud/Social Network Analysis/Programming assignments/Project")
g = read.graph("co-author.gml",format="gml")


# calculate some basic statistics

metrics <- data.frame(
  name = V(g)$author, 
  div = V(g)$division,
  loc = V(g)$location,
  deg = degree(g), # degree
  btw = betweenness(g), # betweenness
  clo = closeness(g), # closeness
  eig = evcent(g)$vector, # eigenvector centrality
  cor = graph.coreness(g) # coreness
)

# sort metrics

head(metrics[with(metrics, order(-deg)),], n = 10L)
head(metrics[with(metrics, order(-eig)),], N = 10L)

# power law distribution

fit <- power.law.fit(deg)


# plot graphs


hist(deg, main = "degree distribution", xlab = "degreek") # plot degree distribution

# plot the cumulative empirical distribution

cumy = c()
y = tabulate(degrees)
x = 1:length(y)
for (i in 1:length(x)) {  
  cumy[i] = sum(y[i:length(x)])/sum(y)
}
options(scipen=10)
plot(x,cumy,log="xy",xlab="degree k",ylab="P(x) >= k",main = "cumulative empirical distribution",cex=0.5)	

# overlay the fitted distribution

startval = cumy[fit$xmin]
fittedvals = (fit$xmin:max(x))^(-fit$alpha + 1)*(startval)/fit$xmin^(-fit$alpha + 1) 
points(fit$xmin:max(x),fittedvals,type='l',col='red')

# overlay the fitted distribution

startval = cum[fit$xmin]
fitted = (fit$xmin:max())



# global statistics

cl <- clusters(g, mode="weak") # Calculate the maximal (weakly or strongly) connected components of a graph
deg <- degree(g, mode="all") # calculate degree
avg_deg <- mean(deg) # average degree
clq <- cliques(g)
lrg_clq <- largest.cliques(g)
max_clq <- maximal.cliques(g)
clq_no <- clique.number(g)


ec <- evcent(g) # eigen vector centrality (largest value = authors in large cliques)

mod.wt <- modularity(g, membership(walktrap.community(g))) # calculate modularity using walktrap community detection (fast)
wmod.fc <- modularity(g, membership(fastgreedy.community(g))) # calculate modularity using fastgreedy community detection(fast)
mod.eb <- modularity(g, membership(edge.betweenness.community(g))) # calculate modularity using edge betweenness community detection (extremely slow)

# report author statistics

V(g)$author[which.max(closeness(g))] # author with top closeness scores
V(g)$author[which.max(degree(g))] # author with highest degrees
V(g)$author[which.max(betweenness(g))] # author with highest betweenness?


# Calculate mixing

div_mix <- assortativity.nominal(g,factor(V(g)$division), directed = FALSE) # homophily in terms of division
loc_mix <- assortativity.nominal(g,factor(V(g)$location), directed = FALSE) # homophily in terms of location
 