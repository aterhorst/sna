
# install.packages("statnet")
# install.packages(c("numDeriv", "yacca", "rgl"))

libray(statnet)

setwd("~/ownCloud/Statnet")

data(package="network")
# List available datasets in network
library(network) # Make sure that network is loaded
data(flo) # Load a built-in data set; see ?flo for more
flo

nflo <- network(flo, directed=FALSE) # Create a network object based on flo
nflo
summary(nflo) # Get an overall summary
print(nflo) # Simple print method
network.dyadcount(nflo) # How many dyads in nflo?
network.edgecount(nflo) # How many edges are present?
network.size(nflo) # How large is the network?
as.sociomatrix(nflo) # Show it as a sociomatrix
nflo[,] # Another way to do it
