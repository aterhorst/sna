

library(intergraph) # required to convert igraph objects into network objects used in statnet
library(statnet)
library(coda)

# load igraph object saved as .rda file

load("/2013/2013_co-author_net.rda")

# convert into sna object

a <- asNetwork(g)

list.vertex.attributes(a) # check 1



# ergm
a.model <- ergm(a~edges+
                  #gwdegree+
                  triangles+
                  cycle +
                  kstar(2)+
                  sociality("bu_id")+
                  sociality("location_id")+
                  nodematch("bu_id")+
                  nodematch("location_id"),
                control = control.ergm(init = coeff, 
                                       MCMC.burnin=10000, 
                                       MCMC.samplesize=1000, 
                                       MCMC.interval=1000,
                                       MCMLE.density.guard = 50,
                                       MCMLE.maxit = 100)
                
                  )

coeff <- coef(a.model)

summary(a.model)
