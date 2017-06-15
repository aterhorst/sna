

library(intergraph) # required to convert igraph objects into network objects used in statnet
library(statnet)
library(coda)

# load igraph object saved as .rda file

load("2016_co-author_net.rda")

# convert into sna object

a <- asNetwork(g)

list.vertex.attributes(a) # check 1



# ergm

## set initial control parameters



## first model run

a.model <- ergm(a~edges+
                  gwesp(0.25, fixed = T)+
                  gwdegree(0.5, fixed = T)+
                  absdiff("org_rank")+
                  absdiff("prod")+
                  nodematch("bu", diff = T)+
                  nodematch("location_id"), 
                  control = control.ergm(MCMLE.density.guard = 100))
coeff <- coef(a.model)

## subsequent model runs

a.model <- ergm(a~edges+
                  gwesp(0.25, fixed = T)+
                  gwdegree(0.5, fixed = T)+
                  absdiff("org_rank")+
                  absdiff("prod")+
                  nodematch("bu", diff = T)+
                  nodematch("location_id"),
                  control = control.ergm(init = coeff,
                                         MCMLE.density.guard = 100,
                                         MCMLE.maxit = 50))
coeff <- coef(a.model)

summary(a.model)
