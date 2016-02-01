#####################################################
#                                                   #
#          R script to do analyse networks          #
#                   using statnet                   #
#                                                   #
#####################################################

# restart R

.rs.restartR()
rm(list=ls())

# load requisite libraries

library(intergraph) # required to convert igraph objects into network objects used in statnet
library(statnet)
library(coda)

# set working directory

setwd("d:/Andrew/ownCloud/Innovation Network Analysis/Case studies/HF") # Home PC

# load and convert igraph objects saved as .rda files

graph.list <- c("knowledge.provider.net", "tacit.knowledge.provider.net", "explicit.knowledge.provider.net", 
                "idea.generation.net", "idea.realisation.net", "affect.based.trust.net", 
                "cognition.based.trust.net", "prior.relationship.net", "report.to.net")

for (g in graph.list){
  eval(parse(text = paste0('load("', g,'.rda")')))
  eval(parse(text = paste0(g,' <- asNetwork(', g,')')))
}

# plotting

e <- get.edge.attribute(knowledge.provider.net, "tacit")
c <- get.vertex.attribute(knowledge.provider.net, "employer")
v <- get.vertex.attribute(knowledge.provider.net, "vertex.id")

plot.network(knowledge.provider.net, label = v, 
             vertex.col = c + 3, 
             vertex.cex = 2,
             edge.lwd = e * 4)

sociomatrixplot(knowledge.provider.net, labels = list(c(1:18),c(1:18)), 
                cex.lab = 0.5, diaglab = FALSE)
                
# ergm

knowledge.provider.model.00 <- ergm(knowledge.provider.net~edges+
                mutual+
                sender(base = c("personality.openness"))+
                nodecov("motiv.intrinsic")+
                nodecov("personality.openness")+
                nodecov("personality.conscientiousness")+
                nodecov("personality.agreeableness")+
                nodecov("self.determination")+
                nodecov("motiv.extrinsic.regulation.social")+
                nodecov("motiv.extrinsic.regulation.material")+
                nodecov("motiv.identified.regulation")+
                nodecov("motiv.introjected.regulation")+
                nodecov("identification.org")+
                nodecov("identification.group")+
                nodematch("employer")+
                nodematch("education.level")+
                nodematch("education.field")+
                nodematch("occupation.class")+
                nodematch("work.location")+
                absdiff("age")+
                absdiff("work.experience")+
                absdiff("current.job.tenure"))

                

knowledge.model.01 <- ergm(knowledge.provider.net~edges+mutual+
                edgecov(affect.based.trust.net)+
                edgecov(cognition.based.trust.net))+
                edgecov(prior.relationship.net)+
                edgecov(report.to.net))

knowledge.model.02 <- ergm(explicit.knowledge.provider.net~edges+mutual+
                   edgecov(affect.based.trust.net)+
                   edgecov(cognition.based.trust.net)+
                   edgecov(prior.relationship.net)+
                   edgecov(report.to.net))

knowledge.model.03 <- ergm(tacit.knowledge.provider.net~edges+mutual+
                   edgecov(affect.based.trust.net)+
                   edgecov(cognition.based.trust.net)+
                   edgecov(prior.relationship.net)+
                   edgecov(report.to.net))





# analysis of idea.generation.net

ideation.model.04 <- ergm(idea.generation.net~edges+mutual+
                            edgecov(knowledge.provider.net)+
                            edgecov(affect.based.trust.net)+
                            edgecov(cognition.based.trust.net)+
                            edgecov(prior.relationship.net))

ideation.model.04.gof <- gof(ideation.model.04~idegree+odegree+espartners+distance)

ideation.model.06 <- ergm(idea.generation.net~edges+
                            gwesp(0.5),
                            verbose = TRUE)
                 




plot(ideation.model.06.gof)


# analysis of idea.realisation.net

realisation.model.05 <- ergm(idea.realisation.net~edges+
                               mutual+
                               edgecov(idea.generation.net)+
                               edgecov(knowledge.provider.net)+
                               edgecov(affect.based.trust.net)+
                               edgecov(cognition.based.trust.net)+
                               edgecov(prior.relationship.net))
