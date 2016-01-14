#####################################################
#                                                   #
#            R script to generate global            #
#               networks statistics                 #
#                                                   #
#####################################################

library(igraph)
library(devtools)

# pre-process data

source_url("https://raw.githubusercontent.com/aterhorst/sna/master/pre_process.R", sha1 = NULL)

# create list of graphs to be crunched

graph.list <- c("knowledge.provider.net", "tacit.knowledge.provider.net", "explicit.knowledge.provider.net", 
           "idea.generation.net", "idea.realisation.net", "affect.based.trust.net", "cognition.based.trust.net", 
           "prior.relationship.net", "report.to.net")

# compute assortativity coefficient for each network 

assortativity <- data.frame("network" = character(),"assort.employer" = numeric(), "assort.location" = numeric(), 
                            "assort.occupation" = numeric(), "assort.education.level" = numeric(), 
                            "assort.education.field" = numeric(), stringsAsFactors = FALSE)  # create empty data frame

for (i in graph.list){
  eval(parse(text = paste0('assort1 <- round(assortativity.nominal(', i,', factor(V(', i,')$employer), directed = TRUE), digits = 4)'))) # calculate assortativity
  eval(parse(text = paste0('assort2 <- round(assortativity.nominal(', i,', factor(V(', i,')$work.location), directed = TRUE), digits = 4)')))
  eval(parse(text = paste0('assort3 <- round(assortativity.nominal(', i,', factor(V(', i,')$occupation.class), directed = TRUE), digits = 4)')))
  eval(parse(text = paste0('assort4 <- round(assortativity.nominal(', i,', factor(V(', i,')$education.level), directed = TRUE), digits = 4)')))
  eval(parse(text = paste0('assort5 <- round(assortativity.nominal(', i,', factor(V(', i,')$education.field), directed = TRUE), digits = 4)')))
  assortativity[nrow(assortativity) + 1,] <- c(i,assort1, assort2, assort3, assort4, assort5) # add row
}

triads <- data.frame("network" = character(), "003" = integer(), "012" = integer(), "102" = integer(), "012D" = integer(), "021U" = integer(),
                     "021C" = integer(), "111D" = integer(), "111U" = integer(), "030T" = integer(), "030C" = integer(), "201" = integer(),
                     "120D" = integer(), "120U" = integer(), "120C" = integer(), "210" = integer(), "300" = integer(), stringsAsFactors = FALSE)

for (i in graph.list){
  eval(parse(text = paste0('census <- triad.census(', i, ')')))
  triads[nrow(triads) + 1,] <- c(i,census)
  
}

dyads <- data.frame("network" = character(), "mutual.connections" = integer(), "asymmetric.connections" = integer(), 
                     "nil.connections" = integer(), stringsAsFactors = FALSE)

for (i in graph.list){
  eval(parse(text = paste0('dcensus <- dyad.census(', i, ')')))
  dyads[nrow(dyads) + 1,] <- c(i,dcensus)
  
}

# export data to csv

write.csv(assortativity, "assortativity.coefficients.csv", row.names = FALSE)
write.csv(triads, "triad.census.csv", row.names = FALSE)
write.csv(dyads, "dyad.census.csv", row.names = FALSE)



