libary(igraph)

graph_list <- c("knowledge_net", "tacit_knowledge_net", "explicit_knowledge_net", 
           "idea_net", "real_net", "affect_trust_net", "cog_trust_net", "prior_net", 
           "boss_net") # list of networks

# compute standard network statistics for each network 

for (g in graph_list){
  eval(parse(text = paste0('V(', g, ')$comm <- membership(optimal.community(', g,'))')))
  eval(parse(text = paste0('V(', g, ')$degree <- degree(', g, ', mode = "all")')))
  eval(parse(text = paste0('V(', g, ')$indegree <- degree(', g, ', mode = "in")')))
  eval(parse(text = paste0('V(', g, ')$outdegree <- degree(', g, ', mode = "out")')))
  eval(parse(text = paste0('V(', g, ')$closeness <- centralization.closeness(', g, ')$res')))
  eval(parse(text = paste0('V(', g, ')$betweenness <- centralization.betweenness(', g, ')$res')))
  eval(parse(text = paste0('V(', g, ')$eigen <- centralization.evcent(', g, ')$vector')))
  }

