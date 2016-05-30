library(GGally)
library(network)
library(sna)
library(intergraph)
library(ggplot2)
library(ggnetwork)

graph.list <- c("knowledge.provider.net", "tacit.knowledge.net", "explicit.knowledge.net", 
                "idea.generation.net", "idea.realisation.net", "affect.based.trust.net", 
                "cognition.based.trust.net", "prior.relationship.net", "report.to.net")

for (g in graph.list){
  eval(parse(text = paste0('load("', g,'.rda")')))
  eval(parse(text = paste0(g,'.sna <- asNetwork(', g,')')))
}



ggnet2(knowledge.provider.net.sna, arrow.size = 12,label ="vertex.id", label.size = 4, color = "employer", palette = colorRampPalette(c("green", "orange","red","blue", "yellow"))(23))

ggnetwork(knowledge.provider.net.sna, arrow.gap = 0,layout = "fruchtermanreingold", cell.jitter = 0.75)

ggplot(knowledge.provider.net.sna, aes(x = x, y = y, xend = xend, yend = yend)) +
  geom_edges(arrow = arrow(length = unit(6, "pt"), type = "closed")) +
  geom_nodes(color = knowledge.provider.net.sna %v% "employer", size = 12) +
  geom_nodetext(label = knowledge.provider.net.sna %v% "vertex.id") +
  theme_blank()