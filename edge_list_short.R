#####################################################
#                                                   #
#     R script to plot network visualisations       #
#                                                   #
#####################################################

edge_all <- read_excel("surveydata.xlsx", sheet = 2) # read in relationships sheet from onasurvey downloaded workbook
edge_knowledge <- filter(edge_all, relationship_set_knowledge_sharing == 1) # extract knowledge received from ties
knowledge_net <- graph.data.frame(edge_knowledge)
source_url("https://raw.githubusercontent.com/aterhorst/sna/master/reverse_direction.R") # function to reverse ties
knowledge_net <- graph.reverse(knowledge_net) # fix direction of knowledge provider ties
knowledge_net <- simplify(knowledge_net, remove.multiple = F, remove.loops = TRUE)