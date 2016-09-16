
# set working directory

setwd("~/ownCloud/Innovation Network Analysis/Case studies") # MacBook
# setwd("d:/Andrew/ownCloud/Innovation Network Analysis/Case studies/AMR") # Home PC
# setwd("c:/Users/ter053/ownCloud/Innovation Network Analysis/Case studies/AMR") # work PC

# import excel file


node.summary.all <- read_excel("node_summary_all.xlsx", sheet = 1)

motiv <- subset(node.summary.all, node.summary.all$case_no == 1)

motiv <- node.summary.all[,c(1:3,22:27)]



# motiv$extrinsic.motivation <- rowMeans(motiv[,c(4:7)])



motiv$id <- as.factor(motiv$id)
motiv$case_no <- as.factor(motiv$case_no)


melty <- melt(motiv)

ggplot(data = melty, aes(x = id, y = value, fill = variable)) + 
  geom_bar(stat = "identity") + coord_flip() + facet_wrap("case_no") + coord_polar()
