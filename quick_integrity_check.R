library(readxl)
nodes <- read_excel("surveydata.xlsx", sheet = 1)
nodes <- subset(nodes, select = c("name", "Gender", "Age", "Location", 
                                  "Education", "Occupation1", "Experience", "Tenure"))
processed <- read.csv("knowledge.provider.net_attributes.csv")
processed <- subset(processed, select = c("name", "gender", "age", "work.location", 
                                          "education.level", "occupation.class", "work.experience", "current.job.tenure"))
nodes <- as.list(nodes[which(nodes$name == "Erik Siedler"),])
processed <- as.list(processed[which(processed$name == "Erik Siedler"),])
    