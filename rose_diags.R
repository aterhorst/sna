

library(ggplot2)
library(plyr)

# load categorical data generated using export_to_MPNet

hf_cat <- na.omit(read.table("d:/Andrew/ownCloud/Innovation Network Analysis/Case studies/HF/categorical_data.txt", sep = "", header = T))
amr_cat <- na.omit(read.table("d:/Andrew/ownCloud/Innovation Network Analysis/Case studies/AMR/categorical_data.txt", sep = "", header = T))
gihh_cat <- na.omit(read.table("d:/Andrew/ownCloud/Innovation Network Analysis/Case studies/GIHH/categorical_data.txt", sep = "", header = T))

# compute frequency - education

hf <- count(hf_cat,"education.level")
hf$case <- 1

amr <- count(amr_cat,"education.level")
amr$case <- 2

gihh <- count(gihh_cat,"education.level")
gihh$case <- 3

ed_rose <- rbind(hf,amr,gihh)

# compute frequency - field

hf_f <- count(hf_cat,"education.field")
hf_f$case <- 1

amr_f <- count(amr_cat,"education.field")
amr_f$case <- 2

gihh_f <- count(gihh_cat,"education.field")
gihh_f$case <- 3

field_rose <- rbind(hf_f,amr_f,gihh_f)

# specify labels

case_id <- c("1" = "Case 1", "2" = "Case 2", "3" = "Case 3")

ed_level <- c("Secondary Education","Certificate Level","[Advanced] Diploma",
              "Bachelors Degree","Graduate Certificate/Diploma", 
              "Masters Degree","Doctoral Degree")

ed_field <- c("Natural & Physical Sciences", "information Technology", "Engineering & Related Technologies",
              "Architecture & Building", "Agricultural, Environmental & Related Studies",
              "Health","Education", "Management & Commerce", "Society & Culture", "Creative Arts",
              "Food, Hospitality & Personal Services", "Mixed Field Programmes")

# rose diagrams

# Plot 1

ggplot(ed_rose, aes(factor(education.level), freq, fill = factor(education.level))) +
  geom_bar(stat = "identity", position = "dodge", color = "white") + 
  geom_text(aes(y = freq + 0.75,label = freq)) +
  coord_polar() +
  scale_y_continuous(trans = "log2", breaks = c(1,2,4,8,16,32)) +
  # theme_bw() +
  theme(axis.text.x = element_blank(), axis.text.y = element_blank(), 
        axis.ticks = element_blank(), axis.title.x = element_blank(), 
        axis.title.y = element_blank()) +
  facet_wrap(~ case, labeller = as_labeller(case_id)) +
  scale_fill_discrete(name="Education Level",
                      breaks=c(2:8),
                      labels= ed_level)

# Plot 2

ggplot(field_rose, aes(factor(education.field), freq, fill = factor(education.field))) +
  geom_bar(stat = "identity", position = "dodge", color = "white") + 
  geom_text(aes(y = freq + 0.75,label = freq)) +
  coord_polar() +
  scale_y_continuous() +
  # theme_bw() +
  theme(axis.text.x = element_blank(), axis.text.y = element_blank(), 
        axis.ticks = element_blank(), axis.title.x = element_blank(), 
        axis.title.y = element_blank()) +
  facet_wrap(~ case, labeller = as_labeller(case_id)) +
  scale_fill_discrete(name="Education Field",
                      breaks=c(1:12),
                      labels= ed_field)

# boxplot

