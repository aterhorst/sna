

library(ggplot2)
library(plyr)
library(reshape2)
library(ggthemes)

# load categorical data generated using export_to_MPNet.R

hf_cat <- na.omit(read.table("~/ownCloud/Innovation Network Analysis/Case studies/HF/categorical_data.txt", sep = "", header = T))
amr_cat <- na.omit(read.table("~/ownCloud/Innovation Network Analysis/Case studies/AMR/categorical_data.txt", sep = "", header = T))
gihh_cat <- na.omit(read.table("~/ownCloud/Innovation Network Analysis/Case studies/GIHH/categorical_data.txt", sep = "", header = T))

# load continuous data generated using export_to_MPNet.R

hf_cont <- na.omit(read.table("~/ownCloud/Innovation Network Analysis/Case studies/HF/continuous_data.txt", sep = "", header = T))
amr_cont <- na.omit(read.table("~/ownCloud/Innovation Network Analysis/Case studies/AMR/continuous_data.txt", sep = "", header = T))
gihh_cont <- na.omit(read.table("~/ownCloud/Innovation Network Analysis/Case studies/GIHH/continuous_data.txt", sep = "", header = T))

# merge continuous data

hf_cont$case <- 1
amr_cont$case <-2
gihh_cont$case <-3

cont <- rbind(hf_cont,amr_cont,gihh_cont)
cont <- cont[,c(1:3,19)]

x <- ddply(cont,"case", summarise, 
      N = length(age),
      mean.age = mean(age),
      mean.experience = mean(work.experience),
      mean.tenure = mean(current.job.tenure),
      sd.age = sd(age),
      sd.experience = sd(work.experience),
      sd.tenure = sd(current.job.tenure),
      range.age = max(age) - min(age),
      range.experience = max(work.experience) - min(work.experience),
      range.tenure = max(current.job.tenure) - min(current.job.tenure))




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

ed_level <- c("Secondary Education","Certificate Level","Diploma/Advanced Diploma",
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
  geom_text(aes(y = freq + 0.75, label = freq)) +
  coord_polar() +
  scale_y_continuous(trans = "sqrt", breaks = c(1,4,9,16,25,36)) +  
#  scale_y_continuous(trans = "log2", breaks = c(1,2,4,8,16,32)) +
  # theme_bw() +
  theme_economist() +
  theme(axis.text.x = element_blank(), axis.text.y = element_blank(), 
        axis.ticks = element_blank(), axis.title.x = element_blank(), 
        axis.title.y = element_blank()) +
  facet_wrap(~ case, labeller = as_labeller(case_id)) +
  scale_fill_discrete(name="LEVEL OF EDUCATION",
                      breaks=c(2:8),
                      labels= ed_level)

# Plot 2

ggplot(field_rose, aes(factor(education.field), freq, fill = factor(education.field))) +
  geom_bar(stat = "identity", position = "dodge", color = "white") + 
  geom_text(aes(y = freq + 0.75,label = freq)) +
  coord_polar() +
  scale_y_continuous(breaks = c(4,8,12,16)) +
  theme_economist() +
  # theme_bw() +
  theme(axis.text.x = element_blank(), axis.text.y = element_blank(), 
        axis.ticks = element_blank(), axis.title.x = element_blank(), 
        axis.title.y = element_blank()) +
  facet_wrap(~ case, labeller = as_labeller(case_id)) +
  scale_fill_discrete(name="FIELD OF EDUCATION",
                      breaks=c(1:12),
                      labels= ed_field)

# boxplot

cont$case <- as.factor(cont$case)

dat.m <- melt(cont,id.vars = 'case', measure.vars=c('age','work.experience','current.job.tenure'))

p <- ggplot(dat.m) +
  geom_violin(color="gray",aes(x=factor(variable), y = value)) +
  geom_boxplot(aes(x=factor(variable), y = value, fill = variable), width = 0.1, outlier.colour = NA) +
  geom_jitter(alpha=0.5, aes(x=factor(variable), y = value), color= "black",position = position_jitter(width = .4))+
  facet_wrap(~case, labeller = as_labeller(case_id)) +
  scale_y_continuous("Years", breaks = c(0,5,10,15,20,25,30,35,40,45,50,55,60,65,70)) +
  scale_x_discrete(name = "", labels = c("Age", "Work\nExperience", "Job\nTenure")) +
  theme_economist() +
#  theme_classic()
  theme(legend.position = "none")

p                      

