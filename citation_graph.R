#####################################################
#                                                   #
#                R script to generate               #
#           bibliometric analysis ACAP vs OI        #
#               Version 2016-09-21                  #
#                                                   #
#####################################################

# Load requisite libraries.

library(readxl)
library(ggplot2)
library(dplyr)
library(ggthemes)
library(reshape2)
library(ggthemes)
library(stringr)

# Set working directory.

# setwd("~/ownCloud/Innovation Network Analysis/Literature Review") # MacBook
setwd("d:/Andrew/ownCloud/Innovation Network Analysis/Literature Review") # Home PC
# setwd("c:/Users/ter053/ownCloud/Innovation Network Analysis/Literature Review") # work PC

# Load data.

oi <- read_excel("Bibliometrics.xlsx", sheet = 1)
ac <- read_excel("Bibliometrics.xlsx", sheet = 2)
ac_oi <- read_excel("Bibliometrics.xlsx", sheet = 3)
citations <- merge(ac,oi, by = 'Year', all.x = TRUE)
citations <- merge(citations, ac_oi, by = 'Year', all.x = TRUE)
citations[is.na(citations)] <- 0
citations <- subset(citations, citations$Year < 2016)
melted <- melt(citations, id.var = "Year")

# Create plot.

ggplot(melted, aes(Year, value, fill = variable)) +
  geom_bar(stat = "identity", data = filter(melted, variable == "AC")) +
  geom_bar(stat = "identity", data = filter(melted, variable == "OI")) +
  geom_bar(stat = "identity", data = filter(melted, variable == "AC_AND_OI")) +
  scale_x_continuous(breaks = yr) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5)) +
  scale_fill_discrete(breaks=c("AC","OI","AC_AND_OI"), 
                      labels = str_wrap(c("Absorptive Capacity", "Open Innovation", "Absorptive Capacity & Open Innovation"), width = 25)) +
  labs(fill = "") +
  ylab("Research Articles") +
  theme_economist_white()
  

  
  
  
