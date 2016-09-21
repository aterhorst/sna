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

# Set working directory.

setwd("~/ownCloud/Innovation Network Analysis/Literature Review") # MacBook
# setwd("d:/Andrew/ownCloud/Innovation Network Analysis/Literature Review") # Home PC
# setwd("c:/Users/ter053/ownCloud/Innovation Network Analysis/Literature Review") # work PC

# Load data.

citations <- read_excel("Publication_Year_ACAP_OI.xlsx", sheet = 1)
citations <- subset(citations, citations$Year < 2016)
labels <- seq(1990, 2015, by = 2)

# Create plot.

ggplot(citations, aes(x = Year, y = Articles)) +
  geom_bar(stat = "identity", data = filter(citations, Topic == "Absorptive Capacity"), fill = "blue") +
  geom_bar(stat = "identity", data = filter(citations, Topic == "Open Innovation"), fill = "red") +
  scale_x_continuous(breaks = labels) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5)) 
  
  
  
