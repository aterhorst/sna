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

setwd("~/ownCloud/Innovation Network Analysis/Literature Review") # MacBook
setwd("d:/Andrew/ownCloud/Innovation Network Analysis/Literature Review") # Home PC
# setwd("c:/Users/ter053/ownCloud/Innovation Network Analysis/Literature Review") # work PC

# Load data.

ac <- read_excel("Bibliometrics.xlsx", sheet = 1)
oi <- read_excel("Bibliometrics.xlsx", sheet = 2)
tk <- read_excel("Bibliometrics.xlsx", sheet = 4)
oi_ac <- read_excel("Bibliometrics.xlsx", sheet = 3)
ac_tk <- read_excel("Bibliometrics.xlsx", sheet = 5)
oi_tk <- read_excel("Bibliometrics.xlsx", sheet = 6)
citations <- merge(ac,oi, by = 'Year', all.x = TRUE)
citations <- merge(citations, oi_ac, by = 'Year', all.x = TRUE)
citations <- merge(citations, tk, by = 'Year', all.x = TRUE)
citations <- merge(citations, ac_tk, by = 'Year', all.x = TRUE)
citations <- merge(citations, oi_tk, by = 'Year', all.x = TRUE)
citations[is.na(citations)] <- 0
citations <- subset(citations, citations$Year <= 2016)
melted <- melt(citations, id.var = "Year")

nr <- seq(0,400, by = 50) # breaks for number of research articles/year

# Create faceted plot.

yr <- seq(1990,2016, by = 4) # breaks for years

melted$var <- factor(melted$variable, levels = c("AC", "OI", "TK", "OI_AC", "AC_TK", "OI_TK"), ordered = TRUE) # reorder
titles <- c(AC = "Absorptive Capacity", OI = "Open Innovation", TK = "Tacit Knowledge", 
            OI_AC = "Open Innovation & Absorptive Capacity", AC_TK = "Absorptive Capacity & Tacit Knowledge",
            OI_TK = "Open Innovation & Tacit Knowledge")

melted$var <- plyr::revalue(melted$variable,titles)


ggplot(melted, aes(Year, value, fill = variable)) +
  facet_wrap(~var, labeller = labeller(var = label_wrap_gen(20)), ncol = 3) +
  geom_bar(stat = "identity") +
  scale_x_continuous(breaks = yr) +
  scale_y_continuous(breaks = nr) +
#  theme_few() +
theme_fivethirtyeight()
  #  theme_economist_white(base_size = 6) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, size = 8), axis.title=element_text(size=10,face="bold")) +
  theme(strip.text.x = element_text(size=12, face="bold")) +
  theme(legend.position = "none") +
  labs(fill = "") +
  xlab("Year") +
  ylab("Peer-Reviewed Articles") 
  
dev.print(device = png, width = 2000, height = 1000, units = "px", "bibliometric.png")

# Create single plot.

yr <- seq(1990,2016, by = 2)


ggplot(melted, aes(Year, value, col = var)) +
  geom_line(data = filter(melted, variable == "AC"), linetype = 1, size = 1.5) +
  geom_line(data = filter(melted, variable == "OI"), linetype = 1, size = 1.5) +
  geom_line(data = filter(melted, variable == "TK"), linetype = 1, size = 1.5) +
  geom_line(data = filter(melted, variable == "OI_AC"), linetype = 1, size = 1.5) +
  geom_line(data = filter(melted, variable == "AC_TK"), linetype = 1, size = 1.5) +
  geom_line(data = filter(melted, variable == "OI_TK"), linetype = 1, size = 1.5) +
#  theme_few() +
#  the?.,mnjbhvgcfxdzsAsdrfikopl=-0  theme_fivethirtyeight() +
  theme(legend.text=element_text(size=8)) +
  scale_colour_discrete(name = "Research Topic", 
                        breaks = c("Absorptive Capacity", "Open Innovation",
                        "Tacit Knowledge", "Open Innovation & Absorptive Capacity",
                        "Absorptive Capacity & Tacit Knowledge",
                        "Open Innovation & Tacit Knowledge"),
                        labels = c("Absorptive Capacity", "Open Innovation", "Tacit Knowledge",
                        "Open Innovation & Absorptive Capacity", "Absorptive Capacity & Tacit Knowledge",
                        "Open Innovation & Tacit Knowledge")) +
  scale_x_continuous(breaks = yr) +
  scale_y_continuous(breaks = nr) +
  theme(axis.text.y = element_text(size = 8), axis.text.x = element_text(angle = 90, vjust = 0.5, size = 8), axis.title = element_text(size=12,face="bold")) +
  ylab("Published Articles") +
  xlab("Year")

dev.print(pdf,"bibliometric.pdf")
  
  
