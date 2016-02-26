#####################################################
#                                                   #
#            R script to check correlation          #
#             between continuous variables          #
#                                                   #
#####################################################

library(RColorBrewer)
library(corrplot)
library(devtools)

# set working directory

# Case study 1

# setwd("~/ownCloud/Innovation Network Analysis/Case studies/HF") # MacBook
# setwd("d:/Andrew/ownCloud/Innovation Network Analysis/Case studies/HF") # Home PC
setwd("c:/Users/ter053/ownCloud/Innovation Network Analysis/Case studies/HF") # work PC

# Case study 2

# setwd("~/ownCloud/Innovation Network Analysis/Case studies/AMR") # MacBook
# setwd("d:/Andrew/ownCloud/Innovation Network Analysis/Case studies/AMR") # Home PC
setwd("c:/Users/ter053/ownCloud/Innovation Network Analysis/Case studies/AMR") # work PC


source_url("https://raw.githubusercontent.com/aterhorst/sna/master/pre_process.R", sha1 = NULL) # pre-process data


continuous <- na.omit(node.summary[,c(4,9:25)])
correlation <- cor(continuous)

corrplot(correlation, type = "upper", order = "hclust", method = "pie",col=brewer.pal(n=8, name="RdBu"))
corrplot(correlation, type="upper", order="hclust", 
         col=brewer.pal(n=8, name="RdBu"))

# mat : is a matrix of data
# ... : further arguments to pass to the native R cor.test function
cor.mtest <- function(mat, ...) {
  mat <- as.matrix(mat)
  n <- ncol(mat)
  p.mat<- matrix(NA, n, n)
  diag(p.mat) <- 0
  for (i in 1:(n - 1)) {
    for (j in (i + 1):n) {
      tmp <- cor.test(mat[, i], mat[, j], ...)
      p.mat[i, j] <- p.mat[j, i] <- tmp$p.value
    }
  }
  colnames(p.mat) <- rownames(p.mat) <- colnames(mat)
  p.mat
}

# matrix of the p-value of the correlation

p.mat <- cor.mtest(continuous)

# Highlight insignificant value according to the significant level

corrplot(correlation, type="upper", order="hclust", 
         p.mat = p.mat, sig.level = 0.01)

# Leave blank on no significant coefficient

corrplot(correlation, type="upper", order="hclust", 
         p.mat = p.mat, sig.level = 0.01, insig = "blank")
