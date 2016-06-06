#####################################################
#                                                   #
#            R script to check correlation          #
#             between continuous variables          #
#                                                   #
#####################################################

library(RColorBrewer)
library(corrplot)
library(devtools)

# pre-process network data and export files for MPNet. Load exported files for correlation analysis.

amr <- na.omit(read.table("~/ownCloud/Innovation Network Analysis/Case studies/AMR/continuous_data.txt", header = T, sep = ""))
hf <- na.omit(read.table("~/ownCloud/Innovation Network Analysis/Case studies/HF/continuous_data.txt", header = T, sep = ""))
gihh <- na.omit(read.table("~/ownCloud/Innovation Network Analysis/Case studies/GIHH/continuous_data.txt", header = T, sep = ""))

continuous <- rbind(hf,amr,gihh) # create single data set
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

# Highlight insignificant value according to the significance level

corrplot(correlation, type="upper", order="hclust", 
         p.mat = p.mat, sig.level = 0.01)

# Leave blank on no significant coefficient

corrplot(correlation, type="upper", order="original", 
         p.mat = p.mat, sig.level = 0.01, insig = "blank")
