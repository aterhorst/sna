#####################################################
#                                                   #
#            R script to check correlation          #
#             between continuous variables          #
#                 Version 2016-09-19                #
#                                                   #
#####################################################

# Load requisite libraries.

library(RColorBrewer)
library(corrplot)
library(devtools)

# Pre-process network data and export files for MPNet. 
# Load exported files for correlation analysis.

amr <- na.omit(read.table("d:/Andrew/ownCloud/Innovation Network Analysis/Quantitative Data/Case 2/MPNet data/continuous_data.txt", header = T, sep = ""))
hf <- na.omit(read.table("d:/Andrew/ownCloud/Innovation Network Analysis/Quantitative Data/Case 1/MPNet data/continuous_data.txt", header = T, sep = ""))
gihh <- na.omit(read.table("d:/Andrew/ownCloud/Innovation Network Analysis/Quantitative Data/Case 3/MPNet data/continuous_data.txt", header = T, sep = ""))

# Merge into one dataset.

continuous <- rbind(hf,amr,gihh) # create single data set

# Compute correlation coefficients.

correlation <- cor(continuous)

# Plot correlation coefficients using corrplot library.

corrplot(correlation, type = "upper", order = "hclust", method = "pie",col=brewer.pal(n=8, name="RdBu"))
corrplot(correlation, type="upper", order="hclust", col=brewer.pal(n=8, name="RdBu"))
corrplot(correlation, type="upper", col=brewer.pal(n=8, name="RdBu"), tl.col="black")

# Compute p-values.

## mat : is a matrix of data
## ... : further arguments to pass to the native R cor.test function

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

# Matrix of the p-value of the correlation

p.mat <- cor.mtest(continuous)

# Highlight insignificant value according to the significance level.

corrplot(correlation, type="upper", order="hclust", p.mat = p.mat, sig.level = 0.01)

# Leave blank on no significant coefficient.

corrplot(correlation, type="upper", order="original", p.mat = p.mat, sig.level = 0.01, insig = "blank")

# Customise further.

col <- colorRampPalette(c("#BB4444", "#EE9988", "#FFFFFF", "#77AADD", "#4477AA"))
corrplot(correlation, method="color", col=col(200),  
         type="upper", order="hclust", 
         addCoef.col = "black", # Add coefficient of correlation
         tl.col="black", tl.srt=45, #Text label color and rotation
         # Combine with significance
         p.mat = p.mat, sig.level = 0.01, insig = "blank", 
         # hide correlation coefficient on the principal diagonal
         diag=FALSE)

# End.