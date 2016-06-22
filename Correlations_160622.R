#####################################################################
# Title:Correlation Explained
# Author: Steph de Silva.
# Email: steph@rex-analytics.com
# Date created: 22/06/16
# Date last altered: 22/06/16
# Attributions and acknowledgment of derivation:
# This script is due to information, tips and advice given in:
# - Generating random numbers in R: http://www.cookbook-r.com/Numbers/Generating_random_numbers/
# - http://www.sthda.com/english/wiki/correlation-matrix-a-quick-start-guide-to-analyze-format-and-visualize-a-correlation-matrix-using-r-software?utm_content=bufferb6058&utm_medium=social&utm_source=twitter.com&utm_campaign=buffer#correlation-analysis-in-r
# Purpose: This script is intended generate random data and then plot 
# frequency distributions of sample mean and z score at different sample sizes.
#########################################################################
# Data Used: Generated
# Source: the computer
# Specifically: NA
# Translation by:NA
# Date Accessed: NA
# Gutenberg Number: NA
#########################################################################
# Script Outline:
# 1. Load Libraries, generate initial population
# 2. Calculate correlations, create correlograms
#########################################################################
# 1. Load libraries, generate initial population
#########################################################################

rm(list=ls(all=TRUE)) # clear the workspace of anything else
set.seed(1234) # we may want to do this again some day. 

# Generate variables

epsilon<-rnorm(1000, mean=0, sd=1)
x<- rnorm(1000, mean=0, sd=1)
y1<-0.01*x+epsilon
y2<-0.2*x+epsilon
y3<-0.5*x+epsilon
y4<-0.9*x+epsilon
y5<-0.9*x+epsilon/2

y<-cbind(x,y1,y2,y3,y4,y5)

# Calculate the Pearson's correlation coefficient + p-values
library(Hmisc)
correlations<-cor(y, method="pearson")
pvalues_correlation<-rcorr(y, type="pearson")

print(correlations)
print(pvalues_correlation)

## Function from http://www.sthda.com/english/wiki/correlation-matrix-a-quick-start-guide-to-analyze-format-and-visualize-a-correlation-matrix-using-r-software?utm_content=bufferb6058&utm_medium=social&utm_source=twitter.com&utm_campaign=buffer#correlation-analysis-in-r

flattenCorrMatrix <- function(cormat, pmat) {
  ut <- upper.tri(cormat)
  data.frame(
    row = rownames(cormat)[row(cormat)[ut]],
    column = rownames(cormat)[col(cormat)[ut]],
    cor  =(cormat)[ut],
    p = pmat[ut]
  )
}

res<-rcorr(as.matrix(y))
flattenCorrMatrix(res$r, res$P)

library(corrplot)
corrplot(correlations, type="upper", order="hclust", tl.col="black", tl.srt=45)

#### Now create some scatter plots 

loops<- dim(y) # length of vector of examples
loops<-loops[2]-1
words<-c("Extremely weak", "Weak", "Moderate", "Strong", "Very Strong")
correlations<-round(correlations, digits=2)
rho_words<-"rho="
numbers<-c(paste(rho_words, correlations[2,1]),paste(rho_words, correlations[3,1]), paste(rho_words, correlations[4,1]), paste(rho_words, correlations[5,1]),paste(rho_words, correlations[6,1]))
for (i in 1:loops) {
  print(i)
  
  saveto=paste("~/Documents/Rex Analytics/Blog/asymptotics/Correlations/rho is", words[i])
  jpeg(file =saveto )
  plot(y[,1], y[,i+1], type="p", main=paste("Correlation: ",words[i]), sub=numbers[i], col="chartreuse4", xlab="", ylab="" )
  abline(h=0, v=0)
  dev.off() 
}


