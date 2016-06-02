#####################################################################
# Title:CLT vs the LLN: Seeing the difference
# Author: Steph de Silva.
# Email: steph@rex-analytics.com
# Date created: 02/06/16
# Date last altered: 02/06/16
# Attributions and acknowledgment of derivation:
# This script is due to information, tips and advice given in:
# - Generating random numbers in R: http://www.cookbook-r.com/Numbers/Generating_random_numbers/
# - random sampling in R: https://stat.ethz.ch/R-manual/R-devel/library/base/html/sample.html
# - density plots: http://stackoverflow.com/questions/6939136/how-to-overlay-density-plots-in-r
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
# 2. Create do loop to run different samples
# 3. Inside loop: second loop: calculate sample mean and z-score and plot
#########################################################################
# 1. Load libraries, generate initial population
#########################################################################

rm(list=ls(all=TRUE)) # clear the workspace of anything else
set.seed(1234) # we may want to do this again some day. 
library(sm) # we need this later
library(ggplot2)

# Let's create a populations of data 
# Let population be drawn from the U(0,1) distribution.
# We will call population 1 popn_x
# Our population can have 100000 observations because it's a 
# nice round big number.

popn_x<-runif(100000,min=0,max=1)

# Let's create a name for the population that R can recognise. 
# This will be useful later.

popn_x_name<-"Uniform[0,1] Population"

# Let's see what the population looks like

hist(popn_x, main = paste("Histogram of" , popn_x_name), xlab = "Values taken by x", ylab="Frequency")
# it doesn't look nice and normal because it is not

#########################################################################
# 2. Create a do loop to run different sample sizes
#########################################################################

# let's look at different sample sizes
# create a vector to store these sample sizes for us, call it sample_size because that's simple.

sample_size<-c(5, 10, 20, 50, 100, 500)

# now we need to open a do loop because we don't want to have to C&P code everywhere

for (i in sample_size) {
  print("the sample size is")
  print(i)
  
  # we want to sample 
  # we will call the samples sample_x
  
  sample_x<-sample(popn_x, size=i, replace =TRUE, prob = NULL)

  # What does a sample look like?
  # hist(sample_x, main = paste("Sample from Normal Distribution: n=", i), xlab = "Values taken by x", ylab="Frequency")

  # Now actually, we want to take many samples of this size and look
  # at the behaviour of these statistics. Let's take 500 samples from the 
  # population at this size. We will need to open another do loop for that.
  # We should create somewhere to store the sample means and z scores
  # we want to calculate in each sample.
  
  mean_x<-rep(0,500)
  zscore_x<-rep(0,500)

  for (j in 1:500) {
    sample_x<-sample(popn_x, size=i, replace =TRUE, prob = NULL)
  
    # Let's calculate sample means and z-scores
    # We can use the r function for the mean
    mean_x[j]=mean(sample_x)
    # Th z-score is sqrt(n)*(x-bar-mu)/sigma.
    # For our normal population x, mu=1/2 and sigma=sqrt(1/12)
    zscore_x[j]=sqrt(i)*(mean_x[j]-0.5)/(sqrt(1/12))
  }
  
  # Now we have 500 sample means for the population 
  # we have 500 z scores as well
  # What do our z scores look like?
  # hist(zscore_x, main = paste("Z score from Normal Distribution: n=", i), xlab = "Values taken by Z score", ylab="Frequency")
  # What do our sample means look like?
  # hist(mean_x, main = paste("Sample Mean from Normal Distribution: n=", i), xlab = "Values taken by sample mean", ylab="Frequency")

  # Let's compare the density of the sample mean and z score together
  dat <- data.frame(values = mean_x) 
  lines = rep("Sample Mean", each = j)
  
  plot.dens<-function(plot.item, title.item){
    p<-ggplot(dat, aes(x = values, fill = lines)) + geom_density(alpha = 0.5)+ xlim(-4, 4)+ylim(0,22)
    theme(legend.position="bottom") 
    print(p)
  }
  
  saveto=paste("~/Documents/Rex Analytics/Blog/asymptotics/LLN/n is", i)
  jpeg(file =saveto )
  plot.dens(plot.item=dat, title.item=paste("Sample from Normal Distribution: n=", i))
  dev.off()  

  # Let's look at the density of the z score 
  dat <- data.frame(values = zscore_x) 
  lines = rep("Z score", each = j)
  
  plot.dens<-function(plot.item, title.item){
    p<-ggplot(dat, aes(x = values, fill = lines)) + geom_density(alpha = 0.5)+ xlim(-4, 4)+ylim(0,0.5)
    theme(legend.position="bottom") 
    print(p)
  }
  
  saveto=paste("~/Documents/Rex Analytics/Blog/asymptotics/CLT/n is", i)
  jpeg(file =saveto )
  plot.dens(plot.item=dat, title.item=paste("Sample from Normal Distribution: n=", i))
  dev.off()  
  
  
  
  # Combine z score and mean into one data frame
  dat <- data.frame(values = c(mean_x, zscore_x)) 
  lines = rep(c("Sample Mean", "Z score"), each = j)
  
  plot.dens<-function(plot.item, title.item){
    p<-ggplot(dat, aes(x = values, fill = lines)) + geom_density(alpha = 0.5)+ xlim(-4, 4)+ylim(0,22)
    theme(legend.position="bottom") 
    print(p)
  }
  
  saveto=paste("~/Documents/Rex Analytics/Blog/asymptotics/CLT vs LLN/n is", i)
  jpeg(file =saveto )
  plot.dens(plot.item=dat, title.item=paste("Sample from Normal Distribution: n=", i))
  dev.off()
}