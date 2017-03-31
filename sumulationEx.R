# Simulation Exercise
# Statistical Inference
# Mark Culp
# 3/25/2017


## --------
## Overview:
## --------

# This paper explores simulations on an Exponential Distribution, 
# a distribution that describes the time between events in a 
# Poisson process.  We use R's rexp(n, lambda) function to create 
# samples from an Eponential Distribution, and compare it with 
# the Central Limit Theorem (CLT).  The CLT is one of the most  
# important theorems in statistics which states that the 
# distribution or sum of a large number of independent random 
# variables tends toward a normal distribution regardless of the 
# underlying distribution.  

# First, let's create a small exponential distribution sample, and
# examine its properties:

# code start---------------------------

# Set parameters for distribution
n <- 40
lambda <- 0.2

# Create small sample
ssample <- rexp(n, lambda)

# Range of numbers in sample
range(ssample)



# An exploratory plot 

plot(ssample)
# abline(0,..)




# code end ---------------------------

## -----------
## Simulations:  
## -----------

set.seed(0)

# Generate 1,000 random deviates for an exponential distribution
sampleExpDist <- rexp(1000, lambda)

# Generate a distribution of 1000 averages of 40 random 
# deviates by sampling the exponential distribution
# and calculate the variances in each sample.
expSample = NULL
mnsExpDist = NULL
varExpDist = NULL
for (i in 1 : 1000) {
  expSample = rexp(n, lambda)
  mnsExpDist = c(mnsExpDist, mean(expSample))
  varExpDist = c(varExpDist, var(expSample))
}

# Create a histogram of the distribution of exponentials
hist(sampleExpDist)

# Create histogram of the distribution of exponential averages
hist(mnsExpDist)

## -----------------------------------
## Sample Mean versus Theoretical Mean
## -----------------------------------

# Calculate the mean of the sample exponential distribution
sampleExpDistMean <- round(mean(sampleExpDist),3)

# Calculate mean of the distribution of exponential averages
mnsDistMean <- round(mean(mnsExpDist),3)

# Calculate hypothetical mean for the exponential distribution
hypoDistMean <- round(1/lambda,3)

# Combine means in a row
rowMeans <- rbind(c(sampleExpDistMean, mnsDistMean, hypoDistMean))

rowMeans

## -------------------------------------------
## Sample Variance versus Theoretical Variance
## -------------------------------------------

# Calculate variance of the sample exponential distribution
sampleExpDistVariance <- round(var(sampleExpDist),3)

# Calculate variance of the distribution of exponential averages
mnsExpDistVariance <- round(mean(varExpDist),3)

# Calculate hypothetical mean for the exponential distribution
hypoDistVariance <- (1/lambda)^2

# Combine variances in a row
rowVar <- rbind(c(sampleExpDistVariance, mnsExpDistVariance, hypoDistVariance))

rowVar

## -----------------------------------
## Distribution: Assumpt & Conclusions
## -----------------------------------






