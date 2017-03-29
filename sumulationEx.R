# Simulation Exercise
# Statistical Inference
# Mark Culp
# 3/25/2017


## --------
## Overview:
## --------

# Set parameters for distribution
n <- 40
lambda <- 0.2



# An exploratory plot 
# . . .

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






