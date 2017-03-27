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

set.seed(0)

# Generate random deviates for an exponetial distribution
sampleDist <- rexp(n, lambda)

# An exploratory plot 
# . . .

## -----------
## Simulations:  
## -----------



## -----------------------------------
## Sample Mean versus Theoretical Mean
## -----------------------------------

# Calculate mean of the sample distribution
sampleDistMean <- mean(sampleDist)

# Calculate hypothetical mean for the exponential distribution
hypoDistMean <- 1/lambda

## -------------------------------------------
## Sample Variance versus Theoretical Variance
## -------------------------------------------

# Calculate variance of the sample distribution
sampleDistVariance <- var(sampleDist)

# Calculate hypothetical mean for the exponential distribution
hypoDistVariance <- (1/lambda)^2

## -----------------------------------
## Distribution: Assumpt & Conclusions
## -----------------------------------

hist(runif(1000))

# ---

mns = NULL
for (i in 1:1000) mns = c(mns, mean(runif(40)))
hist(mns)




