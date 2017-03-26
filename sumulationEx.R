# Simulation Exercise
# Statistical Inference
# Mark Culp
# 3/25/2017

## Overview:

# Set parameters for distribution
n <- 40
lambda <- 0.2

# Generate random deviates for distribution
rexp(n, lambda)

# An exploratory plot 
# . . .

## Simulations:  

## Sample Mean versus Theoretical Mean

## Sample Variance versus Theoretical Variance

## Distribution: Assumpt & Conclusions

hist(runif(1000))

#---

mns = NULL
for (i in 1:1000) mns = c(mns, mean(runif(40)))
hist(mns)




