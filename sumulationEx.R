# Simulation Exercise
# Statistical Inference
# Mark Culp
# 3/25/2017


## --------
## Overview:
## --------

# This paper explores simulations on an Exponential Distribution, 
# a distribution that describes the time between events in a 
# Poisson process.  We use R's rexp() function to create 
# samples from an Eponential Distribution, and compare it with 
# the Central Limit Theorem (CLT).  The CLT is one of the most  
# important theorems in statistics which states that the 
# distribution or sum of a large number of independent random 
# variables tends toward a normal distribution regardless of the 
# underlying distribution.  

# First, let's create an exponential distribution sample, and
# examine its properties:

# code start---------------------------

set.seed(0)

# Set parameters for our exponential distribution
n <- 1000
lambda <- 0.2

# Create an exponential distribution
expDist <- rexp(n, lambda)

# Check the range of numbers in distribution
range(expDist)

# Create a histogram of the distribution of exponentials
hist(expDist)
# plot(sampleExpDist)
# abline(0,..)



# code end ---------------------------

# This histogram shows how the density of the 
# exponential distribution changes according 
# to its rate parameter.  It is downward sloping, 
# at a decreasing rate.  It appears nothing like 
# a normal bell-shaped distribution.

## -----------
## Simulations:  
## -----------

# Now let's run a simulation where we create 
# 1,000 samples of 40 exponential distribution 
# deviates, and calculate the means of the 1,000 
# samples.  These means or averages from the 
# exponential distribution will create a very 
# different distribution.

set.seed(0)

# Reset parameters for our exponential distribution
n <- 40
lambda <- 0.2

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

# Create histogram of the distribution of exponential averages
hist(mnsExpDist)

# Now this distribution appears to be much more normal 
# and bell-shaped.  This is the Central Limits Theorem 
# at work. Lets compare the means and variances of the 
# two distributions with the theoretical mean and variance 
# of an exponential distribution. 

## -----------------------------------
## Sample Mean versus Theoretical Mean
## -----------------------------------

# This distribution of averages is centered at 4.99 while 
# the theoretical center, or mean is 5.  This is consistent 
# with the Law of Large Numbers (LLN) which indicates that 
# a distribution will converge on its theoretical or expected 
# outcomes as the sample size increases.  

# The distribution of 1,000 means is drawn from a sampling 
# of 40,000 exponential distribution deviates.  It approximates 
# the mean better than the sample of 1,000 random deviates 
# drawn directly from the exponential distribution.

# Calculate the mean of the exponential distribution
expDistMean <- round(mean(expDist),3)

# Calculate mean of the distribution of exponential averages
mnsDistMean <- round(mean(mnsExpDist),3)

# Calculate hypothetical mean for the exponential distribution
hypoDistMean <- round(1/lambda,3)

# Combine means in a row
rowMeans <- rbind(c(expDistMean, mnsDistMean, hypoDistMean))

rowMeans

## -------------------------------------------
## Sample Variance versus Theoretical Variance
## -------------------------------------------

# Calculate variance of the exponential distribution
expDistVariance <- round(var(expDist),3)

# Calculate variance of the distribution of exponential averages
mnsExpDistVariance <- round(mean(varExpDist),3)

# Calculate hypothetical mean for the exponential distribution
hypoDistVariance <- (1/lambda)^2

# Combine variances in a row
rowVar <- rbind(c(expDistVariance, mnsExpDistVariance, hypoDistVariance))

rowVar

## -----------------------------------
## Distribution: Assumpt & Conclusions
## -----------------------------------

# We assumed we were dealing with a properly normalized 
# distribution of independent and identically distributed 
# (IID) random variables.  A collection of random variables 
# is independent if they are statistically unrelated from one 
# and another.  They are identically distributed if they 
# were drawn from the same population distribution.  Use of 
# the rexp() function ensures these properties. 

# Our initial histogram of the exponential distribution of 
# averages appeared normal.  We now conduct some tests to 
# assess that fact.

# Standard deviation of distribution
# of exponential averages
sdMns <- sd(mnsExpDist)

# 90 quantile for distribution of 
# exponential averages
mns90quan <- qnorm(0.90, mean = mnsDistMean, sd = sdMns)

# Calculate actual number of random deviates
# from the distribution of exponential averages
# below the 90th quantile
sum(mnsExpDist < mns90quan)

# 75 quantile for distribution of 
# exponential averages
mns75quan <- qnorm(0.75, mean = mnsDistMean, sd = sdMns)

# Calculate actual number of random deviates
# from the distribution of exponential averages
# below the 75th quantile
sum(mnsExpDist < mns75quan)


# We know this distribution is normal because ...

t.test(mnsExpDist)
# 95 percent confidence interval:
# 4.940888 5.038467

# Now we calculate a 95% confidence interval
# for the distribution of averages
5 + c(-1,1) * qnorm(0.975) * sdMns/sqrt(1000)
# [1] 4.95127 5.04873

mean(mnsExpDist)
sum(mnsExpDist > 3.449017 & mnsExpDist < 6.530983)

mnsDistMean + c(-1,1) * qnorm(0.975) * sdMns
# [1] 3.449017 6.530983

