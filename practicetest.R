source("./hawkeshelp.R")

# Question 1
pbar <- 0.05
phat1 <- 0.01
phat2 <- 0.09
sigma <- sqrt(0.05*(1-0.05)/216)
z1 <- (phat1 - pbar)/sigma
z2 <- (phat2 - pbar)/sigma
GetAreaBetweenValues(z1, z2)

# Question 3
z1 <- (1417 - 1428)/(119/sqrt(49))
z2 <- (1439 - 1428)/(119/sqrt(49))
GetAreaBetweenValues(z1, z2)

# Question 4
E <- EstimateErrorForDifferenceInTwoMeans(n=c(10,14), c=0.90, s=c(14, 10), sd.assumed.equal = TRUE)
print(E)
GetConfidenceIntervalForComparisonBetweenMeans(mean=c(143, 164), n=c(10,14), c=0.90, s=c(14, 10), sd.assumed.equal = TRUE)

# Question 5
t <- GetTFromAlpha(0.90, 13)
print(t)
E <- t * (24.65 / sqrt(14))
67.58 + E
67.58 - E