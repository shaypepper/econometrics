CalculateExpectedValueWhenBetting <- function(p, win.value, loss.value, n=1){
  expectedValue = round(win.value * p + loss.value * (1 - p), 2)
  round(expectedValue * n, 2)
}

IsDiscreteProbability <- function(...){
  sum(...) == 1
}

GetVariance <- function(values, probabilities){
  expectedValues = values * probabilities
  mu = sum(expectedValues)
  distances = ((values - mu)**2 )* probabilities
  variance = sum(distances)
  variance
}

GetExpectedValue <- function(values, probabilities){
  expectedValues = values * probabilities
  sum(expectedValues)
}

GetProbabilityForBinomial <- function(p, n, lower.bound=1, upper.bound=lower.bound){
  totalP <- 0
  i <- lower.bound
  while (i <= upper.bound) {
    totalP <- totalP + choose(n, i) * (p ** i) * ((1-p) ** (n-i))
    i <- i + 1
  }
  round(totalP, 4)
}

GetProbabilityForPoisson <- function(lambda, lower.bound=0, upper.bound=lower.bound){
  totalP <- 0
  i <- lower.bound
  while (i <= upper.bound) {
    totalP <- totalP + (exp(-lambda) * (lambda** i)) / factorial(i)
    i <- i + 1
  }
  round(totalP, 4)
}

GetProbabilityForHyperGeometric <- function(k, n, N, lower.bound=0, upper.bound=lower.bound){
  totalP <- 0
  i <- lower.bound
  while (i <= upper.bound) {
    totalP <- totalP + choose(k, i) * choose(N-k, n-i)/ choose(N, n)
    i <- i + 1
  }
  round(totalP, 4)
}

GetRightTail <- function(z, ...){
  round( pnorm(z, lower.tail=FALSE, ...), 4)
}

GetLeftTail <- function(z, ...){
  round( pnorm(z, ...), 4)
}

GetOuterTails <- function(z1, z2, ...){
  p <- pnorm(z2, lower.tail=FALSE, ...) + pnorm(z1, ...)
  round(p, 4)
}

GetAreaBetweenValues <- function(z1, z2, ...){
  p <- pnorm(z2, ...) - pnorm(z1, ...)
  round(p, 4)
}

GetZFromInnerArea <- function(alpha, ...){
  p <- qnorm(.5 - alpha/2, ...)
  round(p, 4)
}

GetZFromTail <- function(alpha, ...){
  p <- qnorm(alpha/2, ...)
  round(p, 4)
}

GetTFromAlpha <- function(alpha, df, ...){
  p <- qt(p=alpha/2, df=df, ...)
  round(-p, 4)
}

GetFsFromAlpha <- function(alpha, df1, df2){
  a <- qf(alpha/2, df1, df2, lower.tail = FALSE)
  b <- qf(1 - (alpha/2), df1, df2, lower.tail = FALSE)
  c(a, b)
}

EstimatePopulationProportion <- function(p, n, lower.bound=FALSE, upper.bound=FALSE, ...){
  sd <- sqrt(p*(1-p)/n)
  print(sd)
  if (!upper.bound) {
    new.p <- GetRightTail(lower.bound, mean=p, sd=sd, ...)
  } else if (!lower.bound) {
    new.p <- GetLeftTail(upper.bound, mean=p, sd=sd, ...)
  } else {
    new.p <- GetAreaBetweenValues(lower.bound, upper.bound, mean=p, sd=sd, ...)
  }
  new.p 
}

EstimatePopulationVariance <- function(values){
  mu <- mean(values)
  variance <- sum((values-mu) ** 2) / (length(values) - 1)
  variance
}

EstimatePopulationStandardDeviation <- function(values){
  sqrt(EstimatePopulationVariance(values))
}

ConstructConfidenceIntervalForVariance <- function(n, sd, c){
  alpha <- (1-c)/2
  numerator <- (n-1) * (sd**2)
  lower.bound <- numerator / qchisq(alpha, n-1)
  upper.bound <- numerator / qchisq(1 - (alpha), n-1)
  c(upper.bound, lower.bound)
}

ConstructConfidenceIntervalForStandardDeviation <- function(n, sd, c){
  interval.for.variance <- ConstructConfidenceIntervalForVariance(n, sd, c)
  interval.for.standard.deviation <- sqrt(interval.for.variance)
  interval.for.standard.deviation
}

GetCriticalValuesForConfidenceInterval <- function(n, c){
  alpha <- (1-c)/2
  lower <- qchisq(alpha, n-1)
  upper <- qchisq(1 - alpha, n-1)
  round(c(lower, upper), 3)
}

GetSampleSizeToEstimateMu <- function(sd, level.of.confidence, desired.error){
  alpha = 1 - level.of.confidence
  z <- GetZFromTail(alpha)
  (z*sd / desired.error)**2
}

GetSampleSizeToEstimateStandardDeviation <- function(sd, level.of.confidence, desired.error){
  alpha = 1 - level.of.confidence
  z <- GetZFromTail(alpha)
  (z*sd / desired.error)**2
}

ApproximateProbabilityForBinomial <- function(p, n, lower.bound=FALSE, upper.bound=FALSE, ...){
  mu <- p*n
  if (mu < 5 | (1-p)*n < 5) {
    print("np or n(1-p) is less than five. cannot approximate")
    return()
  }
  sd <- sqrt(p*n*(1-p))
  if (!upper.bound) {
    new.p <- GetRightTail(lower.bound, mean=mu, sd=sd, ...)
  } else if (!lower.bound) {
    new.p <- GetLeftTail(upper.bound, mean=mu, sd=sd, ...)
  } else {
    new.p <- GetAreaBetweenValues(lower.bound, upper.bound, mean=mu, sd=sd, ...)
  }
  new.p
}

EstimateErrorForDifferenceInTwoMeans <- function(n, c, sigma=c(FALSE), s=c(FALSE), sd.assumed.equal=FALSE){
  alpha = 1 - c
  if (sigma[1]){
    z <- -GetZFromTail(alpha)
    v1 <- ((sigma[1]**2)/n[1])
    v2 <- ((sigma[2]**2)/n[2])
    E <- z * sqrt(v1+ v2)
  } else if (!sd.assumed.equal){
    t <- round(GetTFromAlpha(alpha, min(n)-1),4 )
    v1 <- ((s[1]**2)/n[1])
    v2 <- ((s[2]**2)/n[2])
    E <- t * sqrt(v1+ v2)
  } else {
    t <- GetTFromAlpha(alpha, n[1] + n[2] - 2)
    a <- sqrt( 
      ((n[1]-1)*s[1]**2 + (n[2]-1)*s[2]**2)  
      /  (n[1] + n[2] - 2) 
    )
    b <- sqrt((1 / n[1]) + (1 / n[2]))
    E <- t*a*b
  }
  E
}

EstimateMeanForDependentSamples <- function(x1, x2){
  d.bar <- sum(x2-x1)/length(x1)
  d.bar
}

EstimateStandardDeviationForDependentSamples <- function(x1, x2){
  mean <- EstimateMeanForDependentSamples(x1, x2)
  n <- length(x1)
  numerator <- sum( (x2-x1-mean) ** 2)
  sqrt(numerator / (n - 1))
}

EstimateDifferenceInVariance <- function(s2, n, c, sd=FALSE){
  if (sd){
    s2 <- s2 ** 2
  }
  point.estimate <- s2[1] / s2[2]
  f <- GetFsFromAlpha(1-c, n[1]-1, n[2]-1)
  point.estimate*(1/f)
}

EstimateErrorForDifferenceInDependentMeans <- function(x1, x2, c){
  n <- length(x1)
  alpha <- 1 - c
  t <- GetTFromAlpha(alpha, n-1)
  s <- EstimateStandardDeviationForDependentSamples(x1, x2)
  t * s / sqrt(n)
}

GetConfidenceIntervalForComparisonBetweenDependentMeans <- function(x1, x2, c){
  E <- EstimateErrorForDifferenceInDependentMeans(x1, x2, c)
  mean <- EstimateMeanForDependentSamples(x1, x2)
  c(mean - E, mean + E)
}

GetConfidenceIntervalForComparisonBetweenMeans <- function(mean, ...){
  E <- EstimateErrorForDifferenceInTwoMeans(...)
  point.estimate = mean[1] - mean[2]
  c(point.estimate - E, point.estimate + E)
}

EstimateErrorForDifferenceInProportions <- function(p, n, c){
  alpha <- 1-c
  z <- -GetZFromTail(alpha)
  a <- p[1]*(1-p[1])/n[1]
  b <- p[2]*(1-p[2])/n[2]
  z * sqrt(a + b)
}

GetConfidenceIntervalForDifferenceInProportions <- function(p, n, c){
  E <- EstimateErrorForDifferenceInProportions(p, n, c)
  point.estimate <- (p[1]-p[2])
  c(point.estimate - E, point.estimate + E)
}
