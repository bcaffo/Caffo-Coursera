rrTest <- function(n1, n2, pi1, pi2, alpha = .05){
  powerWald <- 0
  powerScore <- 0
  for (x in 1 : (n1-1)){
    for (y in 1 : (n2-1)){
      p1 <- x / n1
      p2 <- y / n2
      p <- (x + y) / (n1 + n2)
      est <- log(p1 / p2)
      seWald <- sqrt((1 - p1) / p1 / n1 + (1 - p2) / p2 / n2)
      seScore <- sqrt((1 - p) / p / n1 + (1 - p) / p / n2)
      z <- qnorm(1 - alpha / 2)
      zWald <- est / seWald
      zScore <- est /seScore
      prob <- dbinom(x, n1, pi1) * dbinom(y, n2, pi2)
      if (zWald >= z) powerWald <- powerWald + prob
      if (zScore >= z) powerScore <- powerScore + prob
    }
  }
  return(c(powerWald = powerWald, powerScore = powerScore))
}

n1 <- 20
n2 <- 20
pi1 <- .5
pi2 <- .5
rrTest(n1, n2, pi1, pi2)

rrVals <- seq(1, 1.5, by = .01)
pi2 <- .5
print(dbinom(n1, n1, rrVals[length(rrVals)] * pi2))
output <- t(sapply(rrVals,
                 function(rr){
                   pi1 <- rr * pi2
                   rrTest(n1, n2, pi1, pi2)
                 }
                 ))
plot(range(rrVals), range(output), type = "n", frame = F, xlab = "RR", ylab = "Power")
lines(rrVals, output[,1], lty = 1)
lines(rrVals, output[,2], lty = 2)
abline(h = .05)
title(paste("pi2 = ", pi2, ", n1 = ", n1, ", n2 = ", n2))
legend(min(rrVals), max(output), c("Wald", "Score"), lty = 1 : 2)
