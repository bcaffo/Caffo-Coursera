png("binomialLikelihoods.png")
plot(0 : 1, 0 : 1, type = "n", xlab = "p", ylab = "likelihood")
plotPoints <- seq(0, 1, length = 100)
lbinom <- function(p, x, n){
  dbinom(x, n, p) / dbinom(x, n, x / n)
}
n <- 10
sapply(0 : n,
       function(x){
         lines(plotPoints, lbinom(plotPoints, x, n))
       }
       )
dev.off()

png("binomialLikelihoods2.png")
plot(0 : 1, 0 : 1, type = "n", xlab = "p", ylab = "likelihood")
x <- 3
temp <- lbinom(plotPoints, x, n)
lines(plotPoints, lbinom(plotPoints, x, n))
lines(plotPoints[temp > 1/8], rep(1/8, sum(temp > 1/8)))
lines(plotPoints[temp > 1/32], rep(1/32, sum(temp > 1/32)))
title("X = 3 successes n = 10 trials")
dev.off()

