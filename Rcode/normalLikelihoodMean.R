data(sleep)
g1 <- sleep$extra[1 : 10]
g2 <- sleep$extra[11 : 20]
difference <- g2 - g1
mn <- mean(difference)
s <- sd(difference)
n <- 10

#postscript("normalLikelihoodMean.eps", height = 6, width = 6, horizontal = FALSE)
tStat <- sqrt(n) * mn / s
esVals <- seq(0, 1, length = 1000)
likVals <- dt(tStat, n - 1, ncp = n * esVals)
likVals <- likVals / max(likVals)
plot(esVals, likVals, type = "l", xlab = expression(mu / sigma), ylab = "likelihood", frame = FALSE)
lines(range(esVals[likVals>1/8]), c(1/8,1/8))
lines(range(esVals[likVals>1/16]), c(1/16,1/16))
#dev.off()

muVals <- seq(0, 3, length = 1000)
likVals <- sapply(muVals,
                  function(mu){
                    (sum((difference - mu)^2) /
                     sum((difference - mn)^2)) ^ (-n/2) 
                  }
                  )
#postscript("normalProfileLikelihoodMean.eps", height = 6, width = 6, horizontal = FALSE)
plot(muVals, likVals, type = "l", xlab = expression(mu), ylab = "likelihood", frame = FALSE)
lines(range(muVals[likVals>1/8]), c(1/8,1/8))
lines(range(muVals[likVals>1/16]), c(1/16,1/16))
#dev.off()
