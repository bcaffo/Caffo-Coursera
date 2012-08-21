##CI for the variance
s2 <- 105.977 ^ 2
n <- 513
alpha <- .05
qtiles <- qchisq(c(alpha/2, 1 - alpha/2), n - 1)
ival <- rev((n - 1) * s2 / qtiles)
##interval for the sd
sqrt(ival)

##plot the likelihood function
sigmaVals <- seq(90, 120, length = 1000)
likeVals <- dgamma((n - 1) * s2,
                   shape = (n - 1)/2,
                   scale =  2 * sigmaVals^2)
likeVals <- likeVals / max(likeVals)
#postscript("varianceLikelihood.eps", height = 6, width = 6, horizontal = FALSE)
plot(sigmaVals, likeVals,
     type = "l",
     frame = F,
     xlab = expression(sigma),
     ylab = "likelihood")
lines(range(sigmaVals[likeVals >= 1 / 8]), c(1 / 8, 1 / 8))
lines(range(sigmaVals[likeVals >= 1 / 16]), c(1 / 16, 1 / 16))
#dev.off()
