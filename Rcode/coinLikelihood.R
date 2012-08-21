thetaVals <- seq(0, 1, length = 1000)
maxL <- .75 ^ 3 * .25 ^1
lVals <- thetaVals ^ 3 * (1 - thetaVals) ^1 / maxL

#postscript("coinLikelihood.eps", width=4, height=4, horizontal = FALSE)
plot(thetaVals, lVals,
     type = "l",
     frame = F,
     xlab = expression(theta),
     ylab = "Likelihood",
     lwd = 2)
lines(range(thetaVals[lVals > 1 / 8]), c(1/8,1/8))
lines(range(thetaVals[lVals > 1 / 16]), c(1/16,1/16))
#dev.off()


     
