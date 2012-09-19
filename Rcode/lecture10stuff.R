xoc <- 132.86
soc <- 15.34
xc <- 127.44
sc <- 18.23
noc <- 8
nc <- 21
sp <- sqrt( (15.34^2 * (noc - 1) + 18.23^2 * (nc - 1)) / (noc + nc - 2))
ival <- xoc - xc + c(-1, 1)  * qt(.975, df = noc + nc - 2) * sp * sqrt(1 / noc + 1 / nc)

df <- (soc^2 / noc + sc^2 / nc)^2 / (
  (soc^2/noc)^2 / (noc - 1) + (sc^2 / nc)^2 / (nc - 1)
  )


stat <- (xoc - xc) / sp / sqrt(1 / noc + 1 / nc)

esVals <- seq(-1.5, 1.5, length = 100)
tVals <- dt(stat, df = noc + nc - 2, ncp = esVals / sqrt(1 / noc + 1 / nc))
tVals <- tVals / max(tVals)
pdf("Lecture10ESlikelihood.pdf")
plot(esVals, tVals, type = "l", frame = FALSE, xlab = "Effect Size", ylab = "Likelihood", lwd = 3)
lines(range(esVals[tVals > 1 / 8]), c(1/8, 1/8), lwd = 3)
lines(range(esVals[tVals > 1 / 16]), c(1/16, 1/16), lwd = 3)
dev.off()



