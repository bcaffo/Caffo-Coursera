#postscript("lln.eps", horizontal = FALSE, height = 4, width = 4)
nosim <- 100
dat <- rnorm(nosim)
plot(1 : nosim, cumsum(dat) / (1 : nosim),
     type = "l",
     xlab = "iteration",
     ylab = "average",
     frame = FALSE)
abline(h = 0, lty = 2)
#dev.off()
