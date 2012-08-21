likelihood <- function(p){
  dbinom(7, 8, p) / dbinom(7, 8, 7/8)
}

#postscript("birthLikelihood.eps", width = 5, height = 5, horizontal = FALSE)
pvals <- seq(0, 1, length = 1000)
lvals <- likelihood(pvals)
plot(pvals, lvals, frame = F, xlab = "p", ylab = "Likelihood", type = "l", lwd = 3)
lines(range(pvals[lvals >= 1/8]), c(1/8, 1/8), lwd = 3)
lines(range(pvals[lvals >= 1/32]), c(1/32, 1/32), lwd = 3)
#dev.off()

