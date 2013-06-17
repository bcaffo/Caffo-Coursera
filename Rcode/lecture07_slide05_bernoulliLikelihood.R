#postscript("bernoulliLikelihood.eps", width=4, height=4, horizontal = FALSE)
n <- 4
xvals <- seq(0, 1, length = 1000)
plot(c(0, 1), c(0, 1), type = "n", frame = FALSE, xlab = "p", ylab = "likelihood")
for (i in 0 : n){
  ml <-  (i / n) ^ i * (1 - i / n) ^ (n - i)
  likelihood <- xvals ^ i * (1 - xvals) ^ (n - i) / ml
  lines(xvals, likelihood, type = "l", lwd = 2)
}
#dev.off()
