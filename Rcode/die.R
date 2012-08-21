png("die.png")#, horizontal = FALSE, paper = "letter")
par(mfrow = c(2, 2))
for (n in c(1, 2, 6, 20)){
  temp <- matrix(sample(0 : 1, n * 10000, replace = TRUE), ncol = n)
  temp <- apply(temp, 1, mean)
  temp <- (temp - .5) / (4.1833 / sqrt(6 * n))
  dty <- density(temp)
  plot(dty$x, dty$y, xlab = "", ylab = "density", type = "n", xlim = c(-3, 3), ylim = c(0, .5))
  title(paste("sample mean of", n, "obs"))
  lines(seq(-3, 3, length = 100), dnorm(seq(-3, 3, length = 100)), col = grey(.8), lwd = 3)
  lines(dty$x, dty$y, lwd = 2)
}
dev.off()

