pdf("coinCLT.pdf", horizontal = FALSE, paper = "letter")
par(mfrow = c(2, 2))
p <- .5
for (n in c(1, 10, 20, 50)){
  temp <- matrix(rbinom(n * 10000, 1, p), ncol = n)
  temp <- apply(temp, 1, mean)
  temp <- (temp - p) / sqrt(p * (1 - p) / n)
  dty <- density(temp)
  plot(dty$x, dty$y, xlab = "", ylab = "density", type = "n", xlim = c(-3, 3), ylim = c(0, .5))
  title(paste("sample mean of", n, "obs"))
  lines(seq(-3, 3, length = 100), dnorm(seq(-3, 3, length = 100)), col = grey(.8), lwd = 3)
  lines(dty$x, dty$y, lwd = 2)
}

p <- .1
for (n in c(1, 20, 50, 100)){
  temp <- matrix(rbinom(n * 10000, 1, p), ncol = n)
  temp <- apply(temp, 1, mean)
  temp <- (temp - p) / sqrt(p * (1 - p) / n)
  dty <- density(temp)
  plot(dty$x, dty$y, xlab = "", ylab = "density", type = "n", xlim = c(-3, 3), ylim = c(0, .5))
  title(paste("sample mean of", n, "obs"))
  lines(seq(-3, 3, length = 100), dnorm(seq(-3, 3, length = 100)), col = grey(.8), lwd = 3)
  lines(dty$x, dty$y, lwd = 2)
}
dev.off()
