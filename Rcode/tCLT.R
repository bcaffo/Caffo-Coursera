pdf("tCLT.pdf", horizontal = FALSE, paper = "letter")
par(mfrow = c(2, 2))
for (n in c(2, 3, 5, 10)){
  dty <- density(tStats)
  plot(dty$x, dty$y, xlab = "", ylab = "density", type = "n", xlim = c(-3, 3), ylim = c(0, .5))
  title(paste("sample mean of", n, "obs"))
  lines(seq(-3, 3, length = 100), dnorm(seq(-3, 3, length = 100)), col = grey(.8), lwd = 3)
  lines(seq(-3, 3, length = 100), dt(seq(-3, 3, length = 100), df = n - 1), lwd = 2)
}
dev.off()
