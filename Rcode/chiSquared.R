pdf("chisquared.pdf", paper = "letter", horizontal = FALSE)
par(mfrow = c(2, 2))
xvals <- seq(0, 20, length = 200)
for (df in c(1, 2, 5, 10)){
  plot(xvals, dchisq(xvals, df = df), type = "l")
  title(paste("df = ", df, sep = ""))
}
dev.off()
