xval <- seq(-3.2, 3.2, length = 1000)
yval<- dnorm(xval)

plot(xval, yval, type = "l", axes = FALSE, frame = FALSE, lwd = 3, xlab = "", ylab = "")

x <- seq(0, 2, length = 100)
polygon(c(x, rev(x)),c(dnorm(x), rep(0, length(x))), col = "salmon")
text(mean(x), mean(dnorm(xval)), "47.5%", cex = 2)

x <- seq(-2, 0, length = 100)
polygon(c(x, rev(x)),c(dnorm(x), rep(0, length(x))), col = "royalblue")
text(mean(x), mean(dnorm(xval)), "47.5", cex = 2)

plot(xval, yval, type = "l", axes = FALSE, frame = FALSE, lwd = 3, xlab = "", ylab = "")
x <- seq(-1.96, 3.2, length = 100)
polygon(c(x, rev(x)),c(dnorm(x), rep(0, length(x))), col = "salmon")
text(mean(x), mean(dnorm(xval)), "97.5%", cex = 2)
x <- seq(-3.2, -1.96, length = 100)
polygon(c(x, rev(x)),c(dnorm(x), rep(0, length(x))), col = "royalblue")
text(mean(x), mean(dnorm(x)) + .03, "2.5%", cex = 2)
text(-1.96, 0, "-1.96", cex = 2)


plot(xval, yval, type = "l", axes = FALSE, frame = FALSE, lwd = 3, xlab = "", ylab = "")
x <- seq(1.96, 3.2, length = 100)
polygon(c(x, rev(x)),c(dnorm(x), rep(0, length(x))), col = "salmon")
text(mean(x), mean(dnorm(x))+.02, "2.5%", cex = 2)
x <- seq(-3.2, 1.96, length = 100)
polygon(c(x, rev(x)),c(dnorm(x), rep(0, length(x))), col = "royalblue")
text(mean(x), mean(dnorm(xval)), "97.5%", cex = 2)
text(1.96, 0, "1.96", cex = 2)

plot(xval, yval, type = "l", axes = FALSE, frame = FALSE, lwd = 3, xlab = "", ylab = "")
x <- seq(1.96, 3.2, length = 100)
polygon(c(x, rev(x)),c(dnorm(x), rep(0, length(x))), col = "salmon")
text(mean(x), mean(dnorm(x))+.02, "2.5%", cex = 2)
x <- seq(-3.2, -1.96, length = 100)
polygon(c(x, rev(x)),c(dnorm(x), rep(0, length(x))), col = "salmon")
text(mean(x), mean(dnorm(x)) + .03, "2.5%", cex = 2)
text(0, mean(dnorm(xval)), "95%", cex = 2)


plot(xval, yval, type = "l", axes = FALSE, frame = FALSE, lwd = 3, xlab = "", ylab = "")
x <- seq(qnorm(.95), 3.2, length = 100)
polygon(c(x, rev(x)),c(dnorm(x), rep(0, length(x))), col = "salmon")
text(mean(x), mean(dnorm(x))+.02, "5%", cex = 2)
x <- seq(-3.2, qnorm(.95), length = 100)
polygon(c(x, rev(x)),c(dnorm(x), rep(0, length(x))), col = "royalblue")
text(mean(x), mean(dnorm(xval)), "95%", cex = 2)

png("bkg.png", 1920, 1080)
showCols1 <- function(bg = "black", cex = 1.5, srt = 30) {
  m <- ceiling(sqrt(n <- length(cl <- colors())))
  length(cl) <- m*m; cm <- matrix(cl, m)
  ##
  require("graphics")
  op <- par(mar=rep(0,4), ann=FALSE, bg = bg); on.exit(par(op))
  plot(1:m,1:m, type="n", axes=FALSE)
  text(col(cm), rev(row(cm)), cm,  col = cl, cex=cex, srt=srt)
}
showCols1()
dev.off()

