pdf("qqnorm.pdf", paper = "letter", horizontal = FALSE)

y <- rt(200, df = 2)
qqnorm(y)
qqline(y, col = grey(.6))

xvals <- seq(-4, 4, length = 100)
plot(range(xvals), range(dnorm(xvals), dt(xvals, df = 2)), type = "n", xlab = "", ylab = "", frame = FALSE)
lines(xvals, dnorm(xvals), lwd = 3, col = gray(.6))
lines(xvals, dt(xvals, df = 2), lwd = 3)

y <- rexp(200)
qqnorm(y)
qqline(y, col = grey(.6))

xvals <- seq(-3, 4, length = 100)
plot(range(xvals), range(dnorm(xvals), dexp(xvals)), type = "n", xlab = "", ylab = "", frame = FALSE)
lines(xvals, dnorm(xvals), lwd = 3, col = gray(.6))
lines(xvals, dexp(xvals), lwd = 3)


y <- rnorm(200, mean = 10, sd = 4)
qqnorm(y)
qqline(y, col = grey(.6))

dev.off()
