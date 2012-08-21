pdf("qqnorm.pdf", paper = "letter", horizontal = FALSE)

y <- rt(200, df = 2)
qqnorm(y)
qqline(y, col = grey(.6))

y <- rexp(200)
qqnorm(y)
qqline(y, col = grey(.6))

y <- rnorm(200, mean = 10, sd = 4)
qqnorm(y)
qqline(y, col = grey(.6))

dev.off()
