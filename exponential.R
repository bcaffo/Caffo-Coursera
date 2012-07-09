pdf("exponential.pdf")
xvals <- seq(0, 20, length = 1000) 
plot(xvals, dexp(xvals, 1/5), xlab = "Survival time in years",
     ylab = "density",
     frame = FALSE,
     type = "l")
polygon(c(xvals[xvals >= 6], rev(xvals[xvals >= 6])),
        c(dexp(xvals[xvals >= 6], 1/5), rep(0, sum(xvals >= 6))),
        col = grey(.5)
        )
dev.off()

