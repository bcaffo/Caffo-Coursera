

xvals <- seq(-.5, 1.5, by = .001)
yvals <- rep(0, length(xvals))
yvals[xvals > 0 & xvals < 1] <- 1

png(filename = "uniform.png", width = 960, height = 480)
plot(xvals, yvals, type = "n", ylab = "density", xlab = "support", frame = FALSE)
lines(xvals, yvals, lwd = 3)
dev.off()




