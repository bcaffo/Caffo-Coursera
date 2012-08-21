##example plots

##simple example
x <- 1 : 10
y <- 1 : 10
plot(x, y)
##add axis labels and a title
plot(x, y, xlab = "X axis", ylab = "Y axis")
title("This is a title")

##do the same plot without the frame
plot(x, y, xlab = "X axis", ylab = "Y axis", frame = FALSE)

##do the same plot with lines
plot(x, y, type = "l", xlab = "X axis", ylab = "Y axis", frame = FALSE)
##add points to the current plot
px <- c(4, 5)
py <- c(2, 1)
points(px, py)
##add a line with slope 1.5 and intercept 0
abline(0, 1.5)
##make the linewidth bigger
abline(0, 1.5, lwd = 2)

##plot the gamma density for alpha = 1.5, beta = 2
##get some points to plot, I pick 10,0000 points from 0 to 100
xvals <- seq(0, 10, length = 10000)
yvals <- dgamma(xvals, shape = 1.5, scale = 2)
plot(xvals, yvals, type = "l", lwd = 3, xlab = "Support", ylab = "Density", frame = FALSE)

##output the plot to a pdf
pdf("myplot.pdf")
plot(xvals, yvals, type = "l", lwd = 3, xlab = "Support", ylab = "Density", frame = FALSE)
dev.off()

##output the plot to a png
png("myplot.png")
plot(xvals, yvals, type = "l", lwd = 3, xlab = "Support", ylab = "Density", frame = FALSE)
dev.off()



