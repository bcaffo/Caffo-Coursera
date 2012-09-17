
var(sample(1 : 6, 10000, replace = TRUE))

barplot(table(sample(1 : 6, 1000, replace = TRUE)), col = "lightblue")

n <- 10
nosim <- 1000
xbar <- apply(matrix(sample(1 : 6, n * nosim, replace = TRUE), nosim), 1, mean)
barplot(table(xbar), col = "lightblue")
var(xbar)


library(rgl)
library(mvtnorm)
sigma <- matrix(c(8, .25 * 8, .25 * 8, 8), 2, 2)
xvals <- seq(-10, 10, length = 100)
yvals <- seq(-10, 10, length = 100)
zvals <- apply(expand.grid(xvals, yvals), 1, function(w) dmvnorm(w, mean = c(0, 0), sigma = sigma)) 
persp3d(x = xvals, y = yvals, z = zvals, col = "lightblue")
planes3d(0, 1, 0, -5, col = grey(.8))

thetavals <- seq(0, 2 * pi, length = 100)
zvals <- seq(0, 1 / pi, length = 100)
temp <- expand.grid(thetavals, zvals)
theta <- temp[],1]
z <- temp[,2]
x <- cos(theta)
y <- sin(theta)
persp3d(x, y, z)  
    



