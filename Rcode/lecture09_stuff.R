library(rgl)
library(mvtnorm)

x <- rnorm(10)


sigmaVals <- seq(0.1, 4, by = .1)
muVals <- seq(-2, 2, by = .1)
zVals <- apply(expand.grid(muVals, sigmaVals), 1, function(z) prod(dnorm(x, mean = z[1], sd = z[2])))
zVals <- zVals / max(zVals)

muProfile <- sapply(muVals, function(mu) {(sum( (x - mu) ^ 2) / length(x))^(-length(x) / 2)})
muProfile <- muProfile / max(muProfile)

persp3d(x = muVals, y = sigmaVals, z = zVals, col = "lightblue")
lines3d(muVals, rep(0, length(muVals)), muProfile, lwd = 3)


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

