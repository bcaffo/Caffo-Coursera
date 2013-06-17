#postscript("expectation.eps", width=4, height=4, horizontal = FALSE)
par(mfrow = c(2, 2), mai = rep(0, 4))
x <- c(.1, .3, .7, .9)
probs <- c(.25, .25, .25, .25)
e <- sum(x * probs)
plot(c(0, 1), c(-.1, 1),
     type = "n",
     frame = FALSE,
     axes = FALSE,
     xlab = "",
     ylab = "")
lines(range(x), c(0, 0), lwd = 5)
for (i in 1 : length(x)){lines(c(x[i], x[i]), c(0, probs[i]), lwd = 6)} 
polygon(c(e - .05, e, e + .05), c(-.1, 0, -.1), col = "black")

##

x <- c(.1, .7, .8, .9)
probs <- c(.25, .25, .25, .25)
e <- sum(x * probs)
plot(c(0, 1), c(-.1, 1),
     type = "n",
     frame = FALSE,
     axes = FALSE,
     xlab = "",
     ylab = "")
lines(range(x), c(0, 0), lwd = 5)
for (i in 1 : length(x)){lines(c(x[i], x[i]), c(0, probs[i]), lwd = 6)} 
polygon(c(e - .05, e, e + .05), c(-.1, 0, -.1), col = "black")

##
x <- c(.1, .5, .7, .9)
probs <- c(.6, .2, .15, .05)
e <- sum(x * probs)
plot(c(0, 1), c(-.1, 1),
     type = "n",
     frame = FALSE,
     axes = FALSE,
     xlab = "",
     ylab = "")
lines(range(x), c(0, 0), lwd = 5)
for (i in 1 : length(x)){lines(c(x[i], x[i]), c(0, probs[i]), lwd = 6)} 
polygon(c(e - .05, e, e + .05), c(-.1, 0, -.1), col = "black")


##
x <- c(.1, .7, .8, .9)
probs <- c(.05, .15, .2, .6)
e <- sum(x * probs)
plot(c(0, 1), c(-.1, 1),
     type = "n",
     frame = FALSE,
     axes = FALSE,
     xlab = "",
     ylab = "")
lines(range(x), c(0, 0), lwd = 5)
for (i in 1 : length(x)){lines(c(x[i], x[i]), c(0, probs[i]), lwd = 6)} 
polygon(c(e - .05, e, e + .05), c(-.1, 0, -.1), col = "black")
#dev.off()
