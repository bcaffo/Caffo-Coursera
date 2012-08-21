#postscript("hist.eps", height = 5, width = 5, horizontal = FALSE)
data(islands)
hist(islands, , col="gray", labels = TRUE, ylim = c(0, 45))
dev.off()

#postscript("histLog10.eps", height = 5, width = 5, horizontal = FALSE)
hist(log10(islands), , col="gray", labels = TRUE, ylim = c(0, 25))
dev.off()

#postscript("dotChart.eps", height = 8, width = 8, horizontal = FALSE)
dotchart(log(islands, 10),
         main = "islands data: log10(area) (log10(sq. miles))")
#dev.off()

#postscript("dotPlot.eps", height = 6, width = 6, horizontal = FALSE)
attach(InsectSprays)
plot(c(.5, 6.5), range(count),
     type = "n",
     xlab = "Spray",
     ylab = "Count",
     frame = FALSE,
     axes = FALSE)
sprayTypes <- unique(spray)
axis(1, at = 1 : 6, labels = as.character(sprayTypes))
axis(2)
for (i in 1 : length(sprayTypes)){
  y <- count[spray == sprayTypes[i]]
  n <- sum(spray == sprayTypes[i])
  points(jitter(rep(i, n), amount = .1), y, pch = 21, cex = 1.5, bg = "blue") 
  lines(i + c(.15, .30), rep(mean(y), 2), lwd = 3)
  lines(rep(i + .225, 2), mean(y) + c(-1.96, 1.96) * sd(y) / sqrt(n), lwd = 3)
}
#dev.off()

#postscript("boxplot.eps", height = 6, width = 6, horizontal = FALSE)
boxplot(count ~ spray, data = InsectSprays, col = "grey")
#dev.off()

#postscript("boxplotBad.eps", height = 6, width = 6, horizontal = FALSE)
boxplot(rt(500, 2))
#dev.off()

#postscript("kde.eps", height = 6, width = 6, horizontal = FALSE)
data(faithful)
d <- density(faithful$eruptions, bw = "sj")
d
plot(d)
#dev.off()

