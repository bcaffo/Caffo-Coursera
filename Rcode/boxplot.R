pdf("boxplot.pdf", paper = "letter", horizontal = FALSE)
data(ToothGrowth)
boxplot(len ~ dose, data = ToothGrowth, boxwex = 0.25, 
    at = 1:3 - 0.2, subset = supp == "VC", col = "yellow", main = "Guinea Pigs' Tooth Growth", 
    xlab = "Vitamin C dose mg", ylab = "tooth length", ylim = c(0, 
        35))
boxplot(len ~ dose, data = ToothGrowth, add = TRUE, 
    boxwex = 0.25, at = 1:3 + 0.2, subset = supp == "OJ", col = "orange")
dev.off()
