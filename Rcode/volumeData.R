sbpNOC <- c(115, 112, 107, 119, 115, 138, 126, 105, 104, 115)
sbpOC  <- c(128, 115, 106, 128, 122, 145, 132, 109, 102, 117)

ratio <- sbpOC / sbpNOC
logRatio <- log(ratio)
exp(mean(logRatio))

round(mean(logRatio) + c(-1, 1) * qt(.975, 9) * sd(logRatio) / 3, 3)
round(exp(mean(logRatio) + c(-1, 1) * qt(.975, 9) * sd(logRatio) / 3), 3)
