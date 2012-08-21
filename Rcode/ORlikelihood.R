ORlikelihood <- function(x, thetaVals = seq(0, 2, length = 100)){
  if (!is.matrix(x)) stop("x must be a matrix")
  if (!is.numeric(x)) stop("x must be numeric")
  if (any(dim(x) != c(2,2))) stop("x must be 2x2")
  rs <- rowSums(x)
  cs <- colSums(x)
  n <- sum(x)
  mLower <- max(0, rs[1] + cs[1] - n)
  mUpper <- min(rs[1], cs[1])
  z <- cs[1]
  
  likeVals <- sapply(thetaVals,
                     function(theta){
                       denom <- sum(sapply(mLower : mUpper,
                                           function(u){
                                             (choose(rs[1], u) *
                                              choose(rs[2], z - u) *
                                              theta ^ u)}
                                           )
                                    )
                       theta^x[1,1] / denom
                     }
                     )
  return(data.frame(or = thetaVals, like = likeVals / max(likeVals)))
}

postscript("fisherLikelihood.ps", horizontal = FALSE, paper = "letter")
dat <- ORlikelihood(matrix(c(3, 1, 1, 3), 2), seq(0,10, length = 100))
plot(dat$or, dat$like, type = "l", xlab = "or", ylab = "likelihood")
lines(range(dat$or[dat$like > 1/8]), c(1/8,1/8))
title("Conditional likelihood for odds ratio")
dev.off()
