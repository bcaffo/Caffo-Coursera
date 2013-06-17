############################################################
### Does Monte Carlo sampling from the posterior of a      #
### two sample binomial with independent beta priors       #
###                                                        #
### Brian Caffo 11/1/07                                    #
###                                                        #
### To be programmed HPD intervals                         #
###                                                        #
############################################################

twoBinomPost <- function(x1, n1, x2, n2, alpha1 = 1, alpha2 = 1, beta1 = 1, beta2 = 1,
                         nosim = 1000,
                         level = .95){
  alpha1Post <- alpha1 + x1
  alpha2Post <- alpha2 + x2
  beta1Post <- beta1 + (n1 - x1)
  beta2Post <- beta2 + (n2 - x2)

  p1 <- rbeta(nosim, alpha1Post, beta1Post)
  p2 <- rbeta(nosim, alpha2Post, beta2Post)

  rd <- p2 - p1
  rr <- p2 / p1
  or <- (p2 / (1 - p2)) / (p1 / (1 - p1))

  ##see the notes at the bottom about this calculation
  posteriorDensityValuesRD <- sapply(rd,
                                     function(rdi){
                                       mean(dbeta(rdi + p1, alpha2Post, beta2Post))
                                     }
                                     )  
  posteriorDensityValuesRR <- sapply(rr,
                                     function(rri){
                                       mean(dbeta(rri * p1, alpha2Post, beta2Post) * p1)
                                     }
                                     )
  ##these are somewhat ad hoc for the other ones
  ##temp <- density(rd)
  ##posteriorDensityValuesRD <- sapply(rd, function(rdi) mean(dnorm(rdi, mean = rd, sd = temp$bw)))
  ##temp <- density(rr)
  ##posteriorDensityValuesRR <- sapply(rr, function(rri) mean(dnorm(rri, mean = rr, sd = temp$bw)))
  temp <- density(or)
  posteriorDensityValuesOR <- sapply(or, function(ori) mean(dnorm(ori, mean = or, sd = temp$bw)))
  
  postModeRD <- rd[order(posteriorDensityValuesRD)][nosim]
  postModeRR <- rr[order(posteriorDensityValuesRR)][nosim]
  postModeOR <- or[order(posteriorDensityValuesOR)][nosim]
  
  alpha <- 1 - level
  ##create posterior summaries for the difference
  equiTailRD <- quantile(rd, c(alpha / 2, (1 - alpha / 2)))
  equiTailRR <- quantile(rr, c(alpha / 2, (1 - alpha / 2)))
  equiTailOR <- quantile(or, c(alpha / 2, (1 - alpha / 2)))

  rval <- list(postMeanRD = mean(rd), postMeanRR = mean(rr), postMeanOR = mean(or),
               mcseRD = sd(rd) / sqrt(nosim), mcseRR = sd(rr) / sqrt(nosim), mcseOR = sd(or) / sqrt(nosim),
               postMedRD = median(rd), postMedRR = median(rr), postMedOR = median(or), 
               postModeRD = postModeRD, postModeRR = postModeRR, postModeOR = postModeOR,
               equiTailRD = equiTailRD,  equiTailRR = equiTailRR, equiTailOR = equiTailOR,
               p1SimValues = p1,  p2SimValues = p2, alpha1 = alpha1, beta1 = beta1, alpha2 = alpha2, beta2 = beta2,
               level = level, x1 = x1, x2 = x2, n1 = n1, n2 = n2)
  class(rval) <- "twoBinomPost"
  return(rval)
}

print.twoBinomPost <- function(output, digits = 3){
  cat("Post mn rd (mcse) = ", round(output$postMeanRD, digits), " (", round(output$mcseRD, digits), ")\n", sep = "")
  cat("Post mn rr (mcse) = ", round(output$postMeanRR, digits), " (", round(output$mcseRR, digits), ")\n", sep = "")
  cat("Post mn or (mcse) = ", round(output$postMeanOR, digits), " (", round(output$mcseOR, digits), ")\n", sep = "")
  cat("\n")
  cat("Post med rd       = ", round(output$postMedRD, digits), "\n")
  cat("Post med rr       = ", round(output$postMedRR, digits), "\n")
  cat("Post med or       = ", round(output$postMedOR, digits), "\n")  
  cat("\n")
  cat("Post mod rd       = ", round(output$postModeRD, digits), "\n")
  cat("Post mod rr       = ", round(output$postModeRR, digits), "\n")
  cat("Post mor or       = ", round(output$postModeOR, digits), "\n")  
  cat("\n")
  cat("Equi-tail rd      = ", round(output$equiTailRD, digits), "\n")
  cat("Equi-tail rr      = ", round(output$equiTailRR, digits), "\n")
  cat("Equi-tail or      = ", round(output$equiTailOR, digits), "\n")  
}

##some notes on the Bayesian computations
##u = p2 - p1
##v = p1
##u + v = p2
##v     = p1
##jacobian = 1
##therefore f(u) = int f1(v)f2(u + v)dv
##u = p2 / p1
##v = p1
##uv = p2
##v = p1
##jacobian = |v u| = v
##           |0 1|
##therefore f(u) = int f1(v)f2(uv)vdv


