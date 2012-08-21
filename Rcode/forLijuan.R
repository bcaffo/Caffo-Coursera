nosim <- 100
myexp <- rexp(nosim)

means <- rep(NA, nosim)
for(i in nosim)
{ x<-rexp(20)
  a<-mean(x)
  means[i] <- a
}
  
