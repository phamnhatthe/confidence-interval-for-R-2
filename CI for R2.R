# sample data
setwd("C:/hw/330/CI for R2 simple linear reg")
x<-
"distance concentration
2 11.5
4 10.2
6 10.3
8 9.68
10 9.32"
writeLines(x,"dat.txt")
distcon<-read.table("dat.txt",header=T,sep=" ")
mod <- lm(distcon$concentration ~ distcon$distance)
# 2 methods for generating confidence interval for R^2
# First, bootstrapping
library(boot)
getR2 <- function(data,indices){
  d <- data[indices,]
  return(summary(lm(d$concentration ~ d$distance))$r.squared)
}
bootobject <- boot(distcon, getR2, R = 30000)
# the warnings you get are probably from getting 1 for your R^2. 
# The warning said the result as such is not reliable. Just filter out the 1's
boot_keep <- bootobject$t[-which(bootobject$t == 1)]
# confidence interval
quantile(boot_keep,c(.025,.975))

# Second, CI.Rsq from psychometric package
library(psychometric)
# confidence interval
CI.Rsq(summary(mod)$r.squared,n=5,k=1)
# comparison
# 95% CI from bootstrapping: 0.5504587 0.9999411 
# 95% CI from CI.Rsq function: 0.7629687 0.9722689
# The CI from bootstrapping is wider
