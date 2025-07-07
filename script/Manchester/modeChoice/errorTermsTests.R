library(evd)
library(tidyverse)

### Script to explore the sampling of nested logit error terms using the R evd package ###
# Useful guidance: https://stackoverflow.com/questions/71199270/simulate-nested-logit-errors

# 2 in nest
asy_list <- list(0,0,c(1,1))
vdep <- c(0.999)
deviates <- data.frame(rmvevd(10000, dep = vdep, asy = asy_list, model = "alog", d = 2))

# 3 in nest
asy_list <- list(0,0,0,c(0,0),c(0,0),c(0,0),c(1,1,1))
vdep <- c(1,1,1,1)
deviates <- data.frame(rmvevd(1000, dep = vdep, asy = asy_list,model = "alog", d = 3))

ggplot(deviates,aes(x = X1)) + geom_density()

d <- 3
sim <- c(0.03214 , 0.31669 , 0.23059)
p <- c(0,1,0)
x <- matrix(1/sim, ncol = d, byrow = TRUE)