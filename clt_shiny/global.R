
# 지수분포 --------------------
# This R code uses rexp to create 1000 simulated samples, each of size 40 with lambda = .02
lambda <- .02
mean   <- 1/lambda
sigma  <- 1/lambda
nosims <- 1000
n      <- 40
semean <- sigma/sqrt(n)
popvar <- sigma^2

set.seed(10109)
mns    <- NULL
for (i in 1:nosims) mns <-  c(mns, mean(rexp(n,lambda)))
stdev   <- NULL
for (i in 1:nosims) stdev <-  c(stdev, sd(rexp(n,lambda)))

means <- cumsum(mns)/(1:nosims)

library(ggplot2)

LLN_tbl <- tibble( x = 1:nosims, 
                   y = means)

LLN_tbl %>% 
  ggplot(aes(x=x, y=y)) +
    geom_hline(yintercept = mean, color = "blue") +
    geom_point(size = 0.7) +
    geom_line(size = 0.5) +
    labs(x = "관측점 수",
         y= "누적평균") +
    theme_bw()  
  

for (i in 1:nosims) {
    mns <-  c(mns, mean(rexp(n,lambda)))
}



