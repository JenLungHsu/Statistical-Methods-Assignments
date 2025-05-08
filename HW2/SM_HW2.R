#install.packages('extraDistr') 
library(extraDistr)

data1 = read.csv('/Users/xurenlong/Desktop/Statistical Methods/Assignments/HW2/Set1.csv') 
data2 = read.csv('/Users/xurenlong/Desktop/Statistical Methods/Assignments/HW2/Set2.csv') 

# dataset1
hist(data1$x ,
     probability = TRUE)

x <- seq(-3, 3, 0.1)

pdf.norm <- dnorm(x, mean = 0, sd = 1)
pdf.exp <- dexp(x, rate = 2)
pdf.laplace <- dlaplace(x, mu = 0, sigma = sqrt(2)/2)
pdf.gamma <- dgamma(x, shape = 10, rate = 1/2)
pdf.chisq <- dchisq(x , df = 20)

lines(x, pdf.norm    , col = 2)
lines(x, pdf.exp     , col = 3)
lines(x, pdf.laplace , col = 4)
lines(x, pdf.gamma   , col = 5)
lines(x, pdf.chisq   , col = 6)

legend("topright", c("Normal","Exponential","Laplace","Gamma","Chi-square"), 
       lty = c(1, 1, 1, 1, 1),
       col = c(2, 3, 4, 5, 6))

# dataset2
hist(data2$x ,
     probability = TRUE)

x <- seq(5, 35, 0.1)

pdf.norm    <- dnorm(x, mean = 0, sd = 1)
pdf.exp     <- dexp(x, rate = 2)
pdf.laplace <- dlaplace(x, mu = 0, sigma = sqrt(2)/2)
pdf.gamma   <- dgamma(x, shape = 10, rate = 1/2)
pdf.chisq   <- dchisq(x , df = 20)

lines(x, pdf.norm    , col = 2)
lines(x, pdf.exp     , col = 3)
lines(x, pdf.laplace , col = 4)
lines(x, pdf.gamma   , col = 5)
lines(x, pdf.chisq   , col = 6)

legend("topright", c("Normal","Exponential","Laplace","Gamma","Chi-square"), 
       lty = c(1, 1, 1, 1, 1),
       col = c(2, 3, 4, 5, 6))
