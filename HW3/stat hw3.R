#install.packages('extraDistr') 
library(extraDistr)

dat1 = read.csv('C:/Users/RE612/OneDrive/桌面/stat/Set1.csv')
dat2 = read.csv('C:/Users/RE612/OneDrive/桌面/stat/Set2.csv')


### estimation ###
mme.norm = function(data){
  mu = mean(data)
  sigma2 = sum((data-mean(data))^2)/length(data)
  return(c(mu, sigma2))
}

mme.exp = function(data){
  lambda = 1/mean(data)
  return(lambda)
}

mme.laplace = function(data){
  location = mean(data)
  scale = sqrt( sum((data-mean(data))^2)/(2*length(data)) )
  return(c(location, scale))
}

mme.gamma = function(data){
  shape = (length(data)*mean(data)^2)/sum((data-mean(data))^2)
  rate = (length(data)*mean(data))/sum((data-mean(data))^2)
  return(c(shape, rate))
}

mme.chi2 = function(data){
  k = mean(data)
  return(k)
}


### PDF ###

# dataset1
hist(dat1$x ,probability = TRUE)

x <- seq(-3, 3, 0.01)

pdf.norm    <- dnorm(x, mean = mme.norm(dat1$x)[1], sd = sqrt(mme.norm(dat1$x)[2]))
pdf.exp     <- dexp(x, rate = mme.exp(dat1$x))
pdf.laplace <- dlaplace(x, mu = mme.laplace(dat1$x)[1], sigma = sqrt(2*mme.laplace(dat1$x)[2]^2) )
pdf.gamma   <- dgamma(x, shape = mme.gamma(dat1$x)[1], rate = mme.gamma(dat1$x)[2])
pdf.chisq   <- dchisq(x , df = mme.chi2(dat1$x))

lines(x, pdf.norm    , col = 2)
lines(x, pdf.exp     , col = 3)
lines(x, pdf.laplace , col = 4)
lines(x, pdf.gamma   , col = 5)
lines(x, pdf.chisq   , col = 6)

legend("topright", c("Normal","Exponential","Laplace","Gamma","Chi-squared"), 
       lty = c(1, 1, 1, 1, 1),
       col = c(2, 3, 4, 5, 6))

# dataset2
hist(dat2$x , probability = TRUE)

x <- seq(5, 35, 0.01)

pdf.norm    <- dnorm(x, mean = mme.norm(dat2$x)[1], sd = sqrt(mme.norm(dat2$x)[2]))
pdf.exp     <- dexp(x, rate = mme.exp(dat2$x))
pdf.laplace <- dlaplace(x, mu = mme.laplace(dat2$x)[1], sigma = sqrt(2*mme.laplace(dat2$x)[2]^2) )
pdf.gamma   <- dgamma(x, shape = mme.gamma(dat2$x)[1], rate = mme.gamma(dat2$x)[2])
pdf.chisq   <- dchisq(x , df = mme.chi2(dat2$x))

lines(x, pdf.norm    , col = 2)
lines(x, pdf.exp     , col = 3)
lines(x, pdf.laplace , col = 4)
lines(x, pdf.gamma   , col = 5)
lines(x, pdf.chisq   , col = 6)

legend("topright", c("Normal","Exponential","Laplace","Gamma","Chi-squared"), 
       lty = c(1, 1, 1, 1, 1),
       col = c(2, 3, 4, 5, 6))


##### CDF #####

# dataset1
plot(ecdf(dat1$x))
x <- seq(-3, 3, 0.01)

pdf.norm    <- pnorm(x, mean = mme.norm(dat1$x)[1], sd = sqrt(mme.norm(dat1$x)[2]))
pdf.exp     <- pexp(x, rate = mme.exp(dat1$x))
pdf.laplace <- plaplace(x, mu = mme.laplace(dat1$x)[1], sigma = sqrt(2*mme.laplace(dat1$x)[2]^2) )
pdf.gamma   <- pgamma(x, shape = mme.gamma(dat1$x)[1], rate = mme.gamma(dat1$x)[2])
pdf.chisq   <- pchisq(x , df = mme.chi2(dat1$x))

lines(x, pdf.norm    , col = 2)
lines(x, pdf.exp     , col = 3)
lines(x, pdf.laplace , col = 4)
lines(x, pdf.gamma   , col = 5)
lines(x, pdf.chisq   , col = 6)

legend("bottomright", c("Normal","Exponential","Laplace","Gamma","Chi-squared"), 
       lty = c(1, 1, 1, 1, 1),
       col = c(2, 3, 4, 5, 6))

# dataset2
plot(ecdf(dat2$x))
x <- seq(5, 35, 0.01)

pdf.norm    <- pnorm(x, mean = mme.norm(dat2$x)[1], sd = sqrt(mme.norm(dat2$x)[2]))
pdf.exp     <- pexp(x, rate = mme.exp(dat2$x))
pdf.laplace <- plaplace(x, mu = mme.laplace(dat2$x)[1], sigma = sqrt(2*mme.laplace(dat2$x)[2]^2) )
pdf.gamma   <- pgamma(x, shape = mme.gamma(dat2$x)[1], rate = mme.gamma(dat2$x)[2])
pdf.chisq   <- pchisq(x , df = mme.chi2(dat2$x))

lines(x, pdf.norm    , col = 2)
lines(x, pdf.exp     , col = 3)
lines(x, pdf.laplace , col = 4)
lines(x, pdf.gamma   , col = 5)
lines(x, pdf.chisq   , col = 6)

legend("bottomright", c("Normal","Exponential","Laplace","Gamma","Chi-squared"), 
       lty = c(1, 1, 1, 1, 1),
       col = c(2, 3, 4, 5, 6))





##### testing the suitable estimated distributions #####

#install.packages('goftest')
library(goftest)

set.seed(123)

# dataset1

# KS test 
ks.norm    = ks.test(dat1$x, rnorm(1000, mean = mme.norm(dat1$x)[1], sd = sqrt(mme.norm(dat1$x)[2])) )
ks.exp     = ks.test(dat1$x, rexp(1000, rate = mme.exp(dat1$x)) )
ks.laplace = ks.test(dat1$x, rlaplace(1000 , mu = mme.laplace(dat1$x)[1], sigma = sqrt(2*mme.laplace(dat1$x)[2]^2)) )
ks.gamma   = ks.test(dat1$x, rgamma(1000 , shape = mme.gamma(dat1$x)[1], rate = mme.gamma(dat1$x)[2]))
ks.chisq   = ks.test(dat1$x, rchisq(1000 , df = mme.chi2(dat1$x))) 

# AD test 
ad.norm    = ad.test(dat1$x, 'pnorm', mean = mme.norm(dat1$x)[1], sd = sqrt(mme.norm(dat1$x)[2]) )
ad.exp     = ad.test(dat1$x, 'pexp' , rate = mme.exp(dat1$x))
ad.laplace = ad.test(dat1$x, 'plaplace',mu = mme.laplace(dat1$x)[1], sigma = sqrt(2*mme.laplace(dat1$x)[2]^2))
ad.gamma   = ad.test(dat1$x, 'pgamma', shape = mme.gamma(dat1$x)[1], rate = mme.gamma(dat1$x)[2])
ad.chi2    = ad.test(dat1$x, 'pchisq', df = mme.chi2(dat1$x))

# CVM test
cvm.norm    = cvm.test(dat1$x, 'pnorm', mean = mme.norm(dat1$x)[1], sd = sqrt(mme.norm(dat1$x)[2]) )
cvm.exp     = cvm.test(dat1$x, 'pexp' , rate = mme.exp(dat1$x))
cvm.laplace = cvm.test(dat1$x, 'plaplace',mu = mme.laplace(dat1$x)[1], sigma = sqrt(2*mme.laplace(dat1$x)[2]^2))
cvm.gamma   = cvm.test(dat1$x, 'pgamma', shape = mme.gamma(dat1$x)[1], rate = mme.gamma(dat1$x)[2])
cvm.chi2    = cvm.test(dat1$x, 'pchisq', df = mme.chi2(dat1$x))

dist = data.frame(
  Normal = c(ks.norm$p.value , ad.norm$p.value , cvm.norm$p.value) ,
  Exponential = c(ks.exp$p.value , ad.exp$p.value , cvm.exp$p.value),
  Laplace = c(ks.laplace$p.value , ad.laplace$p.value , cvm.laplace$p.value),
  Gamma = c(ks.gamma$p.value , ad.gamma$p.value , cvm.gamma$p.value),
  Chisq = c(ks.chisq$p.value , ad.chi2$p.value , cvm.chi2$p.value)
)
row.names(dist) = c('KS test','AD test','CVMtest')
dist



# dataset2

# KS test 
ks.norm    = ks.test(dat2$x, rnorm(1000, mean = mme.norm(dat2$x)[1], sd = sqrt(mme.norm(dat2$x)[2])) )
ks.exp     = ks.test(dat2$x, rexp(1000, rate = mme.exp(dat2$x)) )
ks.laplace = ks.test(dat2$x, rlaplace(1000 , mu = mme.laplace(dat2$x)[1], sigma = sqrt(2*mme.laplace(dat2$x)[2]^2)) )
ks.gamma   = ks.test(dat2$x, rgamma(1000 , shape = mme.gamma(dat2$x)[1], rate = mme.gamma(dat2$x)[2]))
ks.chisq   = ks.test(dat2$x, rchisq(1000 , df = mme.chi2(dat2$x))) 

# AD test 
ad.norm    = ad.test(dat2$x, 'pnorm', mean = mme.norm(dat2$x)[1], sd = sqrt(mme.norm(dat2$x)[2]) )
ad.exp     = ad.test(dat2$x, 'pexp' , rate = mme.exp(dat2$x))
ad.laplace = ad.test(dat2$x, 'plaplace',mu = mme.laplace(dat2$x)[1], sigma = sqrt(2*mme.laplace(dat2$x)[2]^2))
ad.gamma   = ad.test(dat2$x, 'pgamma', shape = mme.gamma(dat2$x)[1], rate = mme.gamma(dat2$x)[2])
ad.chi2    = ad.test(dat2$x, 'pchisq', df = mme.chi2(dat2$x))

# CVM test
cvm.norm    = cvm.test(dat2$x, 'pnorm', mean = mme.norm(dat2$x)[1], sd = sqrt(mme.norm(dat2$x)[2]) )
cvm.exp     = cvm.test(dat2$x, 'pexp' , rate = mme.exp(dat2$x))
cvm.laplace = cvm.test(dat2$x, 'plaplace',mu = mme.laplace(dat2$x)[1], sigma = sqrt(2*mme.laplace(dat2$x)[2]^2))
cvm.gamma   = cvm.test(dat2$x, 'pgamma', shape = mme.gamma(dat2$x)[1], rate = mme.gamma(dat2$x)[2])
cvm.chi2    = cvm.test(dat2$x, 'pchisq', df = mme.chi2(dat2$x))

dist = data.frame(
  Normal = c(ks.norm$p.value , ad.norm$p.value , cvm.norm$p.value) ,
  Exponential = c(ks.exp$p.value , ad.exp$p.value , cvm.exp$p.value),
  Laplace = c(ks.laplace$p.value , ad.laplace$p.value , cvm.laplace$p.value),
  Gamma = c(ks.gamma$p.value , ad.gamma$p.value , cvm.gamma$p.value),
  Chisq = c(ks.chisq$p.value , ad.chi2$p.value , cvm.chi2$p.value)
)
row.names(dist) = c('KS test','AD test','CVMtest')
dist

