par(mfrow=c(1,1))
test1 <- function(x,z,sx=0.3,sz=0.4)
  {(pi**sx*sz)*(1.2*exp(-(x-0.2)^2/sx^2-(z-0.3)^2/sz^2)+0.8*exp(-(x-0.7)^2/sx^2-(z-0.8)^2/sz^2))}
n <- 500
x <- runif(n);z <- runif(n);
y <- test1(x,z)+rnorm(n)*0.1
b4 <- gam(y~s(x,z))
vis.gam(b4)
################################################

