##############  CHAPTER 3 QUESTION #1
## polynomial fits
## create some data:
set.seed(1)
## uniform x from approx .xx to 3.14:
x <- sort(runif(40)*10)^.5
## uniform y from approx .68 to 1
y <-sort(runif(40))^0.1
## create a sequence of 200 equally spaced values from min(x) to max(x)
xx <- seq(min(x),max(x),length=200)
plot(x,y)
b <- lm(y~poly(x,5))
lines(xx,predict(b,data.frame(x=xx)))
b10 <- lm(y~poly(x,10))
lines(xx,predict(b10,data.frame(x=xx)))
## splines fits
sb <- function(x,xk) { abs(x-xk)^3}
q <- 11
xk <- ((1:(q-2)/(q-1))*10)^.05
## lazy person's formula construction ...
form <- paste("sb(x,xk[",1:(q-2),"])",sep="",collapse="+")
form <- paste("y~x+",form)
bform <- lm(formula(form))
lines(xx,predict(bform,data.frame(x=xx)),col=3)
##################################################
## QUESTION #2
## x,y, and xx from previous question
b1 <- lm(form)
plot(x,y)
lines(xx,predict(b1,data.frame(x=xx)),col=4)
X <- model.matrix(b1)  # extract model matrix
beta <- solve(t(X)%*%X,t(X)%*%y,tol=0)
b1$coefficients <- beta # trick for simple prediction
lines(xx,predict(b1,data.frame(x=xx)),col=5)
##  Upping the basis dimension to 11 also makes the normal
##  equations estimates perform very badly
