# R code for chapter 5 of Wood (2006) "GAMs: An Introduction with R"

## 5.1 Cherry trees again

library(mgcv)
data(trees)
ct1<-gam(Volume~s(Height)+s(Girth),
         family=Gamma(link=log),data=trees)
ct1
par(mfrow=c(1,2))
plot(ct1,residuals=TRUE)

## 5.1.1 Finer control of gam

ct2 <- gam(Volume~s(Height,bs="cr")+s(Girth,bs="cr"),
           family=Gamma(link=log),data=trees)
ct2
par(mfrow=c(2,2))
plot(ct1,residuals=TRUE)
plot(ct2,residuals=TRUE)

ct3 <- gam(Volume ~ s(Height)+s(Girth,bs="cr",k=20),
           family=Gamma(link=log),data=trees)
ct3

ct4 <- gam(Volume ~ s(Height) + s(Girth),
           family=Gamma(link=log),data=trees,gamma=1.4)
ct4
par(mfrow=c(2,2))
plot(ct1,residuals=TRUE)
plot(ct4,residuals=TRUE)

## 5.1.2 Smooths of several variables

ct5 <- gam(Volume ~ s(Height,Girth,k=25),
           family=Gamma(link=log),data=trees)
ct5
plot(ct5,too.far=0.15)

ct6 <- gam(Volume ~ te(Height,Girth,k=5),
           family=Gamma(link=log),data=trees)
ct6
plot(ct6,too.far=0.15)

## 5.1.3 Parametric model terms

gam(Volume~Height+s(Girth),family=Gamma(link=log),data=trees)
trees$Hclass <- factor(floor(trees$Height/10)-5,
                labels=c("small","medium","large"))

ct7 <- gam(Volume ~ Hclass+s(Girth),
           family=Gamma(link=log),data=trees)
par(mfrow=c(1,2))
plot(ct7,all.terms=T)

anova(ct7)
AIC(ct7)
summary(ct7)

## 5.2 Brain imaging example
## 5.2.1 Preliminary modeling
library(gamair)
data(brain)
brain

## Each row corresponds to one voxel

brain <- brain[brain$medFPQ>=3e-5,] # exclude 2 outliers
m0 <- gam(medFPQ~s(Y,X,k=100),data=brain)
gam.check(m0) 

e <- residuals(m0); fv <- fitted(m0)
lm(log(e^2)~log(fv))

m1<-gam(medFPQ^.25~s(Y,X,k=100),data=brain)
gam.check(m1)

m2<-gam(medFPQ~s(Y,X,k=100),data=brain,family=Gamma(link=log),optimizer="perf")

mean(fitted(m1)^4);mean(fitted(m2));mean(brain$medFPQ)
m2

vis.gam(m2,plot.type="contour",too.far=0.03,
        color="gray",n.grid=60,zlim=c(-1,2),main="model m2 image plot and overlaid contours on scale of linear predictor")

## 5.2.2 Would an additive structure be better?
m3 <- gam(medFPQ~s(Y,k=30)+s(X,k=30),data=brain,
          family=Gamma(link=log),optimizer="perf")
m3
summary(m2)
summary(m3)
# Compare models m2 and m3
anova(m3,m2,test="F")

m4 <- gam(medFPQ~s(Y,k=30)+s(X,k=30)+s(Y,X,k=100),data=brain,
      family=Gamma(link=log),optimizer="perf")

## 5.2.3 Isotropic or tensor product smooths?

tm<-gam(medFPQ~te(Y,X,k=10),data=brain,family=Gamma(link=log),
        optimizer="perf")
tm1<-gam(medFPQ~s(Y,k=10,bs="cr")+s(X,bs="cr",k=10),
         data=brain,family=Gamma(link=log),optimizer="perf")

tm1
tm
anova(tm1,tm,test="F")

## So we plot them again
vis.gam(tm1,plot.type="contour",too.far=0.03,
        color="gray",n.grid=60,zlim=c(-1,2),main="model tm1 image plot and overlaid contours")

vis.gam(tm,plot.type="contour",too.far=0.03,
        color="gray",n.grid=60,zlim=c(-1,2),main="model tm image plot and overlaid contours")

## 5.3 Air pollution in Chicago example
library(mgcv)
library(gamair)
data(chicago)
ap0 <- gam(death~s(time,bs="cr",k=200)+pm10median+so2median+
           o3median+tmpd,data=chicago,family=poisson)
gam.check(ap0)

par(mfrow=c(2,1))
plot(ap0,n=1000)  # n increased to make plot smooth
plot(ap0,residuals=TRUE,n=1000)

chicago$death[3111:3123]

ap1<-gam(death~s(time,bs="cr",k=200)+s(pm10median,bs="cr")+
     s(so2median,bs="cr")+s(o3median,bs="cr")+s(tmpd,bs="cr"),
     data=chicago,family=poisson)
gam.check(ap1)

par(mfrow=c(3,2))
plot(ap1,n=1000)

lag.sum <- function(a,l0,l1)
## l0 is the smallest lag, l1 the largest
{ n<-length(a)
  b<-rep(0,n-l1)
  for (i in 0:(l1-l0)) b <- b + a[(i+1):(n-l1+i)]
  b
}
death<-chicago$death[4:5114]
time<-chicago$time[4:5114]
o3 <- lag.sum(chicago$o3median,0,3)
tmp <- lag.sum(chicago$tmpd,0,3)
pm10 <- lag.sum(log(chicago$pm10median+40),0,3)
so2 <- lag.sum(log(chicago$so2median+10),0,3)

ap2 <- gam(death ~ s(time,bs="cr",k=200) +
                   te(o3,tmp,pm10,k=c(8,8,6)),family=poisson)
gam.check(ap2)

ap3 <- gam(death ~ s(time,bs="cr",k=200) + te(o3,tmp,k=8) +
                   s(pm10,bs="cr",k=6),family=poisson)
par(mfrow=c(3,1))
plot(ap3)

## 5.6.1 Package 'gam' (note need to run chicago pre-proc first)

detach(package:mgcv)  
library(gam)
install.packages("akima")
library(akima) # needed for plotting
bfm <- gam(death~s(time,df=140)+lo(o3,tmp,span=.1),
           family=poisson,control=gam.control(bf.maxit=150))
summary(bfm)

## 5.6.2 Package 'gss'
install.packages("gss")
library(gss)
ssm <- gssanova1(death~time+o3*tmp,family="poisson",nbasis=200)

summary(ssm)

tp <- seq(min(time),max(time),length=500)
fvt <- predict(ssm,newdata=data.frame(time=tp,
               o3=rep(mean(o3),500),tmp=rep(mean(tmp),500)),
               include=list("time"))
plot(tp,fvt,type="l",xlab="time",ylab="time effect")

m <- 40
o3m <- seq(min(o3),max(o3),length=m)
tmpm <- seq(min(tmp),max(tmp),length=m)
tmpp <- rep(tmpm,rep(m,m))
o3p <- rep(o3m,m)
pd <- data.frame(time=rep(0,m*m),o3=o3p,tmp=tmpp)
fv <- predict(ssm,newdata=pd,include=list("o3","tmp","o3:tmp"))
library(mgcv)
ind <- exclude.too.far(o3p,tmpp,o3,tmp,dist=0.04)
fv[ind] <- 0
persp(o3m,tmpm,matrix(fv,m,m),phi=30,theta=-30,zlab="o3*tmp",
      xlab="o3",ylab="tmp")
