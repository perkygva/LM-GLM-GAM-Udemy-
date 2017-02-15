##### Generalized Additive Models (GAMs) provide nonparametric smoothing.
##### They allow you to view the shape of the relationship, without prejudging the particular parametric form.
##### See Wikipedia  http://en.wikipedia.org/wiki/Generalized_additive_model

#### generalized additive models ############################## page 614
rm(list = ls()) # removes previous variables
ozone.data <- read.table("http://www.bio.ic.ac.uk/research/mjcraw/therbook/data/ozone.data.txt",header=T)
## attach data file (makes it #2 in search path in workspace):
attach(ozone.data)
## view the variable names:
names(ozone.data)
## view the structure of the data set:
str(ozone.data)
## view the first six records:
head(ozone.data)

## examine all the bivariate plots fitting a non-parametric smoothed lowess line to each:
pairs(ozone.data,panel=function(x,y){ points(x,y);lines(lowess(x,y)) })
## we will need to use Wood's GAM package mgcv:
library(mgcv)
## fit all three explanatory variables using a non-parametric smoother s():
m1=gam(ozone~s(rad)+s(temp)+s(wind))
## view the results:
summary(m1)

## Note that intercept is parametric coefficient but
## all explanatory variables are fitted as smooth terms.
## All explanatory variables are significant, but radiation is least significant
## So we fit a second model, omitting radiation:
m2=gam(ozone~s(temp)+s(wind))
summary(m2) # without s(rad)

## We compare the two GAMs:
anova(m1,m2,test="F")# significant so keep more complex model

## radiation should remain in the model since deleting radiation
## caused a highly significant increase in deviance.

## Let's investigate the possibility of an interaction
## between wind and temperature:

m3 <- gam(ozone~s(temp)+s(wind)+s(rad)+s(wind,temp))
summary(m3)

## Interaction is highly significant, but main effect of temperature is cancelled out.

## Let's compare m1 and m3:
anova(m1,m3,test="F")

## The more complex model with the interaction term is better
## We inspect the fit of model m3 (with interaction term):
par(mfrow=c(2,2))
plot(m3,residuals=T,pch=16)
