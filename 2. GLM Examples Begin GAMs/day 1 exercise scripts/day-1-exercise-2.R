data(cars)
head(cars)
cm1 <- lm(dist ~ speed + I(speed^2),data=cars)
summary(cm1)
## None of the terms are significant, although the F-test is.
## Let's drop the intercept and see how the model fits.
cm2 <- lm(dist ~ speed + I(speed^2)-1, data=cars)
summary(cm2)
## Both terms are now significant, and the F-test is VERY significant.
## Let's drop the quadratic term as the question asks
cm3 <- lm(dist ~ speed-1, data=cars)
summary(cm3)
## speed is highly significant, and the F-test even more significant.
## Let's compare models cm2 and cm3
anova(cm2,cm3,test="Chi")
## There are significant differences between them in the
## analysis of variance table, so the more complex model (cm2) is preferred
## Let's look at AIC
AIC(cm2,cm3)
## cm2 (the quadratic model) has lower AIC and so is preferred to cm3
## Let's plot residuals for cm2
par(mfrow=c(2,2))
plot(cm2)
## There are problems with both heteroscedascity and non-normality
## But that is to be expected by introducing the quadratic term
## Perhaps we should do a GLM instead 

## The average time it takes for a driver to apply the brakes is:
b <- coef(cm2)
5280/(b[1]*60^2)
## 1.183722 seconds is the average time it takes for a driver to apply the brakes
