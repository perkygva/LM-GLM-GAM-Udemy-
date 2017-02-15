#################### EXERCISE 4 DAY 1  ########################
install.packages("DAAG")
library("DAAG")
data(ACF1)
head(ACF1)

## First we run a plot of count by the log of endtime
plot(count ~ log(endtime), data = ACF1)

## A log scale on x-axis is appropriate because Poisson model
## will use a log link. Observe that counts increase with time.
## We use a Poisson family in the glm and look at the summary

ACF.glm <- glm(count ~ endtime, family = poisson, data = ACF1)
summary(ACF.glm)

## We look at the residual plots

par(mfrow=c(2,2))
plot(ACF.glm)

## Now we run the quadratic glm
ACF.glm2 <- glm(count ~ endtime + I(endtime^2), family = poisson, data = ACF1)
summary(ACF.glm2)

## We look at the residual plots:

par(mfrow=c(2,2))
plot(ACF.glm2)

## We compare the two models:
anova(ACF.glm,ACF.glm2, test="Chisq")
