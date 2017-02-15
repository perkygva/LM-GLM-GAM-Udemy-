island <- read.table("c://temp/isolation.txt",header=T)
attach(island)
names(island)

## There are two continuous explanatory variables. We have a binary
## response, so we perform a logistic regression with binomial errors.

## We will try two models and then compare them using ANOVA
## This is the more complex model with an interaction:
model1 <- glm(incidence~area*isolation,binomial)

## Then we fit a simpler model with only main effects:
model2 <- glm(incidence~area+isolation,binomial)

## Both models use a logit link because the values are between 0 and 1
## The logit link is canonical with binomial distribution

## Then we compare the two models:
anova(model1,model2,test="Chi")

## The Chi value is not significant and the simpler model 
## is not significantly worse, so we go with that one:
summary(model2)

## The estimates of their standard errors are in logits.
## Area has a significant positive effect (larger islands are more likely to be occupied)
## Isolation has a strong negative effect (isolated islands are less likely to be occupied)

## This is the minimal adequate model.

## We plot the fitted model for each variable separately:

modela <- glm(incidence~area,binomial)
modeli <- glm(incidence~isolation,binomial)

par(mfrow=c(2,2))
xv <- seq(0,9,0.01)
yv <- predict(modela,list(area=xv),type="response")
plot(area,incidence)
lines(xv,yv)
xv2 <- seq(0,10,0.1)
yv2 <- predict(modeli,list(isolation=xv2),type="response")
plot(isolation,incidence)
lines(xv2,yv2)
detach(island) ## Is a good idea to do this as a matter of habit

###############################################################################
