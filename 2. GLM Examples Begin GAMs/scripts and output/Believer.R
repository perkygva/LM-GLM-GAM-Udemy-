## Faith and Gender
## Enter the data and check it:
a1 <- data.frame(y=(c(435,147,375,134)),gender=as.factor(c("F","F","M","M")),faith=as.factor(c(1,0,1,0)))
a1

## Gender and faith are both factor variables
## The following fits the model and checks that the model matrix is as expected:

mod.0 <- glm(y~gender+faith,data=a1,family=poisson)
model.matrix(mod.0)

## Now we look at the fitted model object mod.0:
mod.0

fitted(mod.0)

## The fit appears to be quite close, and it would be surprising if a model
## with interactions between faith and gender did significantly better

## The following fits the model, checks the model matrix
## and prints the fitted model object:

mod.1 <- glm(y~gender*faith,data=a1,family=poisson)
model.matrix(mod.1)

mod.1

## We test for evidence for an interaction between gender and faith.
## The null hypothesis that mod.0 is correct is tested against the more general
## alternative that mod.1 is correct, using analysis of deviance:

anova(mod.0,mod.1,test="Chisq")

## A p-value of 0.69 suggests there is no evidence to reject model 0 and the
## hypothesis of no association between gender and belief in the afterlife
