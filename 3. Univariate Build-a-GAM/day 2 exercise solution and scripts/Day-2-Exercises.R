############# EXERCISES FROM DAY 2 OF GLM/GAM COURSE ###########################

library("mgcv")
island <- read.table("c:/temp/isolation.txt",header=T)
attach(island)
names(island)

model1 <- glm(incidence~area*isolation,binomial)
summary(model1)
model2 <- glm(incidence~area+isolation,binomial)
summary(model2)

model3 <- gam(incidence~s(area)+s(isolation),binomial)
summary(model3)

par(mfrow=c(1,2))
plot.gam(model3,residuals=T,pch=16)

model4 <- gam(incidence~s(isolation),binomial)
anova(model1,model2,test="Chi")
anova(model2,model3,test="Chi")
anova(model3,model4,test="Chi")
anova(model1,model2,model3,model4,test="Chi")

model5 <- gam(incidence~area+s(isolation),binomial)
summary(model5)

