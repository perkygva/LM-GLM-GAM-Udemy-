# p87 A Poisson regression epidemic model

# Data provided by Venables and Ripley
y <- c(12,14,33,50,67,74,123,141,165,204,253,246,240)
t <- 1:13
plot(t+1980,y,xlab="Year",ylab="New AIDS Cases",ylim=c(0,280))

# Initial GLM model of counts of new cases modeled as a function of time
m0 <- glm(y~t,poisson)
m0

# Plot the residuals for the initial GLM
par(mfrow=c(2,2))
plot(m0)

# Add a quadratic term with the time variable
m1 <- glm(y~t+I(t^2),poisson)
par(mfrow=c(2,2))
plot(m1)
summary(m1)

# Perform an analysis of deviance of the two models
anova(m0,m1,test="Chisq")

# Calculate point estimate for 95% confidence interval of beta_1
beta.1 <- summary(m1)$coefficients[2,]
ci <- c(beta.1[1]-1.96*beta.1[2],beta.1[1]+1.96*beta.1[2])
ci # print 95% CI for beta_1

# Calculate 95% confidence bands for beta_1
new.t <- seq(1,13,length=100)
fv <- predict(m1,data.frame(t=new.t),se=TRUE)
plot(t+1980,y,xlab="Year",ylab="New AIDS Cases",ylim=c(0,280))
lines(new.t+1980,exp(fv$fit))
lines(new.t+1980,exp(fv$fit+2*fv$se.fit),lty=2)
lines(new.t+1980,exp(fv$fit-2*fv$se.fit),lty=2)

