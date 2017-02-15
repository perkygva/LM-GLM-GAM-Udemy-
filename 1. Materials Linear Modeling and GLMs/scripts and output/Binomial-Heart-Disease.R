### Binomial models and heart disease

# Read in original Hand et al data from heart.csv
heart <- read.csv("c:/temp/heart.csv", header=T)
heart # to view the entire data set
head(heart) # to only view the first six rows

# Then plot the observed proportions of those who had a heart attack
# divided by the total number of patients:
p <- heart$ha /(heart$ha+heart$ok)
p

# Then plot the proportion as a function of increasing CK:
plot(heart$ck,p,xlab="Creatinine kinase level", ylab="Proportion Heart Attack")

# GLM call using cbind() to fit the heart attack model
mod.0 <- glm(cbind(ha,ok)~ck, family=binomial(link=logit), data=heart)
mod.0

#or we could have used since the logit link is the R default for binomial
mod.0 <- glm(cbind(ha,ok)~ck, family=binomial,data=heart)
mod.0

# Generate the model diagnostic plots
par(mfrow=c(2,2))
plot(mod.0)

# Plot the predicted heart attack levels (proportions)
plot(heart$ck,p,xlab="Creatinise kinase level", ylab="proportion Heart Attack")
lines(heart$ck,fitted(mod.0))

# Fit a second model with a cubic linear predictor
mod.2 <- glm(cbind(ha,ok)~ck+I(ck^2)+I(ck^3), family=binomial, data=heart)
mod.2

# Plot predicted heart attack levels (proportions) with the cubic model
par(mfrow=c(1,1))
plot(heart$ck,p,xlab="Creatinine kinase level", ylab="Proportion Heart Attack")
lines(heart$ck,fitted(mod.2))

# Calculate an 'analysis of deviance' table
anova(mod.0,mod.2,test="Chisq")

