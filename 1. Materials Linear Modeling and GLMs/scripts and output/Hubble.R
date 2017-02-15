#### So how old is the universe?
install.packages("gamair")
library(gamair) # contains 'hubble'
data(hubble)
hub.mod <- lm(y~x-1,data=hubble)
summary(hub.mod)

#### Plot the residuals against the fitted values
plot(fitted(hub.mod),residuals(hub.mod),xlab="fitted values",ylab="residuals")

#### Omit offending points and produce new residual plot
hub.mod1 <- lm(y~x-1,data=hubble[-c(3,15),])
summary(hub.mod1)
plot(fitted(hub.mod1),residuals(hub.mod1),xlab="fitted values",ylab="residuals")

#### Estimate Hubble's Constant
hubble.const <- c(coef(hub.mod),coef(hub.mod1))/3.09e19
age <- 1/hubble.const
age/(60^2*24*365)

#### 1.1.3 Adding a distributional assumption
#### Testing Hypotheses about \beta
cs.hubble <- 163000000
t.stat<-(coef(hub.mod1)-cs.hubble)/summary(hub.mod1)$coefficients[2]
pt(t.stat,df=21)*2

#### Confidence intervals
sigb <- summary(hub.mod1)$coefficients[2]
h.ci<-coef(hub.mod1)+qt(c(0.025,0.975),df=21)*sigb
h.ci
h.ci<-h.ci*60^2*24*365.25/3.09e19 # convert to 1/years
sort(1/h.ci)