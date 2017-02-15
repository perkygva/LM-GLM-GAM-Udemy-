######################  EXERCISE 3 DAY 1 #######################

## Is small data set, can read it in:
conc <- c(0.1,0.5,1,10,20,30,50,70,80,100,150)
no <- c(7,1,10,9,2,9,13,1,1,4,3)
yes <- c(0,0,3,4,0,6,7,0,0,1,7)

## Need to create a variable (n) for the total trials:
n <- no + yes

## Plot the probability of inhibition by the
## log of the probability of success.
## Note: need to add 0.5 to values of no and yes
## since you have 0 values of yes and the log
## of 0 is undefined

plot(log(conc),log((yes+0.5)/(no+0.5)))

## The plot seems consistent with the use of log(conc) 
## as the explanatory variable.

## First create the probability of success variable:
p <- yes/n

## Need to weight the regression by the total successes
## and failures for each level of concentration

inhibit.glm <- glm(p ~ I(log(conc)), family = binomial, weights = n)
summary(inhibit.glm)

