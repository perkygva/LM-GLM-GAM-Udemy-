data(warpbreaks)
head(warpbreaks)
wm <- lm(breaks~wool*tension,data=warpbreaks)
par(mfrow=c(2,2))
plot(wm)  # residual plots are fine
anova(wm)

## ... so there is evidence for a wool:tension interaction

with(warpbreaks,interaction.plot(tension,wool,breaks))
