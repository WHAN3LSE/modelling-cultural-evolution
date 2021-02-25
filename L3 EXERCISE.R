#Load Library
library(lme4)

#mixed effects linear regression
plot(ysimp ~ xsimp,pch=rep(16:19,each=40),col=group, data=simp)
summary(lm(ysimp ~ xsimp, data=simp))
abline(lm(ysimp ~ xsimp, data=simp), col = "blue")

#separate regression lines per group 
linear.models <- lmList(ysimp ~ xsimp|group,data=simp)
summary(linear.models)

#random intercepts only
group.int <- lmer(ysimp ~ xsimp+(1|group), data=simp)
group.null <- lmer(ysimp ~ (1|group), data=simp)
anova(group.int, group.null)

summary(group.int)
coef(group.int)