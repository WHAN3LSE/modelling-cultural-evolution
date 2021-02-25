#Load Library
library(lme4)

plot(ysimp ~ xsimp, data=simp, col=group, pch=16)
summary(lm(ysimp~xsimp, data=simp))
lmList(ysimp~xsimp|group, data=simp)
simp.model1 <- lmer(ysimp~xsimp + (1|group), 
                    data=simp)
summary(simp.model1)
4.911/(4.911+1.421)
simp.null <- lmer(ysimp ~ (1|group), data=simp)
anova(simp.model1, simp.null, test= "chisq")

simp.model2 <- lmer(ysimp~xsimp + (1 + xsimp|group), data=simp)
summary(simp.model2)

#Considering Random Effects in the Model 
mode2.null <- lmer(ysimp~ (1 + xsimp|group), 
                   data=simp)
anova(simp.model2, mode2.null, test= "chisq")

#Significance disappears - means if you look as a group of the fixed
#effect there is a Trend but accounting for individual effects the significance
#is not there
