#ANOVA

#can be sued where data normally distributed, and homoscedastic. 
#used here for SQRT(hard.coral) which was both.

#info from:
#http://www.statmethods.net/stats/anova.html

# One Way Anova (Completely Randomized Design)

#DEPTH hard.coral
fit.depth.hard.coral <- aov(sqrt(hard.coral) ~ depth, data=df.reef.pq.simplified3)
fit.depth.hard.coral
#Diagnostic plots
plot(fit.depth.hard.coral) # diagnostic plots

summary(fit.depth.hard.coral) # display Type I ANOVA table
drop1(fit.depth.hard.coral,~.,test="F") # type III SS and F Tests

# Tukey Honestly Significant Differences
TukeyHSD(fit.depth.hard.coral) # where fit comes from aov()

#DEPTH soft.coral
fit.depth.soft.coral <- aov(sqrt(soft.coral) ~ depth, data=df.reef.pq.simplified3)
fit.depth.soft.coral
#Diagnostic plots
plot(fit.depth.soft.coral) # diagnostic plots

summary(fit.depth.soft.coral) # display Type I ANOVA table
drop1(fit.depth.soft.coral,~.,test="F") # type III SS and F Tests

# Tukey Honestly Significant Differences
TukeyHSD(fit.depth.soft.coral) # where fit comes from aov()

#ASPECT hard.coral
fit.aspect.hard.coral <- aov(sqrt(hard.coral) ~ aspect, data=df.reef.pq.simplified3)
fit.aspect.hard.coral
#Diagnostic plots
plot(fit.aspect.hard.coral) # diagnostic plots
summary(fit.aspect.hard.coral) # display Type I ANOVA table
drop1(fit.aspect.hard.coral,~.,test="F") # type III SS and F Tests
# Tukey Honestly Significant Differences
TukeyHSD(fit.aspect.hard.coral) # where fit comes from aov()

#ASPECT soft.coral
fit.aspect.soft.coral <- aov(sqrt(soft.coral) ~ aspect, data=df.reef.pq.simplified3)
fit.aspect.soft.coral
#Diagnostic plots
plot(fit.aspect.soft.coral) # diagnostic plots
summary(fit.aspect.soft.coral) # display Type I ANOVA table
drop1(fit.aspect.soft.coral,~.,test="F") # type III SS and F Tests
# Tukey Honestly Significant Differences
TukeyHSD(fit.aspect.soft.coral) # where fit comes from aov()

#REGION hard.coral
fit.region.hard.coral <- aov(sqrt(hard.coral) ~ region, data=df.reef.pq.simplified3)
fit.region.hard.coral
#Diagnostic plots
plot(fit.region.hard.coral) # diagnostic plots
summary(fit.region.hard.coral) # display Type I ANOVA table
drop1(fit.region.hard.coral,~.,test="F") # type III SS and F Tests
# Tukey Honestly Significant Differences
TukeyHSD(fit.region.hard.coral) # where fit comes from aov()

#REGION soft.coral
fit.region.soft.coral <- aov(sqrt(soft.coral) ~ region, data=df.reef.pq.simplified3)
fit.region.soft.coral
#Diagnostic plots
plot(fit.region.soft.coral) # diagnostic plots
summary(fit.region.soft.coral) # display Type I ANOVA table
drop1(fit.region.soft.coral,~.,test="F") # type III SS and F Tests
# Tukey Honestly Significant Differences
TukeyHSD(fit.region.soft.coral) # where fit comes from aov()
