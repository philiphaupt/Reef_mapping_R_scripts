#MANOVA
#this test will almost certainly not work here, as data are no-parametric, but just to explore:
#load sqrt data set from normality test


Y<- cbind(df.pq.norm$hard.coral,df.pq.norm$soft.coral)
fit <- manova(Y~ df.reef.pq.simplified3$depth*df.reef.pq.simplified3$aspect*df.reef.pq.simplified3$region)
summary(fit, test="Pillai")
