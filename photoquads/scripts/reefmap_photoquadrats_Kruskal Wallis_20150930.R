#non-paramteric test for non normalised data

#http://www.statmethods.net/stats/nonparametric.html
install.packages("car")
library(car)
leveneTest(df.pq.norm$dead.coral~depth)
leveneTest(df.pq.norm$dead.coral~aspect)
leveneTest(df.pq.norm$dead.coral~region)
# Kruskal Wallis Test One Way Anova by Ranks 
#using SQRT transformed data from normality plots
kruskal.test(df.pq.norm$dead.coral~depth) # where y1 is numeric and A is a factor
kruskal.test(df.pq.norm$dead.coral~aspect)
kruskal.test(df.pq.norm$dead.coral~region)


#MACRO-ALGAE

leveneTest(df.pq.norm$macro.algal.assemblage~depth)
leveneTest(df.pq.norm$macro.algal.assemblage~aspect)
leveneTest(df.pq.norm$macro.algal.assemblage~region)


kruskal.test(df.pq.norm$macro.algal.assemblage~depth) # where y1 is numeric and A is a factor
kruskal.test(df.pq.norm$macro.algal.assemblage~aspect)
kruskal.test(df.pq.norm$macro.algal.assemblage~region)


install.packages("PMCMR")
library(PMCMR)
posthoc.kruskal.nemenyi.test(x=df.pq.norm$macro.algal.assemblage,g=aspect)
posthoc.kruskal.nemenyi.test(x=df.pq.norm$macro.algal.assemblage,g=region)
