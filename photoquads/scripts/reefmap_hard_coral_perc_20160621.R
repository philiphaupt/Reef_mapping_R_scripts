#summary plots of benthic substrate by depth
bnth.pq.basic <- read.delim("E:/stats/reefmap.pq.2013/photoquads/data/bnth.pq.basic.txt")

#run function summarySE


#CORAL#
tapply(bnth.pq.basic$coral,bnth.pq.basic$depth,summary)
SE <- function(x,na.rm = TRUE, ...) {
  + sd(x, na.rm = TRUE)/sqrt(NROW(na.omit(x)))
  }
#overall summary
summarySE(bnth.pq.basic$coral)
summarise(group_by(bnth.pq.basic, transect.name), mean(coral))
SE(bnth.pq.basic$coral)

tapply(bnth.pq.basic$coral,bnth.pq.basic$depth,summary)
tapply(bnth.pq.basic$coral,bnth.pq.basic$depth,SE)
#install.packages('Rmisc')
library("Rmisc", lib.loc="~/R/win-library/3.3")
#all CI
#CI(x = bnth.pq.basic$coral.1,ci = 0.95)
#by depth
tapply(bnth.pq.basic$coral.1,bnth.pq.basic$depth,CI)

#SOFT CORAL
# by depth
tapply(bnth.pq.basic$soft.coral,bnth.pq.basic$depth,summary)
summarise(group_by(bnth.pq.basic, transect.name), mean(soft.coral+fire.coral))
SE(bnth.pq.basic$soft.coral+bnth.pq.basic$fire.coral)
tapply(bnth.pq.basic$soft.coral+bnth.pq.basic$fire.coral,bnth.pq.basic$depth,summary)
tapply(bnth.pq.basic$soft.coral+bnth.pq.basic$fire.coral,bnth.pq.basic$depth,SE)
tapply(bnth.pq.basic$soft.coral+bnth.pq.basic$fire.coral,bnth.pq.basic$depth,CI)

#SEAGRASS
#overall summary
summary(bnth.pq.basic$seagrass)
SE(bnth.pq.basic$seagrass)
# by depth
tapply(bnth.pq.basic$seagrass,bnth.pq.basic$depth,summary)
summarise(group_by(bnth.pq.basic, transect.name), mean(seagrass))
tapply(bnth.pq.basic$seagrass,bnth.pq.basic$depth,summary)
tapply(bnth.pq.basic$seagrass,bnth.pq.basic$depth,SE)
tapply(bnth.pq.basic$seagrass,bnth.pq.basic$depth,CI)

#rubble
#overall summary
summary(bnth.pq.basic$rubble)
SE(bnth.pq.basic$rubble)
# by depth
tapply(bnth.pq.basic$rubble,bnth.pq.basic$depth,summary)
summarise(group_by(bnth.pq.basic, transect.name), mean(rubble))
tapply(bnth.pq.basic$rubble,bnth.pq.basic$depth,summary)
tapply(bnth.pq.basic$rubble,bnth.pq.basic$depth,SE)
tapply(bnth.pq.basic$rubble,bnth.pq.basic$depth,CI)
