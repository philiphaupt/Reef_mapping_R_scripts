#Site variabels

CPCe_photoquadrat_site_20160306 <- read.delim2("E:/stats/reefmap.pq.2013/photoquads/data/CPCe_photoquadrat_site_20160306.txt", row.names=1)
site<-as.data.frame(t(CPCe_photoquadrat_site_20160306 ))
summary(site)
attach(site)
str(site)
plot(Depth,Exposure)
plot(Aspect,Exposure)
table(Depth,Exposure)


tabletest <- function(x, y)
{
  tmp1 <- table(x, y)
  tmp2 <- loglin(tmp1, list(1, 2), fit = T)
  list(dep = (tmp1 - tmp2$fit)/tmp2$fit, p = (1 - pchisq(tmp2$pearson,
                                                         tmp2$df)))
}

tabletest(Depth,Exposure)

pres_sum<-rowSums (pq.fine.pres.t, na.rm = FALSE, dims = 1)

plot(Exposure,pres_sum)
plot(Depth,pq.fine.t$`Acropora short branching stag (ACB)`)

