#cluster analysis explore

#untrasnformed presence data
cl.pq.f <- hclust(dis.bray,"complete")
plot(cl.pq.f)

democut<-cutree(cl.pq.f,k=5) #cuts at 5 clusters
democut2<-cutree(cl.pq.f,h=0.65) # cuts at h - 0.65


plot(cl.pq.f, labels = as.character(democut)) # way of showing grouping on the node
plot(cl.pq.f, labels = as.character(democut2))

table(Depth,democut2) #tabulate the number of transects per cluster per depth
table(Exposure,democut2)

boxplot(Exposure~democut2) #does not work: maybe needs to be continuous?, supposed to plot number of transects in boxplot per cluster per Exposure


cluster.bc <- cutree(cl.pq.f,h=0.99)
const(pq.fine.pres.t,cluster.bc,min=0.3) #nice way of seeing min contribution per TYPE to cluster

#agnes beta flexible
library(cluster)
pq.fine.flex <- agnes(dis.bray,method='flexible',par.method=c(0.625,0.625,-0.25))
plot(pq.fine.flex)

#transform the data arc-sine- may need to do??, als may need to use percent coverage ins tead of abundance

trans.arcsine <- function(x){
  asin(sign(x) * sqrt(abs(x)))
}
pqf.arcsin<-trans.arcsine(pq.fine.pres.t)#What to do with th NAN values???


