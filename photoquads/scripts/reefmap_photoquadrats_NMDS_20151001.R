#NDMS plot of photoquadrat analysis groups df.reef.pq.simplified.3 by depth, aspect and region (so run after script reefmap.photoquadrats_20150727)
setwd("E:\\stats\\reefmap.pq.2013\\photoquads")
#Load libraries for the required calculations
#library(rgl) - unused
#install.packages("vegan")
library(vegan) # library used for distance
#install.packages("rgl")
library(rgl)
#install.packages("labdsv")
library(labdsv) # library used for nmds, hcluster

#install.packages("sparcl")
library(sparcl)
library(ggplot2)

#This is working daftaframe derived df.reef.pq.simplified.3 by depth, aspect and region (so run after script reefmap.photoquadrats_20150727)
View(df.reef.pq.simplified3)

#1 Prepare numeric datafarme from this
df.reef.pq.simplified3.num <-subset(df.reef.pq.simplified3, select=-c(ID, depth,exposure,aspect,region,frames,points.total,points.total.min.tws,tws))
View(df.reef.pq.simplified3.num) #check number of columns should be 8


#2 Calculate solution-matrix distance using Manhattan statistics, Bray-Curtis method.
veg.dist.pq<-vegdist(df.reef.pq.simplified3.num,distance='bray')
veg.dist.pq

#3 Derive nmds from solution-matrix distance
nmds.veg.dist.pq<-metaMDS(veg.dist.pq,2)
nmds.veg.dist.pq


#to add transect names,depth,aspect,exposure,region:

transect<-rownames(df.reef.pq)
nmds.veg.dist.pq$transect.name <- transect
nmds.veg.dist.pq$depth.cat<-df.reef.pq$depth
nmds.veg.dist.pq$aspect.cat<-df.reef.pq$aspect
nmds.veg.dist.pq$region.cat<-df.reef.pq$region

#plots for each
#depth
plot(nmds.veg.dist.pq$points, type='n', xlab='', ylab='', main='')
text(nmds.veg.dist.pq$points, labels=nmds.veg.dist.pq$depth.cat)
#aspect
plot(nmds.veg.dist.pq$points, type='n', xlab='', ylab='', main='')
text(nmds.veg.dist.pq$points, labels=nmds.veg.dist.pq$aspect.cat)

#region
plot(nmds.veg.dist.pq$points, type='n', xlab='', ylab='', main='')
text(nmds.veg.dist.pq$points, labels=nmds.veg.dist.pq$region.cat)


#-----------------------------------------------------------------------
#same in vegan package and ggplot
#nmds from vegan
sol<- metaMDS(veg.dist.pq)
#grouping variable should be aspect, depth, etc...
MyMeta = data.frame(
  nmds.veg.dist.pq$transect.name <- transect,
  nmds.veg.dist.pq$depth.cat<-df.reef.pq$depth,
  nmds.veg.dist.pq$aspect.cat<-df.reef.pq$aspect,
  nmds.veg.dist.pq$region.cat<-df.reef.pq$region,
  grp.depth <- nmds.veg.dist.pq$depth.cat,
  grp.aspect <- nmds.veg.dist.pq$aspect.cat,
  grp.region <- nmds.veg.dist.pq$region.cat,
  row.names = nmds.veg.dist.pq$transect.name)
# plot NMDS using basic plot function and color points by "grp" from MyMeta
plot(sol$points)

#depth - 2 colours
NMDS <- data.frame(MDS1 = sol$points[,1], MDS2 = sol$points[,2],group=MyMeta$grp.depth)  
ggplot(data = NMDS, aes(MDS1, MDS2, shape=group, colour = group)) + geom_point(size = 3) + scale_shape_manual(values=c(15,16))+scale_colour_manual(values = c("cornflowerblue","mediumseagreen"))

#aspect - four colours
NMDS <- data.frame(MDS1 = sol$points[,1], MDS2 = sol$points[,2],group=MyMeta$grp.aspect)
p<-ggplot(data = NMDS, aes(MDS1, MDS2, shape=group, colour = group)) + geom_point(size = 3) + scale_shape_manual(values=c(15,16,17,19))+scale_colour_manual(values = c("cornflowerblue","mediumseagreen","darkgoldenrod1","firebrick1"))
p + theme(legend.position = "bottom",legend.background = element_rect(colour = "black"),legend.key = element_rect(fill = "white"),legend.text = element_text(size = 18, colour = "black", angle = 0),legend.title = element_text(size = 20))
#region - 8 colours
NMDS <- data.frame(MDS1 = sol$points[,1], MDS2 = sol$points[,2],group=MyMeta$grp.region)
ggplot(data = NMDS, aes(MDS1, MDS2, shape=group, colour = group)) + geom_point(size = 3) + scale_shape_manual(values=c(15,16,17,19,20,14,22,23,24))+scale_colour_manual(values = c("cornflowerblue","mediumseagreen","darkgoldenrod1","firebrick1","darkblue","grey38","black","lightpink"))


#PCo---------------------------------------------
#  PCo not working the right way round yet... considering the effect of the the categories, depth, aspect, region on these-----------------------------------------------------------------------
library(reshape2)
library(ggplot2)



#use df.reef.pq.simplified3.num

#depth PCo analysis
#add column 
df.reef.pq.simplified3.trans<-df.reef.pq.simplified3.num
df.reef.pq.simplified3.trans$transect<-transect
df.reef.pq.simplified3.trans

#samp.with.rownames <- data.frame(samp[,-1], row.names=samp[,1])

df.reef.pq.simplified3.trans2<-data.frame(df.reef.pq.simplified3.trans[-9], row.names=df.reef.pq.simplified3.trans[,9])
df.reef.pq.simplified3.trans2

pca <- prcomp(df.reef.pq.simplified3.trans2, scale=T)
summary(pca)
biplot(pca, cex = 0.75)


#ggplot not working yet---
melted <- cbind(nmds.veg.dist.pq$depth.cat, melt(pca$rotation[,1:7]))

barplot <- ggplot(data=melted) +
  geom_bar(aes(x=Var1, y=value, fill=nmds.veg.dist.pq$depth.cat), stat="identity") +
  facet_wrap(~Var2)

barplot





#-----------------------------------------------------------------------
veg.dist.pq.trans<-vegdist(df.reef.pq.simplified3.trans2,distance='bray')
df.reef.pq.simplified3.trans2
#6 derive hierarchical cluster
hcluster.pq<-hclust(veg.dist.pq, method='complete')
hcluster.pq.trans<-hclust(veg.dist.pq.trans, method = 'complete')
#7 save picture
#png(file='1as_sol100_dendogram.png',width=1260,height=780,pointsize=10)
#8 save picture hcluster
plot(hcluster.pq, xlab='Solutions', ylab='Disimilarity', main='Bray-Curtis dissimilarity', cex = 0.5)
plot(hcluster.pq.trans, xlab='Solutions', ylab='Disimilarity', main='Bray-Curtis dissimilarity', cex = 0.8)
#Cut tree into three groups
usercut<-cutree(hcluster.pq.trans,k=3)
usercut
rect.hclust(hcluster.pq.trans, 3)
rect.hclust(hcluster.pq.trans, 10, border="blue")

#------------


#reef.t<-trans.arcsine(t(reef/100)) #replace data: "reef" with "reef.t<-trans.arcsine(t(reef/100))" to run data as transformed
trans.arcsine <- function(x){
  asin(sign(x) * sqrt(abs(x)))
}
reef.arcsin<-trans.arcsine(df.reef.pq.simplified3.trans2/100)

#**----------BETA Flexible agglomerative Cluster analysis code R---------------------**#
#Theory: http://ecology.msu.montana.edu/labdsv/R/labs/lab13/lab13.html
#Flexible beta is recommended by Mumby et al in Remote sensing handbook: http://www.unesco.org/csi/pub/source/rs11.htm
library("cluster", lib.loc="C:/Program Files/R/R-3.2.1/library")

#Several parameter options exist for flexible Beta analysis - see the help file: in short the lower the vect: See http://ecology.msu.montana.edu/labdsv/R/labs/lab13/lab13.html for a useful description, and used in the example as advised by labdsv: Special case which yields a good intermediate result 

agnes.flexible<-agnes(reef.arcsin, diss = inherits(reef.arcsin, "dist"), metric = "euclidean",method = "flexible",par.method = c(0.625,0.625,-0.25)) # stores the values in agnes_flexible
summary(agnes.flexible)
plot(agnes.flexible, ask = FALSE, sub = paste("Agglomerative Coefficient = ",round(agnes.flexible$ac, digits = 2)),adj = 0, nmax.lab = 20, max.strlen = 5, xax.pretty = TRUE) 


#----NOT YET CODED change reef mapping code for photoquads----------------------------------SIMPER ANALYSIS----------------------------------------------------------#
#Determine the importance of the benthic cover types within each cluster
simper(comm, group, permutations = 0, trace = FALSE, 
       parallel = getOption("mc.cores"), ...)

# setup as per help file e.g.
(simp<-with(reef.env, simper(reef.t.m, clust)))
summary(simp)

#another way of doing this...
#LABDSV IMPORTANCE TABLES: yields the value contribution of the variables to the overall cluster dissimilarity
importance.table<-importance(reef.t.df, clusters.hc)
importance.table



#Relate the sites back to clusters
#comm.table<-with(reef.env, vegemite(round(reef.t.m,0), clust))
#vegemite(reef.t.df, agnes.flexible.hclust, scale = "Hill", zero=".")

