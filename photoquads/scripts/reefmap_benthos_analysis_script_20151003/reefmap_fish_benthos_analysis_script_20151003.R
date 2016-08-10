#fish and benthos
setwd("E:/stats/reefmap.pq.2013/fish")
fishdata <- read.delim("E:/stats/reefmap.pq.2013/fish/data/qryFishCountsbyTransect.txt")
df.fish.rm<-fishdata

dim(df.fish.rm)
View(df.fish.rm)
attach(df.fish.rm)
#names(df.fish.rm)


sapply(df.fish.rm, class)
tapply(as.integer(df.fish.rm$AvgOfquantity),df.fish.rm$guild, sum)

#benthic data
t.photoquadrats_20150721 <- read.delim("E:/stats/reefmap.pq.2013/photoquads/data/t.photoquadrats_20150721.txt", row.names=1)
df.reef.pq <- as.data.frame(t.photoquadrats_20150721)
is.data.frame(df.reef.pq)
sapply(df.reef.pq,class)


#RENAME column names as follows:
colnames(df.reef.pq)[11] <- "limestone.bedrock"
#rename coral to hard coral
colnames(df.reef.pq)[13] <- "hard.coral"

attach(df.reef.pq)
library(reshape2)
library(ggplot2)

df.reef.pq$ID<-seq.int(nrow(df.reef.pq))

#Groupings sumaries and boxplots to follow: Below several diferent groupings, and omission are applied to the benthic cover types to produce meaningful graphs.

#GROUPING 1: Fire coral grouped with Other, including other hydrozoan, ascidian, zooanthids, corallimorpharians NEW GROUNAME: "others.zoo"-----
df.others.zoo<-as.data.frame(apply(df.reef.pq[,c('fire.coral','other','sponge')],1,sum))
is.data.frame(df.others.zoo)
print(df.others.zoo)
sapply(df.others.zoo,class)
names(df.others.zoo)
#rename column
colnames(df.others.zoo) <- "others.zoo"
#Assign Index to df
df.others.zoo$ID<-seq.int(nrow(df.others.zoo))
dim(df.others.zoo)
#merge others.zoo into simplified dataframe
df.reef.pq.simplified<-merge(df.reef.pq,df.others.zoo,by.x="ID")
#remove old columns
df.reef.pq.simplified$other<-NULL
df.reef.pq.simplified$fire.coral<-NULL
df.reef.pq.simplified$sponge<-NULL
#check dims
dim(df.reef.pq.simplified)
View(df.reef.pq.simplified)
rm(df.others.zoo)


# Melt reshaping further simpification to compare with report results
#report categories: (Sand, rock, rubble),Algae,Hard coral,Soft coral, Dead coral



#GROUPING 2:
#dead.standing.hard.coral grouped with dead.standing.hard.coral.ta
df.dead.coral<-as.data.frame(apply(df.reef.pq.simplified[,c('dead.standing.hard.coral','dead.standing.hard.coral.ta')],1,sum))
#View(df.dead.coral)
#rename column
dim(df.dead.coral)
colnames(df.dead.coral) <- "dead.coral"
#Assign Index to df
df.dead.coral$ID<-seq.int(nrow(df.dead.coral))
#merge into simplified dataframe
df.reef.pq.simplified2<-merge(df.reef.pq.simplified,df.dead.coral,"ID")
#remove old columns
df.reef.pq.simplified2$dead.standing.hard.coral<-NULL
df.reef.pq.simplified2$dead.standing.hard.coral.ta<-NULL
dim(df.reef.pq.simplified2)
View(df.reef.pq.simplified2)
rm(df.dead.coral)

detach(df.reef.pq)
attach(df.reef.pq.simplified3)

#GROUPING 3: limestone rock, rock, cca, and rubble

df.srr<-as.data.frame(apply(df.reef.pq[,c('rubble','limestone.bedrock','silt','rock','crustose.coraline.algae')],1,sum))
colnames(df.srr) <- "limestone.rock.rubble"
#assign index
df.srr$ID<-seq.int(nrow(df.srr))
#merge into simplified dataframe
df.reef.pq.simplified3<-merge(df.reef.pq.simplified2,df.srr,"ID")
dim(df.reef.pq.simplified3)

#remove old columns
df.reef.pq.simplified3$rock<-NULL
df.reef.pq.simplified3$crustose.coraline.algae<-NULL
df.reef.pq.simplified3$silt<-NULL
df.reef.pq.simplified3$rubble<-NULL
df.reef.pq.simplified3$limestone.bedrock<-NULL
names(df.reef.pq.simplified3)
summary(df.reef.pq.simplified3)

#MERGE DATA INTO ONE DATAFARME
df.reef.pq.simplified3$ts<-row.names(t.photoquadrats_20150721)
df.fish.pq<-merge(df.reef.pq.simplified3,df.fish.rm,by = "ts")
df.fish.pq#-----------------------------
#continue analysis of two data frames df.reef.pq.simplified3 and df.fish.rm
#to clear unneccesary df values 
rm(t.photoquadrats_20150721,df.reef.pq.simplified2,df.reef.pq.simplified,df.reef.pq,df.srr,Fishdata,fishdata,df.region.guild,tab,labels,tab.,df.fish.rm,df.reef.pq.simplified3, df.reef.pq.simplified3.num,df.fish.pq.or,breaks)
#leaving only 1 dfs.

#-----------------------------

#df.fish.pq$hard.coral.cat<- as.character(df.fish.pq$shallow_or_deep)
df.fish.pq$hard.coral.cat[df.fish.pq$hard.coral < 20] <- "medium"
df.fish.pq$hard.coral.cat[df.fish.pq$hard.coral > 10] <- "low"
df.fish.pq$hard.coral.cat[df.fish.pq$hard.coral >20] <- "high"

#Fish guild in relation to coral - perhaps too much detail here for report but see graph
#df.fish.pq.or<-data.frame(remove_outliers(df.fish.pq$AvgOfquantity))

with(df.fish.pq,tapply(as.integer(df.fish.pq$AvgOfquantity),list(df.fish.pq$hard.coral.cat,df.fish.pq$guild),mean))


par(las=2)
par(mar=c(14,4,2,1))
boxplot(as.integer(df.fish.pq$AvgOfquantity ~ interaction(df.fish.pq$hard.coral.cat,df.fish.pq$guild), data=df.fish.pq, xlab = "", ylab = "Avg fish count", main = "Avg fish count"))


#SAND

#df.fish.pq$hard.sand.cat<- as.character(df.fish.pq$shallow_or_deep)
df.fish.pq$sand.cat[df.fish.pq$sand < 20] <- "medium"
df.fish.pq$sand.cat[df.fish.pq$sand > 10] <- "low"
df.fish.pq$sand.cat[df.fish.pq$sand >20] <- "high"

with(df.fish.pq,tapply(as.integer(df.fish.pq$AvgOfquantity),list(df.fish.pq$sand.cat,df.fish.pq$guild),mean))


#DEAD CORAL
#df.fish.pq$dead.coral.cat<- as.character(df.fish.pq$shallow_or_deep)
df.fish.pq$dead.coral.cat[df.fish.pq$dead.coral < 5] <- "medium"
df.fish.pq$dead.coral.cat[df.fish.pq$dead.coral > 1] <- "low"
df.fish.pq$dead.coral.cat[df.fish.pq$dead.coral >10] <- "high"

#Fish guild in relation to coral - perhaps too much detail here for report but see graph
#df.fish.pq.or<-data.frame(remove_outliers(df.fish.pq$AvgOfquantity))

with(df.fish.pq,tapply(as.integer(df.fish.pq$AvgOfquantity),list(df.fish.pq$dead.coral.cat,df.fish.pq$guild),mean))

#SEAGRASS
#df.fish.pq$seagrass.cat<- as.character(df.fish.pq$shallow_or_deep)
df.fish.pq$seagrass.cat[df.fish.pq$seagrass < 5] <- "medium"
df.fish.pq$seagrass.cat[df.fish.pq$seagrass > 1] <- "low"
df.fish.pq$seagrass.cat[df.fish.pq$seagrass >5] <- "high"

#Fish guild in relation to coral - perhaps too much detail here for report but see graph
#df.fish.pq.or<-data.frame(remove_outliers(df.fish.pq$AvgOfquantity))

with(df.fish.pq,tapply(as.integer(df.fish.pq$AvgOfquantity),list(df.fish.pq$seagrass.cat,df.fish.pq$guild),mean))

#macro.algal.assemblage
#df.fish.pq$macro.algal.assemblage.cat<- as.character(df.fish.pq$shallow_or_deep)
df.fish.pq$macro.algal.assemblage.cat[df.fish.pq$macro.algal.assemblage < 20] <- "medium"
df.fish.pq$macro.algal.assemblage.cat[df.fish.pq$macro.algal.assemblage > 10] <- "low"
df.fish.pq$macro.algal.assemblage.cat[df.fish.pq$macro.algal.assemblage >20] <- "high"

#Fish guild in relation to coral - perhaps too much detail here for report but see graph
#df.fish.pq.or<-data.frame(remove_outliers(df.fish.pq$AvgOfquantity))

with(df.fish.pq,tapply(as.integer(df.fish.pq$AvgOfquantity),list(df.fish.pq$macro.algal.assemblage.cat,df.fish.pq$guild),mean))
#RUN NMDS script for pq to get veg dist.


