#Summary stats from reef map
setwd("E:\\stats\\reefmap.pq.2013\\photoquads")

t.photoquadrats_20150721 <- read.delim("E:/stats/reefmap.pq.2013/photoquads/data/t.photoquadrats_20150721.txt", row.names=1)


#CLEAR ALL: 
#rm(list=ls())
#detach(df.reef.pq)

df.reef.pq <- as.data.frame(t.photoquadrats_20150721)
is.data.frame(df.reef.pq)
View(df.reef.pq)
dim(df.reef.pq)
typeof(df.reef.pq)
sapply(df.reef.pq,class)


#RENAME column names as follows:
colnames(df.reef.pq)[11] <- "limestone.bedrock"
#rename coral to hard coral
colnames(df.reef.pq)[13] <- "hard.coral"

attach(df.reef.pq)
names(df.reef.pq)

summary(df.reef.pq)


#install.packages("reshape2")
library(reshape2)
#install.packages("ggplot2")
library(ggplot2)


df.reef.pq$ID<-seq.int(nrow(df.reef.pq))#

#######################################################GROUPING#####################################################
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
#PLOTS 2: reshape the SIMPLIFIED dataframe for plotting
#reef.colour.pal<-c("plum","sandybrown","seagreen", "purple","springgreen" ,"gray52","salmon","black")
df.reef.pq.report <- melt(df.reef.pq.simplified3,measure.vars=c("limestone.rock.rubble",'hard.coral','dead.coral','soft.coral','others.zoo','macro.algal.assemblage','sand','seagrass'))
#Overall
p.report <- ggplot(df.reef.pq.report) + geom_boxplot(aes(x='Simplified benthic cover',y=value,color=variable))
p.report + xlab("") + labs(x = "Benthic cover on fore reef")+ ylab("% Cover") + ggtitle("") + theme(axis.title.x=element_text(size = 16), axis.title.y=element_text(size = 16),legend.title=element_text(size = 0), legend.text=element_text(size = 12))
#By depth
p.depth <- ggplot(df.reef.pq.report) + geom_boxplot(aes(x=depth, y=value, color=variable)) # SELECTED VARIABLES
p.depth + scale_x_discrete(breaks =c('d','s'),labels =c("15m", "5m")) + xlab("") + labs(x = "Fore reef benthic cover by depth")+ ylab("% Cover") + ggtitle("") + theme(axis.title.x=element_text(size = 16), axis.title.y=element_text(size = 16),legend.title=element_text(size = 0), legend.text=element_text(size = 12),axis.text.x = element_text(size = 14))

#By aspect
p.aspect <- ggplot(df.reef.pq.report) + geom_boxplot(aes(x=aspect, y=value, color=variable)) # SELECTED VARIABLES
p.aspect + xlab("") + labs(x = "Fore reef benthic cover by reef aspect")+ ylab("% Cover") + ggtitle("") + theme(axis.title.x=element_text(size = 16), axis.title.y=element_text(size = 16),legend.title=element_text(size = 0), legend.text=element_text(size = 12),axis.text.x = element_text(size = 14))
#By region
p.region <- ggplot(df.reef.pq.report) + geom_boxplot(aes(x=region, y=value, color=variable)) # SELECTED VARIABLES
p.region + xlab("") + labs(x = "Fore reef benthic cover by reef region")+ ylab("% Cover") + ggtitle("") + theme(axis.title.x=element_text(size = 16), axis.title.y=element_text(size = 16),legend.title=element_text(size = 0), legend.text=element_text(size = 12),axis.text.x = element_text(size = 14))

#----------------------------GROUPING END--------------------------#




#PLOTS 1: reshape the dataframe for plotting

#df.reef.pq.summary <- melt(df.reef.pq.simplified,measure.vars=c('sand','limestone.bedrock','hard.coral','rubble','seagrass','soft.coral','others.zoo','dead.standing.hard.coral','dead.standing.hard.coral.ta','crustose.coraline.algae','macro.algal.assemblage','seagrass'))
#p.summary <- ggplot(df.reef.pq.summary) + geom_boxplot(aes(x='Benthic cover',y=value,color=variable)) + labs(x = "Benthic cover type",y = "% Cover", title = "Summary of benthic habitat type cover from photoquadrat analysis")
#p.summary
#p.region <- ggplot(df.reef.pq.summary) + geom_boxplot(aes(x=region, y=value, color=variable)) # SELECTED VARIABLES
#p.region
#p.aspect <- ggplot(df.reef.pq.summary) + geom_boxplot(aes(x=aspect, y=value, color=variable)) # SELECTED VARIABLES
#p.aspect
#p.depth <- ggplot(df.reef.pq.summary) + geom_boxplot(aes(x=depth, y=value, color=variable)) # SELECTED VARIABLES
#p.depth


#earlier exploring reshape------------------------------------------
#1. reshaping of dataframe (neccessary for ) to produce boxplots in various forms
#melt reshape of most columns (sand, coral, but omitting rock etc)
#df.reef.pq.m <- melt(df.reef.pq,measure.vars=c('sand','dead.coral.rock','rubble','coral','soft.coral','dead.standing.hard.coral','dead.standing.hard.coral.ta','crustose.coraline.algae','macro.algal.assemblage','seagrass','sponge'))

#2. melt reshape 2 - fewer categories, same as above, excluding dead.coral.cork (bedrock)
#INCLUDING: 'rubble','coral','soft.coral','dead.standing.hard.coral','dead.standing.hard.coral.ta','crustose.coraline.algae','macro.algal.assemblage','sponge'
#df.reef.pq.m2 <- melt(df.reef.pq,measure.vars=c('rubble','coral','soft.coral','dead.standing.hard.coral','dead.standing.hard.coral.ta','crustose.coraline.algae','macro.algal.assemblage','sponge'))
#boxplots for region, aspect and depth to follow


#3. melt coral 1 - melt reshape to prod boxplot of coral categories
#df.reef.pq.m.coral.1 <- melt(df.reef.pq,measure.vars=c('coral','fire.coral','dead.standing.hard.coral.ta','dead.standing.hard.coral','soft.coral'))
#boxplot of benthic cover
#p.region.coral.1 <- ggplot(df.reef.pq.m.coral.1) + geom_boxplot(aes(x=region,y=value,color=variable))#SELECTED CORAL VARIABLES BY REGION
#p.region.coral.1
#--------------------------------------------------------------------

#ALL VAIRABLES p <- ggplot(df.reef.pq.m) + geom_boxplot(aes(x=region, y=value, color=variable))



#OLDER GRAPHS NOT USED BELOW
#BASIC summary by depth


#BASIC SUMMARY BY DEPTH - Exploratory - not used in report
boxplot(df.reef.pq$hard.coral ~ df.reef.pq$depth,data = df.reef.pq.simplified3,col = colours,main="Percentage cover", xlab = "Depth", ylab = "Coral") # CORAL


#BASIC SUMMARY BY REGION
boxplot(coral ~ region,data = df.reef.pq,main="Percentage cover", xlab = "Region", ylab = "Coral") # CORAL

#BASIC SUMMARY BY ASPECT

coral.by.aspect.median<-tapply(coral,aspect,median)
n.by.aspect<-tapply(coral,aspect,median)
tapply(coral,aspect,summary)
colours <- c(rep("blue",1),rep("green",1),rep("yellow",1),rep("red"))
boxplot(coral ~ aspect,data = df.reef.pq,col = colours,main="Percentage cover", xlab = "Aspect", ylab = "Coral") # CORAL
boxplot(dead.coral.rock ~ aspect,data = df.reef.pq,col = colours,xlab = "Aspect", ylab = "Percentage limestone rock cover")
boxplot(soft.coral ~ aspect,data = df.reef.pq,col = colours,xlab = "Aspect", ylab = "Percentage soft coral cover")
boxplot(sand ~ aspect,data = df.reef.pq,col = colours,xlab = "Aspect", ylab = "Percentage sand cover")
boxplot(macro.algal.assemblage ~ aspect,data = df.reef.pq,col = colours,xlab = "Aspect", ylab = "Percentage macro algae cover")
boxplot(sponge ~ aspect,data = df.reef.pq,col = colours,xlab = "Aspect", ylab = "Percentage sponge cover")
boxplot(seagrass ~ aspect,data = df.reef.pq,col = colours,xlab = "Aspect", ylab = "Percentage seagrass cover")
boxplot(crustose.coraline.algae ~ aspect,data = df.reef.pq,col = colours,xlab = "Aspect", ylab = "Percentage crustose coraline algae cover")
boxplot(dead.standing.hard.coral ~ aspect,data = df.reef.pq,col = colours,xlab = "Aspect", ylab = "Percentage standing hard coral cover")
boxplot(dead.standing.hard.coral.ta ~ aspect,data = df.reef.pq,col = colours,xlab = "Aspect", ylab = "Percentage standing hard coral with turf algae cover")

#SUMMARY BY ASPECT AND DEPTH
with(df.reef.pq,tapply(coral,list(depth,aspect),mean))
colours <- c(rep("blue",2),rep("green",2),rep("yellow",2),rep("red",2))
boxplot(coral ~ interaction(depth,aspect), data=df.reef.pq, col = colours,xlab = "Aspect", ylab = "Coral", main = "Percentage cover")

#SUMMARY BY REGION AND DEPTH
with(df.reef.pq,tapply(coral,list(depth,region),mean))
colours <- c(rep("blue",2),rep("green",2),rep("yellow",2),rep("red",2), rep("purple",2), rep ("lightgrey",2), rep("lightblue",2),rep("orange",2))
boxplot(coral ~ interaction(depth,region), data=df.reef.pq, col = colours,xlab = "Region", ylab = "Coral", main = "Percentage cover")

#Summary by ASPECT for most columns
tapply()
