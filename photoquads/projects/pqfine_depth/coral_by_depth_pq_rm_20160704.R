setwd("E:/stats/reefmap.pq.2013/photoquads")
pqfine <- read.delim("E:/stats/aldabra/reefmap/photoquads/data/CPCe_photoquadrat_fine_presence_20160306_t.txt", row.names=1)

library(ggplot2)
library(plyr)
library(dplyr)
library(reshape2)

#subset only coral
coral.df<-dplyr::select(pqfine,depth,Acanthastrea.ACANT:Unkown.coral.UNK)

#means of coral overall transects by depth
coral.df.mean <- coral.df %>% group_by(depth) %>% summarise_each(funs(mean))
#reshape the data to tall format for plotting
coral.means <- melt(coral.df.mean, id.vars="depth")
#rename columns
coral.means<-dplyr::rename(coral.means,coral = variable,means = value)

#standard error
library("plotrix", lib.loc="~/R/win-library/3.3")
#of coral overall transects by depth
coral.df.sterr <- coral.df %>% group_by(depth) %>% summarise_each(funs(std.error))
#reshape the data to tall format for plotting
coral.sterr <- melt(coral.df.sterr,id.vars="depth")
#rename columns
coral.sterr<-dplyr::rename(coral.sterr,depth2 = depth, coral.name = variable,se = value)


#merge means and standard error into singel dataframe
coral.merge<-cbind(coral.means,coral.sterr)

#remove duplcaite columns depth 2
coral.plot<-dplyr::select(coral.merge,depth,coral,means,se)

cp<-ggplot2::ggplot(coral.plot, aes(x=reorder(coral,coral.plot$means), y=coral.plot$means, fill=depth)) +
  geom_bar(position ="dodge", stat="identity") +
  geom_errorbar(aes(ymin=coral.plot$means - coral.plot$se, ymax=coral.plot$means + coral.plot$se), width=.2, position=position_dodge(.9))+
  xlab("coral") +
  ylab("Mean number of presence records per transect")+
  theme(axis.text.x = element_text(size=8, angle = 90, hjust = 1),
        axis.text.y = element_text(size=8, hjust = 1),
        axis.title.x = element_text(size=14,angle=0,hjust=0.5,vjust =0.5,face="plain"),
        axis.title.y = element_text(size=14,angle=90,hjust=0.5,vjust = 0.5,face="plain"),
        legend.text=element_text(size=12),
        legend.title=element_text(size=14))

cp+coord_flip()


#same with subset contributing more than 8 points per transects

common.coral<-filter(coral.plot, means>8)

cp2<-ggplot2::ggplot(common.coral, aes(x=reorder(coral,common.coral$means), y=common.coral$means, fill=depth)) +
  geom_bar(position ="dodge", stat="identity") +
  geom_errorbar(aes(ymin=common.coral$means - common.coral$se, ymax=common.coral$means + common.coral$se), width=.2, position=position_dodge(.9))+
  xlab("coral") +
  ylab("Mean number of presence records per transect")+
  theme(axis.text.x = element_text(size=8, angle = 90, hjust = 1),
        axis.text.y = element_text(size=8, hjust = 1),
        axis.title.x = element_text(size=14,angle=0,hjust=0.5,vjust =0.5,face="plain"),
        axis.title.y = element_text(size=14,angle=90,hjust=0.5,vjust = 0.5,face="plain"),
        legend.text=element_text(size=12),
        legend.title=element_text(size=14))
cp2+coord_flip()
