#Habitat classifcation statsitics to descriminate between sites, find dissimilarity, and provide ecological habitat descriptions
# http://www.unesco.org/csi/pub/source/rs11.htm

#Genreal start: set working directory:
setwd("~/sif/gef/projects/mpa expansion/reef mapping/results/R/svrwandshallow")

#--------------------------------------------READ DATA-------------------------------------------------#
#Export results from Excel to text (tab) delimited file; Open R Studio; Import text file, and assign easy to work with name
reef <- read.delim("cpce_results_combined_201408.txt", quote="",row.names = 1)
reef.t<-t(reef) #DATA NEEDS TO BE TRANSPOSED, (nb!!)
dim(reef.t) #determine the number of rows and columns for the data matrix, replace value 16, 17 wiht this value.
str(reef.t)
View(reef.t)


#--------------------------DATA PREP FOR AGGLOMARATIVE CLUSTER ANALYSIS--------------------------------------------------------#

#Cluster analysis code R
#http://ecology.msu.montana.edu/labdsv/R/labs/lab13/lab13.html

#Load libraries needed: 
library("labdsv", lib.loc="C:/Users/gef_philip/Documents/R/win-library/2.15")
library("vegan", lib.loc="C:/Users/gef_philip/Documents/R/win-library/2.15")
library("cluster", lib.loc="C:/Program Files/R/R-2.15.3/library")
# install.packages('ape') install package first time
library(ape) # dendrogram functions

#Transform data to matrix for analysis
#reef.t.m<-data.frame(matrix(reef.t,nrow = 366, ncol = 16,,dimnames = dimnames(reef.t)))
reef.t.m<-matrix(as.numeric(reef.t),nrow = 366, ncol = 16,,dimnames = dimnames(reef.t))
str(reef.t.m)
View(reef.t.m)

#-----------------------------------CLUSTER ANAYLIS TO FOLLOW----------------------------------------------------#
#Flexible beta is recommended by Mumby et al in Remote sensing handbook
#Several parameter options exist for flexible Beta analysis - see the help file: in short the lower the vect: See http://ecology.msu.montana.edu/labdsv/R/labs/lab13/lab13.html for a useful description, and used in the example as advised by labdsv: Special case which yields a good intermediate result 

agnes.flexible<-agnes(reef.t.m, diss = inherits(reef.t.m, "dist"), metric = "euclidean",method = "flexible",par.method = c(0.625,0.625,-0.25)) # stores the values in agnes_flexible
agnes.flexible
plot(agnes.flexible, ask = FALSE, sub = paste("Agglomerative Coefficient = ",round(agnes.flexible$ac, digits = 2)),adj = 0, nmax.lab = 20, max.strlen = 5, xax.pretty = TRUE)

agnes.flexible.hclust<-as.hclust(agnes.flexible) # sets results of agnes to that of hclust class for more plotting functionality
plot(agnes.flexible.hclust, hang = -1)
#colour plot
mypal = c("#8B4513", "#FF7F50", "#D2691E", "#CD853F", "#F4A460", "#71ABDE", "#DEB887","#BC8F8F", "#B0C4DE", "#696969", "#483D8B","#008080" )#"#FF4500" ) # vector of 12 colours
op = par(bg = "#E8DDCB") # background colour
clustercuttree<-cutree(agnes.flexible.hclust,k=12) #cut the tree
plot(as.phylo(agnes.flexible.hclust), cex = 0.9, label.offset = 1, tip.color = mypal[clustercuttree], col = "red")
#plot(as.phylo(agnes.flexible.hclust), type = "fan", tip.color = hsv(runif(15, 0.65,0.95), 1, 1, 0.7), edge.color = hsv(runif(10, 0.65, 0.75), 1, 1, 0.7), edge.width = runif(20,0.5, 3), use.edge.length = TRUE, col = "gray80")
plot(as.phylo(agnes.flexible.hclust), type = "unrooted")

table(clustercuttree)
#const(bryceveg,agnes.flexible.hclust,min=0.2)
#plot(clustercuttree) #ask = FALSE, sub = paste("Agglomerative Coefficient = ",round(clustercuttree$ac, digits = 2)),adj = 0, nmax.lab = 20, max.strlen = 5, xax.pretty = TRUE)

#OLDER VERSION#agnes_flexible<-agnes(an_t, diss = inherits(an_t, "dist"), metric = "euclidean",method = "flexible",par.method = c(0.625,0.625,-0.25)) # stores the values in agnes_flexible
#OLDER VERSION was not dataframe so needed converting first - delete this line when happy with code# plot(agnes_flexible, ask = FALSE, sub = paste("Agglomerative Coefficient = ",round(agnes_flexible$ac, digits = 2)),adj = 0, nmax.lab = 20, max.strlen = 5, xax.pretty = TRUE)
######tabulate the site names to the clusters!!!table(row.names,agnes.flexible)
#-----------------------------------CLUSTER GROUPINGS TO FOLLOW---------------------------------------------------#

#Decide on cut offs for the tree: The tree needs to be grouped...it will proabbly take a few times to come up with a sensible grouping.
#cutree(tree, k = NULL, h = NULL)
clusters<-factor(cutree(agnes.flexible,12)) # choose the number of groups to cut tree with; a value can also be set, see help file on cutree


#----------------------------------SIMPER DATA PREP TO FOLLOW-----------------------------------------------------#

#Convert to data frame#
reef.t.df <- data.frame(reef.t)
str(reef.t.df)

#add clust as a factor column to an.t.df
site.cluster<-within(reef.t.df, {
        clust <-factor(clusters)
})

tapply(site.cluster$CORAL..C.,site.cluster$clust, mean)

reef.env <- subset(site.cluster, select = "clust")
str(reef.env)
View(reef.env)


#-------------------------------------------SIMPER ANALYSIS----------------------------------------------------------#
# setup as per help file e.g.
(simp<-with(reef.env, simper(reef.t.m, clust)))
summary(simp)


