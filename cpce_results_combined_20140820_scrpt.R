#Habitat classifcation statsitics to descriminate between sites, find dissimilarity, and provide ecological habitat descriptions
# http://www.unesco.org/csi/pub/source/rs11.htm

#Genreal start: set working directory:
setwd("~sif/gef/projects/mpa expansion/reef mapping/results/R/svrwandshallow/Reef_mapping_R_scripts")
#consider splitting the code into sections...as this will seem like replication of of some of what is listed below, e.x. "clusters"

#--------------------------------------------READ DATA-------------------------------------------------#
#Export results from Excel to text (tab) delimited file; Open R Studio; Import text file, and assign easy to work with name
reef <- read.delim("cpce_results_combined_201408.txt", quote="",row.names = 1)
reef.t<-t(reef) #DATA NEEDS TO BE TRANSPOSED, (nb!!)
dim(reef.t) #determine the number of rows and columns for the data matrix, replace value 16, 17 wiht this value.
str(reef.t)
View(reef.t)


#--------------------------Site by benthic cover (cpcpe data) prep (DATA PREP FOR AGGLOMARATIVE CLUSTER ANALYSIS and SIMPER)--------------------------------------------------------#



#Transform data to matrix for analysis
#reef.t.m<-data.frame(matrix(reef.t,nrow = 366, ncol = 16,,dimnames = dimnames(reef.t)))
reef.t.m<-matrix(as.numeric(reef.t),nrow = 366, ncol = 16,,dimnames = dimnames(reef.t))
str(reef.t.m)
View(reef.t.m)





#-----------------------------------CLUSTER ANAYLIS TO FOLLOW----------------------------------------------------#

#**----------BETA Flexible agglomarative Cluster analysis code R---------------------**#
#Theory: http://ecology.msu.montana.edu/labdsv/R/labs/lab13/lab13.html
#Flexible beta is recommended by Mumby et al in Remote sensing handbook: http://www.unesco.org/csi/pub/source/rs11.htm

#Load libraries needed: 
library("labdsv", lib.loc="C:/Users/gef_philip/Documents/R/win-library/2.15")
library("vegan", lib.loc="C:/Users/gef_philip/Documents/R/win-library/2.15")
library("cluster", lib.loc="C:/Program Files/R/R-2.15.3/library")

#Several parameter options exist for flexible Beta analysis - see the help file: in short the lower the vect: See http://ecology.msu.montana.edu/labdsv/R/labs/lab13/lab13.html for a useful description, and used in the example as advised by labdsv: Special case which yields a good intermediate result 

agnes.flexible<-agnes(reef.t.m, diss = inherits(reef.t.m, "dist"), metric = "euclidean",method = "flexible",par.method = c(0.625,0.625,-0.25)) # stores the values in agnes_flexible
agnes.flexible
plot(agnes.flexible, ask = FALSE, sub = paste("Agglomerative Coefficient = ",round(agnes.flexible$ac, digits = 2)),adj = 0, nmax.lab = 20, max.strlen = 5, xax.pretty = TRUE)



#**--------Interpretation of classification results using colour dendrogram plots and tables to decide on the number of clusters------------**#


#Methods: http://ecology.msu.montana.edu/labdsv/R/labs/lab14/lab14.htmlagnes.flexible.hclust
# install.packages('ape') install package first time
library(ape) # dendrogram functions
#install.packages('tree')
library(tree)
#install.packages('dendextend') # label branches with cluster number
library(dendextend)


agnes.flexible.hclust<-as.hclust(agnes.flexible) # Classification results: sets results of agnes to that of hclust class for more plotting functionality
plot(agnes.flexible.hclust, hang = -1) #plots results as hierarchical cluster
gg<-rect.hclust(agnes.flexible.hclust,k=12) #rect.hclust(agnes.flexible.hclust, 12, border = "red", cluster = 12)


#Cut the hcluster tree:
clusters.hc<-cutree(agnes.flexible.hclust,k=12) #Clusters cut the tree at k levels (should be the same as what was used for cluster)


#______________________ PRODUCE HORIZONTAL CLUSTER DIAGRAM USiNG COLOUR----------------------------------------#
#source for colouring infomration http://rpubs.com/gaston/dendrograms, others not used: https://www.biostars.org/p/16181/, http://stackoverflow.com/questions/18036094/how-to-create-a-dendrogram-with-colored-branches
# source for colour codes: http://www.rapidtables.com/web/color/RGB_Color.htm
mypal = c("#8B4513", "#F5F5DC", "#FF7F50", "#D2691E", "#CD853F", "#F4A460", "#71ABDE", "#DEB887","#BC8F8F", "#B0C4DE", "#696969", "#6495ED", "#483D8B","#008080", "#FF4500", "#8B008B", "#EE82EE", "#F0FFFF", "#FFC0CB", "#0000CD" ) # set Colour pallette: vector of 12 colours
op = par(bg = "#E8DDCB") # set background colour
plot(as.phylo(agnes.flexible.hclust), cex = 0.9, label.offset = 1, tip.color = mypal[clusters.hc], col = "red")



#------------------------BELOW PRODUCES horisontal cluster diagram, coloured branches, and cluster numbers-Requires clusters.hc to be set, as above------------#
#install.packages('ggdendro')
library("ggdendro")

dendr<- dendro_data(agnes.flexible.hclust, type="rectangle") # convert for ggplot
clust.df <- data.frame(label=names(clusters.hc), cluster=factor(clusters.hc)) 
# dendr[["labels"]] has the labels, merge with clust.df based on label column
dendr[["labels"]] <- merge(dendr[["labels"]],clust.df, by="label")
# plot the dendrogram; note use of color=cluster in geom_text(...)
ggplot() + 
        geom_segment(data=segment(dendr), aes(x=x, y=y, xend=xend, yend=yend)) + 
        geom_text(data=label(dendr), aes(x, y, label=label, hjust=0, color=cluster), 
                  size=3) +
        coord_flip() + scale_y_reverse(expand=c(0.2, 0)) + 
        theme(axis.line.y=element_blank(),
              axis.ticks.y=element_blank(),
              axis.text.y=element_blank(),
              axis.title.y=element_blank(),
              panel.background=element_rect(fill="white"),
              panel.grid=element_blank())

#---------------------------------#

#other options explored: plot(as.phylo(agnes.flexible.hclust), type = "fan", tip.color = hsv(runif(15, 0.65,0.95), 1, 1, 0.7), edge.color = hsv(runif(10, 0.65, 0.75), 1, 1, 0.7), edge.width = runif(20,0.5, 3), use.edge.length = TRUE, col = "gray80")
attach(reef.t.df)

#Evaluate number of clusters: Methods: http://ecology.msu.montana.edu/labdsv/R/labs/lab14/lab14.html
plot(as.phylo(agnes.flexible.hclust), type = "unrooted") # vry useful as overview to see the 2 dimensional distance between groups
table(clusters.hc) # Tabulate number of members (sites) there are to each cluster (k)
#Note that below is not used as normally suggested: Typically the environmental predictors are used, to the site data - the result of this may not be particularly useful...needs further research...
tree.eval<-tree(factor(clusters.hc) ~ CORAL..C. + SAND..SND. + TURF.ALGAE..TA. + SOFT.CORAL..OC. + SPONGE..SP. + SILT..SIL. + RUBBLE..RBL. + HALIMEDA..HA. + FIRE.CORAL..FC.+ ALGAL.ASSEMBLAGE..AA.+DEAD.STANDING.HARD.CORAL..DC.+CORALLINE.ALGAE..CA.+SEAGRASS..SG.+OTHER..OT.+ROCK..RCK.+DEAD.CORAL.ROCK..CR.)
plot(tree.eval) #plots the result
text(tree.eval) # labels the plotted tree
tree.eval # see the data
summary(tree.eval) # summary of the data

#Confusion matrix to evaluated confused classes
tree.eval.pred <- predict.tree(tree.eval,newdata=site,type="class")
#tree1.pred 

#Consider...but code needs work....Optpart demonstration:...assigns clusters to a specified number of clusters...
#install.packages('optpart')
#library("optpart", lib.loc="C:/Users/gef_philip/Documents/R/win-library/2.15")
#opt10 <- optpart(reef.t.m, agnes.flexible.hclust)
#table(opt5$clusid)

#-----------------------------------CLUSTER GROUPINGS TO FOLLOW---------------------------------------------------#
#this is a repeat of cluster.hc - but saved from agnes, and set as factor - consider simplifying this...you should really only need one of the two....
#Decide on cut offs for the tree: The tree needs to be grouped...it will proabbly take a few times to come up with a sensible grouping.
#cutree(tree, k = NULL, h = NULL)
clusters<-factor(cutree(agnes.flexible,12)) # choose the number of groups to cut tree with; a value can also be set, see help file on cutree

#----------------------------------Environemntal data preparartion (critical for SIMPER DATA analysis) -----------------------------------------------------#

#Convert to data frame#
reef.t.df <- data.frame(reef.t) 
str(reef.t.df)

#add clust as a factor column to an.t.df
site.cluster<-within(reef.t.df, {
        clust <-factor(clusters)
})

tapply(site.cluster$CORAL..C.,site.cluster$clust, mean)

reef.env <- subset(site.cluster, select = "clust") #This would be the Environmental descriptor dataframe. Add other descriptors like slope, asepct etc, to this...
str(reef.env)
View(reef.env)

#---------------------------------------Consider expanind the environmental data set for analysis---------------------------#
#Consider adding factors associated to the environment, such as aspect, slope, depth, and other environemtnal descriptors recorded in the access, Postgres and QGIS
#This will allow for analysis of distribution of the benthic cover data in relation to the environmental descriptors
#Again do this for PhD, or report

#--------------------------------- Consider data transformation--------------------------------------------------------------#
#Theory
#Decision regarding the importance of rarer species: tranform, or even double transform to increase their relative contribution
#this can be explored later, once initial classification of un transformed aata was generated.

#Methods in R: http://ecology.msu.montana.edu/labdsv/R/labs/lab1/lab1.html
#Transformation of Vegetation Data
#If the original dataframe is strictly numeric, and the transformation is a simple mathematical function, then we can simply operate on the whole dataframe at once. For example, to square the values in veg to get more emphasis on dominant species we could simply
#vegsq <- veg^2 
#More commonly we want to de-emphasize dominant species, and could instead use a square root transformation.
#vegsqrt <- sqrt(veg)


#-------------------------------------------SIMPER ANALYSIS----------------------------------------------------------#
#Determine the importance of the benthic cover types within each cluster
# setup as per help file e.g.
(simp<-with(reef.env, simper(reef.t.m, clust)))
summary(simp)

#another way of doing this...?
importance.table<-importance(reef.t.df, clusters.hc)
importance.table
#write(importance.table,"importance.table.txt",sep = "\t")


#Relate the sites back to clusters
#comm.table<-with(reef.env, vegemite(round(reef.t.m,0), clust))
#vegemite(reef.t.df, agnes.flexible.hclust, scale = "Hill", zero=".")



