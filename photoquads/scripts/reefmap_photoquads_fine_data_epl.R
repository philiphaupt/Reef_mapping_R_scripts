#http://ecology.msu.montana.edu/labdsv/R/labs/lab1/lab1.html

CPCe_photoquadrat_results_fine_20160306 <- read.delim2("E:/stats/reefmap.pq.2013/photoquads/data/CPCe_photoquadrat_results_fine_20160306.txt", row.names=1)
CPCe_photoquadrat_fine_presence_20160306 <- read.delim2("E:/stats/reefmap.pq.2013/photoquads/data/CPCe_photoquadrat_fine_presence_20160306.txt", row.names=1)


pq.fine.t<- as.data.frame(t(CPCe_photoquadrat_results_fine_20160306))

pq.fine.pres.t<-as.data.frame(t(CPCe_photoquadrat_fine_presence_20160306))

dim(pq.fine.t)
str(pq.fine.t)
str(pq.fine.pres.t)
dim(pq.fine.pres.t)

spc_pres<-apply(pq.fine.pres.t>0,2,sum) # to get number of presences for each species.  
# Note that the first part of the function 
# call (veg>0) evaluates to TRUE/FALSE or 1/0), 
# and it is the sum of ones and zeros that 
# gets calculated.

plot(sort(spc_pres))     # to see a plot of the cumulative empirical density 
# function (CEDF) for species presences

plot(sort(spc_pres),log='y',
     main="Cumulative Distribution of Species Occurrences",
     xlab='Cumulative Count of Species',ylab='Number of Plots')

# number of species that occurred in fewer than 10 transects
seq(1,169)[sort(spc_pres)==10]

#result
#Seriatopora (SERI)
#Stylocoeniella (STYLA)
#Stylophora (STYL)
#Symphyllia (SYMP)

#To see the actual distribution, with species names attached, simply enter
sort(spc_pres)
hist(spc_pres)

tmp <- apply(pq.fine.t,2,sum)
spc_mean <- tmp/spc_pres   


plot(sort(spc_mean),main="Cumulative Species Abundance Distribution",
     xlab="Cumulative Number of Species",ylab="Mean Abundance")

plot(spc_pres,spc_mean)
#identify(spc_pres,spc_mean,names(pq.fine.t))
abuocc(pq.fine.t)

#SAME FOR CORAL: Explore number of corals
#corals<-slice(pq.fine.pres.t, 7:74)

#abunadnce bu coral species not working need to cahnge names no brackets in names!!!
#Add coral names
attach(pq.fine.pres.t,pq.fine.pres.t)
rename(pq.fine.pres.t,pq.fine.pres.t, Acanthastrea (ACANT) = acanthastria)
corals<-select(pq.fine.pres.t,pq.fine.pres.t$Acanthastrea (ACANT) : pq.fine.pres.t$Unkown coral (UNK))
sp_names<-data_frame(rownames(CPCe_photoquadrat_results_fine_20160306))
sp_names<-slice(sp_names,7:74)
dim(corals)
dim(sp_names)
corals$sp<-sp_names
dim(corals)

summarise(corals&sp,)

