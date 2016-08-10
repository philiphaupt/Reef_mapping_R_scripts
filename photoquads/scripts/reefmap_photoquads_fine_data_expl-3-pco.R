#lab dsv 8 PCO

#evalaute distance matrix, using bray curtis for example
dis.bray <- vegdist(pq.fine.pres.t,method="bray")
disana(dis.bray)

#euclidian distance
dis.euc <- dist(pq.fine.pres.t,'euclidean')
euc.pco <- pco(dis.euc,k=10)
barplot(euc.pco$eig)
plot(euc.pco)

#manhattan distance
dis.mht <- dist(pq.fine.pres.t,"manhattan")
mht.pco <- pco(dis.mht,k=10)
plot(mht.pco)

#binary distance
dis.bin <- dist(pq.fine.pres.t,"binary")
bin.pco <- pco(dis.bin,k=10)
plot(bin.pco)

#plot of eigenvalues to show the differences between the three oridantions
plot(euc.pco$eig/sum(euc.pco$eig),type="b",xlab="Axis Number",
     ylab="Fraction of Sum")
lines(mht.pco$eig/sum(mht.pco$eig),type="b",col=2)
lines(bin.pco$eig/sum(bin.pco$eig),type="b",col=3)
text(8.0,0.45,'Euclidean')
text(8.0,0.4,"Manhattan",col=2)
text(8.0,0.35,"binary",col=3)

#investigate the dsitribution of a species in ordination space
#common species "Porites branching (PORB)"
plot(bin.pco,title="Porites branching (PORB)")
points(bin.pco,pq.fine.pres.t$"Porites branching (PORB)">0)

#medium common "Acropora palifera (ACP)" 
plot(bin.pco,title="Acropora palifera (ACP)" )
points(bin.pco,pq.fine.pres.t$"Acropora palifera (ACP)" >0)


#medium common "Sand (SND)" 
plot(bin.pco,title="Sand (SND)"  )
points(bin.pco,pq.fine.pres.t$"Sand (SND)"  >0)


#plot the probability of the distribution of species XXX, e.g. "Acropora palifera (ACP)") using the LabDSV function surf() 
plot(bin.pco,title="Acropora palifera (ACP)")
surf(bin.pco,pq.fine.pres.t$"Acropora palifera (ACP)">0,family=binomial)
#examine the D^2 score, the cloer to 1 the higher the predicitve value of the pco - can use this to compare the different methods, chnging which pco.data set is used, e.g. pco.euc

#examine the distribution of environmental variables on the ordination using the LabDSV surf function. {Needs continuous variable}
plot(bin.pco,title="Depth")
surf(bin.pco,site$Depth)

#add more variables,e.g. sediment, wave, slope, once these variables have been made continuous and incorporated into site.
plot(bin.pco)
surf(bin.pco,site$Depth)
surf(bin.pco,site$slope,col=3)
surf(bin.pco,site$sediment,col=4)