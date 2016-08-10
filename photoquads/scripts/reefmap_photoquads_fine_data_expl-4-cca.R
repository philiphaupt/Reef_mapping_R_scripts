#labdsv - canonical correspondanc analysis: again, this needs continuous variables to be defined and added to site
#user specifies constrained variables a priori that will be used in a linear regressions using chi-square distance between samples to determine the relevant contribution of each contrained and unconstrained variable.these can be plotted out in ordinaion space, showing which envirnmnetla variable drives the patterns in which directions

attach(site)
cca.1 <- cca(pq.fine.pres.t~Depth+Aspect)
cca.1
cca1.plot <- plot(cca.1,choices=c(1,2))

#apply scaling to further separate
cca2.plot <- plot(cca.1,scaling=1)


#identify sites
cca1.plot <- plot(cca.1)
identify(cca1.plot,what='sites') #idenifies transect names
points(cca1.plot,what='sites',pq.fine.pres.t$"Acropora palifera (ACP)">0,col=3) #highlights opints with "Acropora palifera (ACP)" present


#Discussion

#CCA is overwhelmingly the most commonly used multivariate analysis in ecology. As an eigenanalysis it shares some of the benefits and shortcomings of eigenvector approaches. In contrast to PCA, it uses a Chi-square distance matrix rather than correlation/covariance. This solves the problem of treating joint absences as positive correlation, and is an improvement. On the other hand, as a true distance, it shares the problem of spurious "information" when two sites have no species in common, where their distance is a function of their productivity, rather than a constant. Finally, it assumes the the modes of species relate linearly to the environmental variables employed in the analysis. CCA has proven effective in an enormous number of studies, and is the first choice of most practicing ecologists.