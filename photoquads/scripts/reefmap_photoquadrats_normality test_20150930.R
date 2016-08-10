#Test for data normality to be applied to variables within the df.reef.pq.simplified3 or similar dataframes dervied from photoquadrat data.
#Run reefmap_photoquadrats_20150727.R to ensure that data is laoded
attach(df.reef.pq.simplified3)
#shapiro.test {stats}
#shapiro.test(variable to be tested)
shapiro.test(hard.coral)

#if p value is < 0.5, 
#e.g. 	Shapiro-Wilk normality test

#data:  hard.coral
#W = 0.94034, p-value = 0.008028

#Normality tests:
shapiro.test(df.reef.pq.simplified3$soft.coral)
shapiro.test(df.reef.pq.simplified3$dead.coral)
shapiro.test(df.reef.pq.simplified3$macro.algal.assemblage)


#qqplot
#dim(dataframe to be tested) to obtain teh number of rows in the dataframe, and set the y value for the qqplot
dim(df.reef.pq.simplified3) 
names(df.reef.pq.simplified3)
y = 1:56 

qqplot(df.reef.pq.simplified3$hard.coral,y)
qqplot(df.reef.pq.simplified3$soft.coral,y)
qqplot(df.reef.pq.simplified3$dead.coral,y)
qqplot(df.reef.pq.simplified3$macro.algal.assemblage,y)
#Data are non-normal - transform data. 

#prepare dataset for transformation
df.norm.test<-subset(df.reef.pq.simplified3, select=-c(depth,exposure,aspect,region))
detach(df.reef.pq.simplified3)
attach(t.arc.sc.df.norm.test)
#Arcscine used below
#open MyFucntions, run trans.arcScine to load function, then:
t.arc.sc.df.norm.test <- trans.arcsine(df.norm.test)
t.arc.sc.df.norm.test

shapiro.test(t.arc.sc.df.norm.test$soft.coral)
#OUTCOME: NaNs produced - perhaps too many to use the data
dim(t.arc.sc.df.norm.test)
rm(t.arc.sc.df.norm.test)
detach(t.arc.sc.df.norm.test)

#Log10 transformation
t.log10.df.norm.test <- trans.log10(df.norm.test)
t.log10.df.norm.test
attach(t.log10.df.norm.test)
dim(t.log10.df.norm.test)
shapiro.test(t.log10.df.norm.test$hard.coral) # OUTCOME: Approaching normality
qqplot(t.log10.df.norm.test$hard.coral,y) #OUTCOME: Approaching normality
#OUTCOME: lots of-Inf values, may not be that useful?, consider Square root transformation
rm(t.log10.df.norm.test)
detach(t.log10.df.norm.test)

#Squareroot transformation
t.sqrt.df.norm.test <- sqrt(df.norm.test)
t.sqrt.df.norm.test
attach(t.sqrt.df.norm.test)
dim(t.sqrt.df.norm.test)
shapiro.test(t.sqrt.df.norm.test$hard.coral) # OUTCOME: Normal --->bartlett--> ANOVA
qqplot(t.sqrt.df.norm.test$hard.coral,y) #OUTCOME: Normal --->bartlett--> ANOVA

shapiro.test(df.pq.norm$soft.coral) #Normal --->bartlett--> ANOVA

shapiro.test(df.pq.norm$dead.coral) # not normal ---->Levene--->KW

shapiro.test(df.pq.norm$macro.algal.assemblage) # not normal ---->Levene--->KW

#OUTCOME: Use SQUARROOT TRANSFORAMTION FOR STATISTICAL TESTS OF THIS DATA SET, AND THEN...

#Change name for easy use in next test to: df.pq.norm
rm(t.sqrt.df.norm.test)
detach(t.sqrt.df.norm.test)
df.pq.norm<-sqrt(df.norm.test)
#attach(df.pq.norm)



#consider 4th root transform or non-parametric tests.
#4th.rt.df.norm <-