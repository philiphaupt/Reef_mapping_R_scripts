#significant difference among groups #test for homogeneity of variance
#RUN AFTER NORMALITY TEST SCRIPT

#bartletts test for hard coral,
#DEPTH
bartlett.test(df.pq.norm$hard.coral~depth,data = df.pq.norm) #OUTCOME suggests homoscedastic, continue with ANOVA for hard coral. So as long as SQRT transform is used for hard.coral, df.reef.pq.simplified3 can be used.
bartlett.test(df.pq.norm$soft.coral~depth,data = df.pq.norm)

#ASPECT
bartlett.test(df.pq.norm$hard.coral~aspect,data = df.pq.norm) #OUTCOME suggests homoscedastic, continue with ANOVA for hard coral. So as long as SQRT transform is used for hard.coral, df.reef.pq.simplified3 can be used.
bartlett.test(df.pq.norm$soft.coral~aspect,data = df.pq.norm)

#REGION
bartlett.test(df.pq.norm$hard.coral~region,data = df.pq.norm) #OUTCOME suggests homoscedastic, continue with ANOVA for hard coral. So as long as SQRT transform is used for hard.coral, df.reef.pq.simplified3 can be used.
bartlett.test(df.pq.norm$soft.coral~region,data = df.pq.norm)


#### decide which dataframe to use: I am using df.reef.pq.simplified3, and only considering sqrt(hard.coral), then run:
#Now run ANOVA script
