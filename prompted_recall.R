
# Table the prompted recall table
ftable(prompted_recall_table)

# Xtabs for the prompoted recall
summary(prompted_recall_table)

library(MASS)
mytable <- xtabs(~recall+product+endorse, data=prompted_recall)


# Mutual Independence: A, B, and C are pairwise independent.
loglm(~recall+product+endorse, mytable)
# Partial Independence: A is partially independent of B and C (i.e., A is independent of the composite variable BC).
loglin(~recall+product+endorse+product*endorse, mytable)
# Conditional Independence: A is independent of B, given C.
loglm(~recall+product+endorse+recall*endorse+product*endorse, mytable)
# No Three-Way Interaction
loglm(~recall+product+endorse+recall*product+recall*endorse+product*endorse, mytable)

# install.packages("vcd")
library(vcd)
mosaic(prompted_recall_table, shade=TRUE, legend=TRUE)


# Endorsement * Product
margin.table(prompted_recall_table, c(1,2)) 
summary(margin.table(prompted_recall_table, c(1,2)))
mosaic(margin.table(prompted_recall_table, c(1,2)), shade=TRUE, legend=TRUE)

# Product * Recall
margin.table(prompted_recall_table, c(1,3)) 
summary(margin.table(prompted_recall_table, c(1,3)))
mosaic(margin.table(prompted_recall_table, c(1,3)), shade=TRUE, legend=TRUE)

# Endorsement * Recall
margin.table(prompted_recall_table, c(2,3)) 
summary(margin.table(prompted_recall_table, c(2,3)))
mosaic(margin.table(prompted_recall_table, c(2,3)), shade=TRUE, legend=TRUE)



### begin copying script here
likelihood.test = function(x) {
  nrows = dim(x)[1]                      # no. of rows in contingency table
  ncols = dim(x)[2]                      # no. of cols in contingency table
  chi.out = chisq.test(x,correct=F)      # do a Pearson chi square test
  table = chi.out[[6]]                   # get the OFs
  ratios = chi.out[[6]]/chi.out[[7]]     # calculate OF/EF ratios
  sum = 0                                # storage for the test statistic
  for (i in 1:nrows) {
    for (j in 1:ncols) {
      sum = sum + table[i,j]*log(ratios[i,j])
    }
  }
  sum = 2 * sum                          # the likelihood ratio chi square
  df = chi.out[[2]]                      # degrees of freedom
  p = 1 - pchisq(sum,df)                 # p-value
  out = c(sum, df, p, chi.out[[1]])      # the output vector
  names(out) = c("LR-chisq","df","p-value","Pears-chisq")
  round(out,4)                           # done!
}
### end copying script here and paste into R Console


# Product * Recall
margin.table(prompted_recall_table, c(1,3)) 
summary(margin.table(prompted_recall_table, c(1,3)))
mosaic(margin.table(prompted_recall_table, c(1,3)), shade=TRUE, legend=TRUE)
likelihood.test(margin.table(prompted_recall_table, c(1,3)))

# Bayes Factor
install.packages('BayesFactor', dependencies = TRUE)
library(BayesFactor)

# Product * Recall
contingencyTableBF(margin.table(prompted_recall_table, c(1,3)), sampleType = "indepMulti", fixedMargin = "cols")
# Endorsement * Recall
contingencyTableBF(margin.table(prompted_recall_table, c(2,3)), sampleType = "indepMulti", fixedMargin = "cols")
# Product * Endorsement * Recall
contingencyTableBF(prompted_recall_table, sampleType = "indepMulti", fixedMargin = "cols")



# Endorsement * Recall
margin.table(prompted_recall_table, c(2,3)) 
summary(margin.table(prompted_recall_table, c(2,3)))
# chisq.test(margin.table(prompted_recall_table, c(2,3)))
mosaic(margin.table(prompted_recall_table, c(2,3)), shade=TRUE, legend=TRUE)
likelihood.test(margin.table(prompted_recall_table, c(2,3)))




# loglm(~ Class + Sex + Age + Survived, data=Titanic)
loglm(~ endorse + product + recall, data=prompted_recall_table)

# loglm(~ Class * Sex * Age * Survived, data=Titanic)
# Saturated model
loglm(~ endorse * product * recall, data=prompted_recall_table)

# Without the 4/3 way interaction
# loglm(~ Class * Sex * Age * Survived - Class:Sex:Age:Survived, data=Titanic)
loglm(~ endorse * product * recall - endorse:product:recall, data=prompted_recall_table)
