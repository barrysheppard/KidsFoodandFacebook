
source("load.R", print.eval=TRUE)

names(p_recall_sum)

colnames(p_recall_sum) <- c("PID","profile", "Recalled","Content","Context")

p_recalled.aov <- with(p_recall_sum,
                   aov(Recalled ~ Content * Context +
                         Error(PID / (Content * Context)))
)
summary(p_recalled.aov)


install.packages("multcomp")
library("multcomp")
?glht
