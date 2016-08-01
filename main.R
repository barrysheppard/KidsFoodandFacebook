
# Clear the data at the start for a tidy environment
rm(list=ls())

# Set working director
setwd("~/GitHub/KidsFoodandFacebook")

source("load.R", print.eval=TRUE)
source("descriptives.R", print.eval=TRUE)
source("prompted_recall.R", print.eval=TRUE)
source("free_recall.R", print.eval=TRUE)

# Exports for SPSS
write.csv(prompted_recall, file = "SPSS_prompted_recall.csv")
write.csv(f_recall, file = "SPSS_free_recall.csv")
write.csv(spss_f_recall_sum , file = "spss_f_recall_sum.csv")
write.csv(spss_p_recall_sum , file = "spss_p_recall_sum.csv")
