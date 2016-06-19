
# Clear the data at the start for a tidy environment
rm(list=ls())

# Set working director
setwd("~/GitHub/KidsFoodandFacebook")

source("load.R", print.eval=TRUE)
source("descriptives.R", print.eval=TRUE)
source("prompted_recall.R", print.eval=TRUE)

data[73,]
