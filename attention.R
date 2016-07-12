
# Set working director
setwd("~/GitHub/KidsFoodandFacebook")

# Load the data from csv file, this comes from the Qualtrics survey, headers are loaded separately as the second row is extra header info not needed 
eyetracking <- read.csv("EyetrackingAttention.csv")

# Profiles 1 to 18 have the advert in the first post, 19 to 36 have it in the second post
# All the other posts are distractors. The third post does not have any image and is shorter than the others.

id <- eyetracking$id
p01 <- eyetracking$P001advert1
p02 <- eyetracking$P002advert1
p03 <- eyetracking$P003advert1
p04 <- eyetracking$P004advert1
p05 <- eyetracking$P005advert1
p06 <- eyetracking$P006advert1
p07 <- eyetracking$P007advert1
p08 <- eyetracking$P008advert1
p09 <- eyetracking$P009advert1
p10 <- eyetracking$P010advert1
p11 <- eyetracking$P011advert1
p12 <- eyetracking$P012advert1
p13 <- eyetracking$P013advert1
p14 <- eyetracking$P014advert1
p15 <- eyetracking$P015advert1
p16 <- eyetracking$P016advert1
p17 <- eyetracking$P017advert1
p18 <- eyetracking$P018advert1
p19 <- eyetracking$P019advert2
p20 <- eyetracking$P020advert2
p21 <- eyetracking$P021advert2
p22 <- eyetracking$P022advert2
p23 <- eyetracking$P023advert2
p24 <- eyetracking$P024advert2
p25 <- eyetracking$P025advert2
p26 <- eyetracking$P026advert2
p27 <- eyetracking$P027advert2
p28 <- eyetracking$P028advert2
p29 <- eyetracking$P029advert2
p30 <- eyetracking$P030advert2
p31 <- eyetracking$P031advert2
p32 <- eyetracking$P032advert2
p33 <- eyetracking$P033advert2
p34 <- eyetracking$P034advert2
p35 <- eyetracking$P035advert2
p36 <- eyetracking$P036advert2

# Create data frames groups into celebrity, peer, and sponsored
p_attention <- data.frame(id, p01, p02, p03, p04, p05, p06, p07, p08, p09, p10, p11, p12, p13, p14, p15, p16, p17, p18, p19, p20, p21, p22, p23, p24, p25, p26, p27, p28, p29, p30, p31, p32, p33, p34, p35, p36)

# Converting to long format
library(tidyr)
attention_long <- gather(attention, profile, time, p01:p36)


# Profile ratings - 
# Unhealthy: 01:03, 10:12, 19:21, 28:30
attention_long$product[attention_long$profile == "p01" | attention_long$profile == "p02" | attention_long$profile == "p03"] <- "Unhealthy"
attention_long$product[attention_long$profile == "p10" | attention_long$profile == "p11" | attention_long$profile == "p12"] <- "Unhealthy"
attention_long$product[attention_long$profile == "p19" | attention_long$profile == "p20" | attention_long$profile == "p21"] <- "Unhealthy"
attention_long$product[attention_long$profile == "p28" | attention_long$profile == "p29" | attention_long$profile == "p30"] <- "Unhealthy"
# Healthy: 04:06, 13:15, 22:24, 31:33
attention_long$product[attention_long$profile == "p04" | attention_long$profile == "p05" | attention_long$profile == "p06"] <- "Healthy"
attention_long$product[attention_long$profile == "p13" | attention_long$profile == "p14" | attention_long$profile == "p15"] <- "Healthy"
attention_long$product[attention_long$profile == "p22" | attention_long$profile == "p23" | attention_long$profile == "p24"] <- "Healthy"
attention_long$product[attention_long$profile == "p31" | attention_long$profile == "p32" | attention_long$profile == "p33"] <- "Healthy"
# Nonfood: 07:09, 16:18, 25:27, 34:36
attention_long$product[attention_long$profile == "p07" | attention_long$profile == "p08" | attention_long$profile == "p09"] <- "Nonfood"
attention_long$product[attention_long$profile == "p16" | attention_long$profile == "p17" | attention_long$profile == "p18"] <- "Nonfood"
attention_long$product[attention_long$profile == "p25" | attention_long$profile == "p26" | attention_long$profile == "p27"] <- "Nonfood"
attention_long$product[attention_long$profile == "p34" | attention_long$profile == "p35" | attention_long$profile == "p36"] <- "Nonfood"
# Celebrity: 02, 05, 08, 11, 14, 17, 20, 23, 26, 29, 32, 36
attention_long$endorse[attention_long$profile == "p02" | attention_long$profile == "p05" | attention_long$profile == "p08"] <- "Celebrity"
attention_long$endorse[attention_long$profile == "p11" | attention_long$profile == "p14" | attention_long$profile == "p17"] <- "Celebrity"
attention_long$endorse[attention_long$profile == "p20" | attention_long$profile == "p23" | attention_long$profile == "p26"] <- "Celebrity"
attention_long$endorse[attention_long$profile == "p29" | attention_long$profile == "p32" | attention_long$profile == "p36"] <- "Celebrity"
# Peer: 01, 04, 07, 10, 13, 16, 19, 22, 27, 28, 31, 34
attention_long$endorse[attention_long$profile == "p01" | attention_long$profile == "p04" | attention_long$profile == "p07"] <- "Peer"
attention_long$endorse[attention_long$profile == "p10" | attention_long$profile == "p13" | attention_long$profile == "p16"] <- "Peer"
attention_long$endorse[attention_long$profile == "p19" | attention_long$profile == "p22" | attention_long$profile == "p27"] <- "Peer"
attention_long$endorse[attention_long$profile == "p28" | attention_long$profile == "p31" | attention_long$profile == "p34"] <- "Peer"
# Sponsored: 03, 06, 09, 12, 15, 18, 21, 24, 25, 30, 33, 35
attention_long$endorse[attention_long$profile == "p03" | attention_long$profile == "p06" | attention_long$profile == "p09"] <- "Sponsored"
attention_long$endorse[attention_long$profile == "p12" | attention_long$profile == "p15" | attention_long$profile == "p18"] <- "Sponsored"
attention_long$endorse[attention_long$profile == "p21" | attention_long$profile == "p24" | attention_long$profile == "p25"] <- "Sponsored"
attention_long$endorse[attention_long$profile == "p30" | attention_long$profile == "p33" | attention_long$profile == "p35"] <- "Sponsored"

# Tidy up the data, changing to factors and numbers and removing NAs
attention_long$product <- factor(attention_long$product)
attention_long$endorse <- factor(attention_long$endorse)
attention_long$time <- as.numeric(attention_long$time)
attention_long$time[is.na(attention_long$time)] <- 0

# Showing the average time spent viewing
by(attention_long$time, attention_long$id, mean)

# Removing participant M001 as the data was is so low it appears to be flawed
attention_long <- attention_long[attention_long$id != "M001",]

# Refactor to remove M001
attention_long$id <- factor(attention_long$id)

# Showing the average time spent viewing
by(attention_long$time, attention_long$id, mean)


#
model <- aov(time ~ product + endorse + product:endorse, data = attention_long)
anova(model)
?anova

# Calculating Effect size as per Andy Fields Discovering Statistics Using R output 12.4

omega_factorial <- function(n, a, b, SSa, SSb, SSab, SSr)
{
  MSa <- SSa/(a-1)
  MSb <- SSb/(b-1)
  MSab <- SSab/((a-1)*(b-1))
  MSr <- SSr/(a*b*(n-1))
  varA <- ((a-1)*(MSa-MSr))/(n*a*b)
  varB <- ((b-1)*(MSb-MSr))/(n*a*b)
  varAB <- ((a-1)*(b-1)*(MSab-MSr))/(n*a*b)
  varTotal <- varA + varB + varAB + MSr
  print(paste("Omega-Squared A:", varA/varTotal))
  print(paste("Omega-Squared B:", varB/varTotal))
  print(paste("Omega-Squared AB:", varAB/varTotal))
}

omega_factorial(3, 3, 3, 21.9, 50.16, 89.11, 942.48)


# The below summarySE is from http://www.cookbook-r.com/Manipulating_data/Summarizing_data/

## Summarizes data.
## Gives count, mean, standard deviation, standard error of the mean, and confidence interval (default 95%).
##   data: a data frame.
##   measurevar: the name of a column that contains the variable to be summariezed
##   groupvars: a vector containing names of columns that contain grouping variables
##   na.rm: a boolean that indicates whether to ignore NA's
##   conf.interval: the percent range of the confidence interval (default is 95%)
summarySE <- function(data=NULL, measurevar, groupvars=NULL, na.rm=FALSE,
                      conf.interval=.95, .drop=TRUE) {
  library(plyr)
  
  # New version of length which can handle NA's: if na.rm==T, don't count them
  length2 <- function (x, na.rm=FALSE) {
    if (na.rm) sum(!is.na(x))
    else       length(x)
  }
  
  # This does the summary. For each group's data frame, return a vector with
  # N, mean, and sd
  datac <- ddply(data, groupvars, .drop=.drop,
                 .fun = function(xx, col) {
                   c(N    = length2(xx[[col]], na.rm=na.rm),
                     mean = mean   (xx[[col]], na.rm=na.rm),
                     sd   = sd     (xx[[col]], na.rm=na.rm)
                   )
                 },
                 measurevar
  )
  
  # Rename the "mean" column    
  datac <- rename(datac, c("mean" = measurevar))
  
  datac$se <- datac$sd / sqrt(datac$N)  # Calculate standard error of the mean
  
  # Confidence interval multiplier for standard error
  # Calculate t-statistic for confidence interval: 
  # e.g., if conf.interval is .95, use .975 (above/below), and use df=N-1
  ciMult <- qt(conf.interval/2 + .5, datac$N-1)
  datac$ci <- datac$se * ciMult
  
  return(datac)
}

# Generate some se and ci
attention_chart_data <- summarySE(attention_long, measurevar="time", groupvars=c("product","endorse"))
attention_chart_data_product <- summarySE(attention_long, measurevar="time", groupvars="product")
attention_chart_data_endorse <- summarySE(attention_long, measurevar="time", groupvars="endorse")

attention_chart_data_product
attention_chart_data_endorse


# Lets plot this stuff
library(ggplot2)
library(ggthemes)
ggplot(attention_chart_data, aes(x=product, y=time, fill=endorse)) + 
  geom_bar(position=position_dodge(), stat="identity") +
  geom_errorbar(aes(ymin=time-ci, ymax=time+ci),
                width=.2,                    # Width of the error bars
                position=position_dodge(.9)) +
scale_x_discrete(name="Content Type") +
scale_y_continuous(name="Time (Seconds)") + 
expand_limits(y=c(0,15)) +
theme_economist() + 
scale_fill_economist()
  
# Product
ggplot(attention_chart_data_product, aes(x=product, y=time, fill=product)) + 
  geom_bar(position=position_dodge(), stat="identity") +
  geom_errorbar(aes(ymin=time-ci, ymax=time+ci),
                width=.2,                    # Width of the error bars
                position=position_dodge(.9)) +
  scale_x_discrete(name="Content Type") +
  scale_y_continuous(name="Time (Seconds)") + 
  theme_economist() + 
  scale_fill_economist()

# Endorse
ggplot(attention_chart_data_endorse, aes(x=endorse, y=time, fill=endorse)) + 
  geom_bar(position=position_dodge(), stat="identity") +
  geom_errorbar(aes(ymin=time-ci, ymax=time+ci),
                width=.2,                    # Width of the error bars
                position=position_dodge(.9)) +
  scale_x_discrete(name="Content Type") +
  scale_y_continuous(name="Time (Seconds)") + 
  theme_economist() + 
  scale_fill_economist()

  
attention_chart_data
