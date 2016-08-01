
# Loading up some data
library(psych)
library(tidyr)
library(ggplot2)
library(ggthemes)
library(plyr)
library(scales)

# Load the data
source("load.R", print.eval=TRUE)


#Total participants

nrow(data)

# 1 = 13, 2 = 14
ggplot(data=data, aes(x=age, fill=age)) +
  geom_bar(colour = "black", stat="count") +
  theme_economist() +
  scale_fill_economist() +
  geom_text(stat='count', aes(label=..count..), vjust=2, colour = "white") +
  ggtitle("Age") +
  guides(fill=FALSE)

# 1 = Male, 2 = Female
ggplot(data=data, aes(x=gender, fill=gender)) +
  geom_bar(colour = "black", stat="count") +
  theme_economist() + 
  geom_text(stat='count', aes(label=..count..), vjust=2, colour = "white") +
  ggtitle("Gender") +
  scale_fill_manual(name="Gender", 
                    breaks=c("Male", "Female"), 
                    values=c("#3b5998", "#FF6E8C")) +
  guides(fill=FALSE)



ggplot(data=data, aes(x=age, fill=gender)) +
  geom_bar(stat="count", position=position_dodge(), colour="black")+
  theme_economist() +
  scale_fill_economist()+
  geom_text(stat='count', aes(label=..count..), position=position_dodge(width=0.9), vjust=-.5, colour = "black") +
  ggtitle("Gender and Age") +
  ylab("Number of Participants") +
  xlab("Age") +
  labs(fill="Gender")


# Please tell me how often you go online or use the internet, from a computer, a mobile phone, a smartphone, or any other device you may use to go online (please tick one):
ggplot(data=data, aes(x=how.often, fill=how.often)) +
  geom_bar(colour = "black", stat="count") +
  theme_economist() + 
  scale_fill_economist() +
  geom_text(stat='count', aes(label=..count..), vjust=-0.5, colour = "black") +
  ggtitle("Please tell me how often you go online or use the internet,\n from a computer, a mobile phone, a smartphone, or any other\n device you may use to go online (please tick one):") +
  guides(fill=FALSE)+
  xlab("") +
  ylab("Number of participants")

# How many hours of television do you watch every day?
ggplot(data, aes(x=round(tv.use))) +
  geom_histogram(binwidth=1, fill="#c0392b", alpha=0.75) +
  theme_economist() + 
  scale_fill_economist() +
  geom_text(stat='count', aes(label=..count..), vjust=2, colour = "white") +
  ggtitle("How many hours of television do you watch every day?") +
  ylab("Number of Participants") +
  xlab("Daily Television use in Hours") +
  geom_vline(aes(xintercept=mean(tv.use, na.rm=T)), color="red", linetype="dashed", size=1)

  describe(data$tv.use)

round(data$tv.use)

# Roughly how many people are you in contact with on social media (Please tick one):
ggplot(data=data, aes(x=contacts, fill=contacts)) +
  geom_bar(colour = "black", stat="count") +
  theme_economist() + 
  scale_fill_economist() +
  geom_text(stat='count', aes(label=..count..), vjust=2, colour = "white") +
  ggtitle("Roughly how many people are you in contact with on\n social media (Please tick one):") +
  guides(fill=FALSE)

# is your profile set to (Please tick one):
ggplot(data=data, aes(x=profile.privacy, fill=profile.privacy)) +
  geom_bar(colour = "black", stat="count") +
  guides(fill=FALSE) +
  theme_economist() + 
  scale_fill_economist() +
  geom_text(stat='count', aes(label=..count..), hjust=2, colour = "white") +
  ggtitle("is your profile set to (Please tick one):") +
  theme(plot.title = element_text(hjust = -1)) +
  coord_flip() 

# There are lots of things on the internet that are good for children of my age (Please tick one):
ggplot(data=data, aes(x=good.things, fill=good.things)) +
  geom_bar(colour = "black", stat="count") +
  theme_economist() + 
  scale_fill_economist() +
  geom_text(stat='count', aes(label=..count..), vjust=-.5, colour = "black") +
  ggtitle("There are lots of things on the internet that are good for\n children of my age") +
  guides(fill=FALSE) +
  ylab("Number of participants") +
  xlab("")

# Graphing the 'Which of these things do you know how do to?' data

# Create the data frame
task <- factor(x = c(1,2,3,4,5,6,7,8,9,10,1,2,3,4,5,6,7,8,9,10), levels = c(1,2,3,4,5,6,7,8,9,10), labels = c("Block unwanted adverts or junk mail\n spam.", "Delete the record of which sites then have\n visited.", "Change privacy settings on a social\n networking profile.", "Block messages from someone they don't\n want to hear from.", "Find information on how to use the\n internet safely.", "Publish a comment on a blog, website or\n forum.", "Upload images, videos or music onto\n social meda.", "Create a blog.", "Update status on social networking site\n used most.", "Take a picture or a short video with a\n smartphone and upload it on to social media."))
result <- factor(x = c(1,1,1,1,1,1,1,1,1,1,0,0,0,0,0,0,0,0,0,0), levels = c(1,0), labels = c("Yes", "No"))
total <- c(sum(data$blocking_1 == 1, na.rm=NA), sum(data$blocking_2 == 1, na.rm=NA), sum(data$blocking_3 == 1, na.rm=NA), sum(data$blocking_4 == 1, na.rm=NA), sum(data$blocking_5 == 1, na.rm=NA), sum(data$posting_1 == 1, na.rm=NA), sum(data$posting_2 == 1, na.rm=NA), sum(data$posting_3 == 1, na.rm=NA), sum(data$updating_1 == 1, na.rm=NA), sum(data$updating_2 == 1, na.rm=NA))
total <- c(total, sum(is.na(data$blocking_1)), sum(is.na(data$blocking_2)), sum(is.na(data$blocking_3)), sum(is.na(data$blocking_4)), sum(is.na(data$blocking_5)), sum(is.na(data$posting_1)), sum(is.na(data$posting_2)), sum(is.na(data$posting_3)), sum(is.na(data$updating_1)), sum(is.na(data$updating_2)) )
percentage <- round(total/nrow(data)*100)
pos = (percentage - 0.5 * percentage)
chart_df <- data.frame(task, result, total, percentage, pos)

ggplot(chart_df, aes(x = task, y = percentage, fill = result)) + 
  geom_bar(colour = "black", stat="identity", width= .9) +
  xlab("\nTask") +
  ylab("Percentage of respondents who ticked yes") +
  theme_economist() +
  scale_fill_economist() +
  geom_text(aes(label = ifelse(result == "Yes", paste0(percentage,"%"), ""), y = pos), position = "stack", color='white') +
  coord_flip() + 
  theme(legend.position = "bottom") +
  theme(text = element_text(size=9)) +
  theme(legend.title = element_blank()) +
  ggtitle("Which of these things do you know how to do?") +
  theme(plot.title = element_text(hjust = 2)) +
  theme(legend.position="none")


# We also want to get an average percentage of skills for each user.

# First we need to add a total skills
skills <- rowSums(cbind(data$blocking_1, data$blocking_2, data$blocking_3, data$blocking_4, data$blocking_5, data$posting_1, data$posting_2, data$posting_3, data$updating_1, data$updating_2), na.rm=TRUE)
# Average and SD
mean(skills)
sd(skills)

# Do you know how Facebook, Instagram or Youtube decide which adverts appear in your news feed?
ggplot(data=data, aes(x=how.adverts.appear, fill=how.adverts.appear)) +
  geom_bar(colour="black", stat="count") +
  theme_economist() +
  scale_fill_economist() +
  geom_text(stat='count', aes(label=..count..), vjust=2, colour = "white") +
  ggtitle("Do you know how Facebook, Instagram or Youtube decide\n which adverts appear?") +
  guides(fill=FALSE)

# Which internet platforms do you use the most? 
# Please number them in order of how much time you send on them. 1 = most often, 2 - second most often, etc. 
# You can leave ones you don't use as 0


# Combined platforms
preference <- c(1:6)
facebook <- as.data.frame(cumsum(table(data$platforms_1, exclude = 0)))
facebook <- facebook[,1]/nrow(data)
snapchat <- as.data.frame(cumsum(table(data$platforms_2, exclude = 0)))
snapchat <- snapchat[,1]/nrow(data)

# A user used 10 instead of 6, need to adjust it
data$platforms_3[data$platforms_3==10] <- 6

youtube <- as.data.frame(cumsum(table(data$platforms_3, exclude = 0)))
youtube <- youtube[,1]/nrow(data)
whatsapp <- as.data.frame(cumsum(table(data$platforms_4, exclude = 0)))
whatsapp <- whatsapp[,1]/nrow(data)
instagram <- as.data.frame(cumsum(table(data$platforms_5, exclude = 0)))
instagram <- instagram[,1]/nrow(data)

apps <- data.frame(preference, facebook, snapchat, youtube, whatsapp, instagram)

library("reshape2")
apps_long <- melt(apps, id="preference")

ggplot(data=apps_long,
aes(x=preference, y=value, colour=variable)) +
geom_line(size=3) + 
theme_economist() +
ylab("Culmative total in percent") +
xlab("Preference - 1 is most used, 6 is least used.") +
scale_x_continuous(breaks=seq(1,6,1)) +
scale_y_continuous(labels=percent) +
scale_colour_manual(name="Application", 
breaks=c("facebook", "snapchat", "youtube", "whatsapp", "instagram"), 
labels=c("Facebook", "Snapchat", "Youtube", "Whatsapp", "Instagram"),
values=c("#3b5998", "#fffc00", "#bb0000", "#4dc247", "#e95950"))

# Colors are based on the primary colour code for each platform
# Facebook #3b5998
# Snapchat #fffc00
# Youtube #bb0000
# Whatsapp #4dc247
# Instagram #e95950

# Other
ggplot(data=data, aes(x=platforms_6, fill=platforms_6)) +
geom_bar(stat="count", fill="#c0392b", alpha=0.75) +
guides(fill=FALSE)+
scale_fill_economist() +
geom_text(stat='count', aes(label=..count..), vjust=2, colour = "white") +
ggtitle("Other") +
xlab("") 

data$platforms_6_TEXT

