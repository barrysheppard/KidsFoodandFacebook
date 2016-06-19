# Descriptives

#
# Some basic demographic information
#

# 1 = 13, 2 = 14
data$age <- factor(data$age, levels = c(1,2,3), labels = c("13","14", "other"))
summary(data$age)

# 1 = Male, 2 = Female
data$gender <- factor(data$gender, levels = c(1,2), labels = c("Male","Female"))
summary(data$gender)

# Please tell me how often you go online or use the internet, from a computer, a mobile phone, a smartphone, or any other device you may use to go online (please tick one):
data$how.often <- factor(data$how.often, levels = c(1,2,3,4), labels = c("Several times a day", "Daily or almost daily","At least weekly", "Never or almost never"))
summary(data$how.often)

# How many hours of television do you watch every day?
library("psych")
describe(data$tv.use)

# Roughly how many people are you in contact with on social media (Please tick one):
data$contacts <- factor(data$contacts, levels = c(1,2,3,4,5), labels = c("Up to 10",	"11-50",	"51-100",	"101-300",	"More than 300"))
summary(data$contacts)

# is your profile set to (Please tick one):
data$profile.privacy <- factor(data$profile.privacy, levels = c(1,2,3,4), labels = c("Public, so that everyone can see", "Partially private, so that friends of friends on your network can see", "Private, so that only your friends can see", "Don’t know"))
summary(data$profile.privacy)


# There are lots of things on the internet that are good for children of my age (Please tick one):
data$good.things <- factor(data$good.things, levels = c(1,2,3), labels = c("Not True", "A bit True", "Very True"))
summary(data$good.things)

# Which of these things do you know how to do?  (you can tick more than one)

# Graphing the 'Which of these things do you know how do to?' data

# Create the data frame
task <- factor(x = c(1,2,3,4,5,6,7,8,9,10,1,2,3,4,5,6,7,8,9,10), levels = c(1,2,3,4,5,6,7,8,9,10), labels = c("Block unwanted adverts or junk mail spam", "Delete the record of which sites they have visited", "Change privacy settings on social networking profile", "Block messages from someone they don’t want to\n hear from", "Find information on how to use the internet safely", "Publish a comment on a blog, website or forum", "Upload images, videos or music onto social meda", "Create a blog", "Update status on social networking site used most", "Take a picture or a short video with smartphone\n and upload it on to social media"))
result <- factor(x = c(1,1,1,1,1,1,1,1,1,1,0,0,0,0,0,0,0,0,0,0), levels = c(1,0), labels = c("Yes", "No"))
total <- c(sum(data$blocking_1 == 1, na.rm=NA), sum(data$blocking_2 == 1, na.rm=NA), sum(data$blocking_3 == 1, na.rm=NA), sum(data$blocking_4 == 1, na.rm=NA), sum(data$blocking_5 == 1, na.rm=NA), sum(data$posting_1 == 1, na.rm=NA), sum(data$posting_2 == 1, na.rm=NA), sum(data$posting_3 == 1, na.rm=NA), sum(data$updating_1 == 1, na.rm=NA), sum(data$updating_2 == 1, na.rm=NA))
total <- c(total, sum(is.na(data$blocking_1)), sum(is.na(data$blocking_2)), sum(is.na(data$blocking_3)), sum(is.na(data$blocking_4)), sum(is.na(data$blocking_5)), sum(is.na(data$posting_1)), sum(is.na(data$posting_2)), sum(is.na(data$posting_3)), sum(is.na(data$updating_1)), sum(is.na(data$updating_2)) )
percentage <- round(total/nrow(data)*100)
pos = (percentage - 0.5 * percentage)
chart_df <- data.frame(task, result, total, percentage, pos)

ggplot(chart_df, aes(x = task, y = percentage, fill = result)) + 
  geom_bar(stat="identity", width= .9) +
  xlab("\nTask") +
  ylab("%\n") +
  theme_economist() + 
  scale_fill_economist() +
  geom_text(aes(label = ifelse(result == "Yes", paste0(percentage,"%"), ""), y = pos), position = "stack", color='white') +
  coord_flip() + 
  theme(legend.position = "bottom") +
  theme(text = element_text(size=9)) +
  theme(legend.title = element_blank()) +
  ggtitle("Which of these things do you know how to do?") +
  theme(plot.title = element_text(hjust = -6.4)) 

# What do you like most about Facebook?
data$most.liked
# What do you like least about Facebook?
data$least.liked

# Do you know how Facebook, Instagram or Youtube decide which adverts appear in your news feed?
data$how.adverts.appear <- factor(data$how.adverts.appear, levels = c(1,2), labels = c("Yes", "No"))
table(data$how.adverts.appear)

# If yes, please explain how Facebook decides which adverts appear in your news feed.
data$explain.if.yes

# Thinking about the time you spend on the internet overall, what do you spend most time doing?  
data$most.time.spent

# Which internet platforms do you use the most? 
# Please number them in order of how much time you send on them. 1 = most often, 2 - second most often, etc. You can leave ones you don't use blank
# Facebook
table(data$platforms_1)
# Snapchat
table(data$platforms_2)
# Youtube
table(data$platforms_3)
# WhatsApp
table(data$platforms_4)
# Instagram
table(data$platforms_5)
# Other
table(data$platforms_6)
data$platforms_6_TEXT


# Table the prompted recall table
ftable(prompted_recall_table)
prop.table(prompted_recall_table, 1)

