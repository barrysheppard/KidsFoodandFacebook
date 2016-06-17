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

# Block unwanted adverts or junk mail spam
table(data$blocking_1)
# Delete the record of which sites they have visited
table(data$blocking_2)
# Change privacy settings on social networking profile
table(data$blocking_3)
# Block messages from someone they don’t want to hear from
table(data$blocking_4)
# Find information on how to use the internet safely
table(data$blocking_5)
# Publish a comment on a blog, website or forum
table(data$posting_1)
# Upload images, videos or music onto social media
table(data$posting_2)
# Create a blog
table(data$posting_3)
# Update status on social networking site used most
table(data$updating_1)
# Take a picture or a short video with smartphone and upload it on to social media
table(data$updating_2)

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

