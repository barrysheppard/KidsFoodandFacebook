
library("tidyr")

# Load the data from csv file, this comes from the Qualtrics survey, headers are loaded separately as the second row is extra header info not needed 
header <- read.csv("Facebook.csv", nrows=1, header=FALSE, stringsAsFactors=FALSE)
data <- read.csv("Facebook.csv", skip=2, header=FALSE, stringsAsFactors=FALSE)
names(data) <- header[1,]

# Load the eye tracking data from csv file, this comes from the Qualtrics survey, headers are loaded separately as the second row is extra header info not needed 
eye_header <- read.csv("Eyetracking.csv", nrows=1, header=FALSE, stringsAsFactors=FALSE)
eye_data <- read.csv("Eyetracking.csv", skip=2, header=FALSE, stringsAsFactors=FALSE)
names(eye_data) <- eye_header[1,]

# Free Recall data needed to be manually coded and is saved in a seperate spreadsheet
free_recall <- read.csv("free_recall.csv", stringsAsFactors=FALSE)
free_recall[is.na(free_recall)] <- 0


library("dplyr")
data <- bind_rows(data, eye_data)

# Delete the extra fields added by Qualtrics which are blank or nonrelevant
delete <- c("V2", "V3", "V4", "V5", "V6", "V7", "V8", "V9", "V10", "SC0_0", "SC0_1", "SC0_2", "logo", "intro", "error.message", "thanks", "LocationLatitude", "LocationLongitude", "LocationAccuracy","instructions","Q385")
delete <- c(delete, "profile.f001", "profile.f002", "profile.f003", "profile.f004", "profile.f005", "profile.f006", "profile.f007", "profile.f008", "profile.f009", "profile.f010", "profile.f011", "profile.f012", "profile.f013", "profile.f014", "profile.f015", "profile.f016", "profile.f017", "profile.f018", "profile.f019", "profile.f020", "profile.f021", "profile.f022", "profile.f023", "profile.f024", "profile.f025", "profile.f026", "profile.f027", "profile.f028", "profile.f029", "profile.f030", "profile.f031", "profile.f032", "profile.f033", "profile.f034", "profile.f035", "profile.f036")
delete <- c(delete, "profile.m001", "profile.m002", "profile.m003", "profile.m004", "profile.m005", "profile.m006", "profile.m007", "profile.m008", "profile.m009", "profile.m010", "profile.M011", "profile.m012", "profile.m013", "profile.m014", "profile.m015", "profile.m016", "profile.m017", "profile.m018", "profile.m019", "profile.m020", "profile.m021", "profile.m022", "profile.M023", "profile.m024", "profile.m025", "profile.m026", "profile.m027", "profile.m028", "profile.m029", "profile.m030", "profile.m031", "profile.m032", "profile.m033", "profile.m034", "profile.m035", "profile.m036")

data <- data[, !(names(data) %in% delete)] 
names(data)[1]="id"

eye_data <- eye_data[, !(names(eye_data) %in% delete)] 
names(eye_data)[1]="id"

# Remove variables no longer needed
rm(delete)
rm(header)
rm(eye_header)
rm(eye_data)

# The first schools data for tv.use hours was incorrect due to a validation error and should be marked as NA
data$tv.use[1:9] <- NA
# One entry was recorded as 24, which would suggested they were watching 24 hours of tv a day, assuming this is an error it is being removed
data$tv.use[data$tv.use==24] <- NA
# identify any instances where no answers were selected as these are likely to be incomplete
data$total_fam <- rowSums(data[,320:375], na.rm = TRUE)
fam_true <- data$total_fam != 0
# identify any instances where no answers were correctly selected from the 
free_true <- free_recall$count!=99


# Code some the relevant totals




# Coding the Free Recall data

# Profile ratings - 
# Unhealthy: 01:03, 10:12, 19:21, 28:30
# Healthy: 04:06, 13:15, 22:24, 31:33
# Nonfood: 07:09, 16:18, 25:27, 34:36
# Celebrity: 02, 05, 08, 11, 14, 17, 20, 23, 26, 29, 32, 36
# Peer: 01, 04, 07, 10, 13, 16, 19, 22, 27, 31, 34
# Sponsored: 03, 06, 09, 12, 15, 18, 21, 24, 25, 30, 33, 35


p01 <- as.integer(free_recall[free_true,]$freerecall_1)
p02 <- as.integer(free_recall[free_true,]$freerecall_3)
p03 <- as.integer(free_recall[free_true,]$freerecall_2)
p04 <- as.integer(free_recall[free_true,]$freerecall_5)
p05 <- as.integer(free_recall[free_true,]$freerecall_6)
p06 <- as.integer(free_recall[free_true,]$freerecall_44)
p07 <- as.integer(free_recall[free_true,]$freerecall_8)
p08 <- as.integer(free_recall[free_true,]$freerecall_9 + free_recall[free_true,]$freerecall_7)
p09 <- as.integer(free_recall[free_true,]$freerecall_10)
p10 <- as.integer(free_recall[free_true,]$freerecall_11)
p11 <- as.integer(free_recall[free_true,]$freerecall_12)
p12 <- as.integer(free_recall[free_true,]$freerecall_13)
p13 <- as.integer(free_recall[free_true,]$freerecall_14)
p14 <- as.integer(free_recall[free_true,]$freerecall_15)
p15 <- as.integer(free_recall[free_true,]$freerecall_16)
p16 <- as.integer(free_recall[free_true,]$freerecall_17)
p17 <- as.integer(free_recall[free_true,]$freerecall_18 + free_recall[free_true,]$freerecall_38)
p18 <- as.integer(free_recall[free_true,]$freerecall_19)
p19 <- as.integer(free_recall[free_true,]$freerecall_20)
p20 <- as.integer(free_recall[free_true,]$freerecall_21)
p21 <- as.integer(free_recall[free_true,]$freerecall_22)
p22 <- as.integer(free_recall[free_true,]$freerecall_23)
p23 <- as.integer(free_recall[free_true,]$freerecall_24)
p24 <- as.integer(free_recall[free_true,]$freerecall_25)
p25 <- as.integer(free_recall[free_true,]$freerecall_26)
p26 <- as.integer(free_recall[free_true,]$freerecall_27 + free_recall[free_true,]$freerecall_39)
p27 <- as.integer(free_recall[free_true,]$freerecall_28)
p28 <- as.integer(free_recall[free_true,]$freerecall_29)
p29 <- as.integer(free_recall[free_true,]$freerecall_30)
p30 <- as.integer(free_recall[free_true,]$freerecall_31)
p31 <- as.integer(free_recall[free_true,]$freerecall_32)
p32 <- as.integer(free_recall[free_true,]$freerecall_33)
p33 <- as.integer(free_recall[free_true,]$freerecall_41)
p34 <- as.integer(free_recall[free_true,]$freerecall_35 + free_recall[free_true,]$freerecall_40)
p35 <- as.integer(free_recall[free_true,]$freerecall_36)
p36 <- as.integer(free_recall[free_true,]$freerecall_4)

# Create data frames groups into celebrity, peer, and sponsored
rating<- data.frame(data[free_true,]$id, p01, p02, p03, p04, p05, p06, p07, p08, p09, p10, p11, p12, p13, p14, p15, p16, p17, p18, p19, p20, p21, p22, p23, p24, p25, p26, p27, p28, p29, p30, p31, p32, p33, p34, p35, p36)

# Convert any of the NAs into 0s
rating[is.na(rating)] <- 0

# Converting to long format
rating_long <- gather(rating, profile, rating, p01:p36)

# Any 2s should be changed to 1, this is where the user incorrectly ticked the product of the opposite gender in addition to their own
rating_long[rating_long == 2] <- 1

# Create data frames for each of the nine conditions

# Unhealthy Peer
unhealthy_peer <- data.frame(id = free_recall[free_true,]$id, recall = p01)
unhealthy_peer <- rbind(unhealthy_peer, data.frame(id = free_recall[free_true,]$id, recall = p10))
unhealthy_peer <- rbind(unhealthy_peer, data.frame(id = free_recall[free_true,]$id, recall = p19))
unhealthy_peer <- rbind(unhealthy_peer, data.frame(id = free_recall[free_true,]$id, recall = p28))
unhealthy_peer$product <- factor(1, levels = c(1,2,3), labels = c("Unhealthy", "Healthy","NonFood"))
unhealthy_peer$endorse <- factor(1, levels = c(1,2,3), labels = c("Peer", "Celebrity","Sponsored"))

# Unhealthy Celebrity
unhealthy_celebrity <- data.frame(id = free_recall[free_true,]$id, recall = p02)
unhealthy_celebrity <- rbind(unhealthy_celebrity, data.frame(id = free_recall[free_true,]$id, recall = p11))
unhealthy_celebrity <- rbind(unhealthy_celebrity, data.frame(id = free_recall[free_true,]$id, recall = p20))
unhealthy_celebrity <- rbind(unhealthy_celebrity, data.frame(id = free_recall[free_true,]$id, recall = p29))
unhealthy_celebrity$product <- factor(1, levels = c(1,2,3), labels = c("Unhealthy", "Healthy","NonFood"))
unhealthy_celebrity$endorse <- factor(2, levels = c(1,2,3), labels = c("Peer", "Celebrity","Sponsored"))

# Unhealthy Sponsored
unhealthy_sponsored <- data.frame(id = free_recall[free_true,]$id, recall = p03)
unhealthy_sponsored <- rbind(unhealthy_sponsored, data.frame(id = free_recall[free_true,]$id, recall = p12))
unhealthy_sponsored <- rbind(unhealthy_sponsored, data.frame(id = free_recall[free_true,]$id, recall = p21))
unhealthy_sponsored <- rbind(unhealthy_sponsored, data.frame(id = free_recall[free_true,]$id, recall = p30))
unhealthy_sponsored$product <- factor(1, levels = c(1,2,3), labels = c("Unhealthy", "Healthy","NonFood"))
unhealthy_sponsored$endorse <- factor(3, levels = c(1,2,3), labels = c("Peer", "Celebrity","Sponsored"))

# Healthy Peer
healthy_peer <- data.frame(id = free_recall[free_true,]$id, recall = p04)
healthy_peer <- rbind(healthy_peer, data.frame(id = free_recall[free_true,]$id, recall = p13))
healthy_peer <- rbind(healthy_peer, data.frame(id = free_recall[free_true,]$id, recall = p22))
healthy_peer <- rbind(healthy_peer, data.frame(id = free_recall[free_true,]$id, recall = p31))
healthy_peer$product <- factor(2, levels = c(1,2,3), labels = c("Unhealthy", "Healthy","NonFood"))
healthy_peer$endorse <- factor(1, levels = c(1,2,3), labels = c("Peer", "Celebrity","Sponsored"))

# Healthy Celebrity
healthy_celebrity <- data.frame(id = free_recall[free_true,]$id, recall = p05)
healthy_celebrity <- rbind(healthy_celebrity, data.frame(id = free_recall[free_true,]$id, recall = p14))
healthy_celebrity <- rbind(healthy_celebrity, data.frame(id = free_recall[free_true,]$id, recall = p23))
healthy_celebrity <- rbind(healthy_celebrity, data.frame(id = free_recall[free_true,]$id, recall = p32))
healthy_celebrity$product <- factor(2, levels = c(1,2,3), labels = c("Unhealthy", "Healthy","NonFood"))
healthy_celebrity$endorse <- factor(2, levels = c(1,2,3), labels = c("Peer", "Celebrity","Sponsored"))

# Healthy Sponsored
healthy_sponsored<- data.frame(id = free_recall[free_true,]$id, recall = p06)
healthy_sponsored<- rbind(healthy_sponsored, data.frame(id = free_recall[free_true,]$id, recall = p15))
healthy_sponsored<- rbind(healthy_sponsored, data.frame(id = free_recall[free_true,]$id, recall = p24))
healthy_sponsored<- rbind(healthy_sponsored, data.frame(id = free_recall[free_true,]$id, recall = p33))
healthy_sponsored$product <- factor(2, levels = c(1,2,3), labels = c("Unhealthy", "Healthy","NonFood"))
healthy_sponsored$endorse <- factor(3, levels = c(1,2,3), labels = c("Peer", "Celebrity","Sponsored"))

# Nonfood: 07:09, 16:18, 25:27, 34:36

# Nonfood Peer
nonfood_peer <- data.frame(id = free_recall[free_true,]$id, recall = p07)
nonfood_peer <- rbind(nonfood_peer, data.frame(id = free_recall[free_true,]$id, recall = p16))
nonfood_peer <- rbind(nonfood_peer, data.frame(id = free_recall[free_true,]$id, recall = p27))
nonfood_peer <- rbind(nonfood_peer, data.frame(id = free_recall[free_true,]$id, recall = p34))
nonfood_peer$product <- factor(3, levels = c(1,2,3), labels = c("Unhealthy", "Healthy","NonFood"))
nonfood_peer$endorse <- factor(1, levels = c(1,2,3), labels = c("Peer", "Celebrity","Sponsored"))

# Nonfood Celebrity
nonfood_celebrity <- data.frame(id = free_recall[free_true,]$id, recall = p08)
nonfood_celebrity <- rbind(nonfood_celebrity, data.frame(id = free_recall[free_true,]$id, recall = p17))
nonfood_celebrity <- rbind(nonfood_celebrity, data.frame(id = free_recall[free_true,]$id, recall = p26))
nonfood_celebrity <- rbind(nonfood_celebrity, data.frame(id = free_recall[free_true,]$id, recall = p36))
nonfood_celebrity$product <- factor(3, levels = c(1,2,3), labels = c("Unhealthy", "Healthy","NonFood"))
nonfood_celebrity$endorse <- factor(2, levels = c(1,2,3), labels = c("Peer", "Celebrity","Sponsored"))

# Nonfood Sponsored
nonfood_sponsored <- data.frame(id = free_recall[free_true,]$id, recall = p09)
nonfood_sponsored <- rbind(nonfood_sponsored, data.frame(id = free_recall[free_true,]$id, recall = p18))
nonfood_sponsored <- rbind(nonfood_sponsored, data.frame(id = free_recall[free_true,]$id, recall = p25))
nonfood_sponsored <- rbind(nonfood_sponsored, data.frame(id = free_recall[free_true,]$id, recall = p35))
nonfood_sponsored$product <- factor(3, levels = c(1,2,3), labels = c("Unhealthy", "Healthy","NonFood"))
nonfood_sponsored$endorse <- factor(3, levels = c(1,2,3), labels = c("Peer", "Celebrity","Sponsored"))

f_recall <- rbind(unhealthy_peer, healthy_peer, nonfood_peer, unhealthy_celebrity, healthy_celebrity, nonfood_celebrity, unhealthy_sponsored, healthy_sponsored, nonfood_sponsored)

# Any 2s should be changed to 1, this is where the user incorrectly ticked the product of the opposite gender in addition to their own
f_recall[f_recall == 2] <- 1

# Convert any of the NAs into 0s
f_recall[is.na(f_recall)] <- 0

# Change Recall into a factor
f_recall$recall <- factor(f_recall$recall, levels = c(0,1), labels = c("Forgot", "Recalled"))

# Remove the ID as I don't think we need it. This line can be removed if it turns out we do need it
f_recall$id <- NULL

# Delete random stuff
delete <- c("healthy_celebrity", "healthy_peer", "healthy_sponsored", "nonfood_celebrity", "nonfood_peer", "nonfood_sponsored", "p01", "p02", "p03", "p04")
delete <- c(delete, "p05", "p06", "p07")        
delete <- c(delete, "p08", "p09", "p10", "p11", "p12", "p13", "p14", "p15", "p16", "p17", "p18", "p19", "p20", "p21", "p22", "p23", "p24", "p25", "p26")        
delete <- c(delete, "p27", "p28", "p29", "p30", "p31", "p32", "p33", "p34", "p35", "p36", "unhealthy_celebrity", "unhealthy_peer", "unhealthy_sponsored")
rm(list = delete)
rm(delete)
rm(rating_long)

# Coding the Prompted Recall data

# Profile ratings - 
# Unhealthy: 01:03, 10:12, 19:21, 28:30
# Healthy: 04:06, 13:15, 22:24, 31:33
# Nonfood: 07:09, 16:18, 25:27, 34:36
# Celebrity: 02, 05, 08, 11, 14, 17, 20, 23, 26, 29, 32, 36
# Peer: 01, 04, 07, 10, 13, 16, 19, 22, 27, 31, 34
# Sponsored: 03, 06, 09, 12, 15, 18, 21, 24, 25, 30, 33, 35

p01 <- as.integer(data[fam_true,]$familiarity_1)
p02 <- as.integer(data[fam_true,]$familiarity_3)
p03 <- as.integer(data[fam_true,]$familiarity_2)
p04 <- as.integer(data[fam_true,]$familiarity_5)
p05 <- as.integer(data[fam_true,]$familiarity_6)
p06 <- as.integer(data[fam_true,]$familiarity_44)
p07 <- as.integer(data[fam_true,]$familiarity_8)
p08 <- as.integer(data[fam_true,]$familiarity_9 + data[fam_true,]$familiarity_37)
p09 <- as.integer(data[fam_true,]$familiarity_10)
p10 <- as.integer(data[fam_true,]$familiarity_11)
p11 <- as.integer(data[fam_true,]$familiarity_12)
p12 <- as.integer(data[fam_true,]$familiarity_13)
p13 <- as.integer(data[fam_true,]$familiarity_14)
p14 <- as.integer(data[fam_true,]$familiarity_15)
p15 <- as.integer(data[fam_true,]$familiarity_16)
p16 <- as.integer(data[fam_true,]$familiarity_17)
p17 <- as.integer(data[fam_true,]$familiarity_18 + data[fam_true,]$familiarity_38)
p18 <- as.integer(data[fam_true,]$familiarity_19)
p19 <- as.integer(data[fam_true,]$familiarity_20)
p20 <- as.integer(data[fam_true,]$familiarity_21)
p21 <- as.integer(data[fam_true,]$familiarity_22)
p22 <- as.integer(data[fam_true,]$familiarity_23)
p23 <- as.integer(data[fam_true,]$familiarity_24)
p24 <- as.integer(data[fam_true,]$familiarity_25)
p25 <- as.integer(data[fam_true,]$familiarity_26)
p26 <- as.integer(data[fam_true,]$familiarity_27 + data[fam_true,]$familiarity_39)
p27 <- as.integer(data[fam_true,]$familiarity_28)
p28 <- as.integer(data[fam_true,]$familiarity_29)
p29 <- as.integer(data[fam_true,]$familiarity_30)
p30 <- as.integer(data[fam_true,]$familiarity_31)
p31 <- as.integer(data[fam_true,]$familiarity_32)
p32 <- as.integer(data[fam_true,]$familiarity_33)
p33 <- as.integer(data[fam_true,]$familiarity_41)
p34 <- as.integer(data[fam_true,]$familiarity_35 + data[fam_true,]$familiarity_40)
p35 <- as.integer(data[fam_true,]$familiarity_36)
p36 <- as.integer(data[fam_true,]$familiarity_4)

# Create data frames groups into celebrity, peer, and sponsored
rating<- data.frame(data[fam_true,]$id, p01, p02, p03, p04, p05, p06, p07, p08, p09, p10, p11, p12, p13, p14, p15, p16, p17, p18, p19, p20, p21, p22, p23, p24, p25, p26, p27, p28, p29, p30, p31, p32, p33, p34, p35, p36)

# Give a total of the correct responses 
rating$count <- rowSums(rating[,2:37], na.rm=TRUE)

# Convert any of the NAs into 0s
rating[is.na(rating)] <- 0

# Converting to long format
rating_long <- gather(rating, profile, rating, p01:p36)

# Any 2s should be changed to 1, this is where the user incorrectly ticked the product of the opposite gender in addition to their own
rating_long[rating_long == 2] <- 1

# Create data frames for each of the nine conditions

# Unhealthy Peer
unhealthy_peer <- data.frame(id = data[fam_true,]$id, recall = p01)
unhealthy_peer <- rbind(unhealthy_peer, data.frame(id = data[fam_true,]$id, recall = p10))
unhealthy_peer <- rbind(unhealthy_peer, data.frame(id = data[fam_true,]$id, recall = p19))
unhealthy_peer <- rbind(unhealthy_peer, data.frame(id = data[fam_true,]$id, recall = p28))
unhealthy_peer$product <- factor(1, levels = c(1,2,3), labels = c("Unhealthy", "Healthy","NonFood"))
unhealthy_peer$endorse <- factor(1, levels = c(1,2,3), labels = c("Peer", "Celebrity","Sponsored"))

# Unhealthy Celebrity
unhealthy_celebrity <- data.frame(id = data[fam_true,]$id, recall = p02)
unhealthy_celebrity <- rbind(unhealthy_celebrity, data.frame(id = data[fam_true,]$id, recall = p11))
unhealthy_celebrity <- rbind(unhealthy_celebrity, data.frame(id = data[fam_true,]$id, recall = p20))
unhealthy_celebrity <- rbind(unhealthy_celebrity, data.frame(id = data[fam_true,]$id, recall = p29))
unhealthy_celebrity$product <- factor(1, levels = c(1,2,3), labels = c("Unhealthy", "Healthy","NonFood"))
unhealthy_celebrity$endorse <- factor(2, levels = c(1,2,3), labels = c("Peer", "Celebrity","Sponsored"))

# Unhealthy Sponsored
unhealthy_sponsored <- data.frame(id = data[fam_true,]$id, recall = p03)
unhealthy_sponsored <- rbind(unhealthy_sponsored, data.frame(id = data[fam_true,]$id, recall = p12))
unhealthy_sponsored <- rbind(unhealthy_sponsored, data.frame(id = data[fam_true,]$id, recall = p21))
unhealthy_sponsored <- rbind(unhealthy_sponsored, data.frame(id = data[fam_true,]$id, recall = p30))
unhealthy_sponsored$product <- factor(1, levels = c(1,2,3), labels = c("Unhealthy", "Healthy","NonFood"))
unhealthy_sponsored$endorse <- factor(3, levels = c(1,2,3), labels = c("Peer", "Celebrity","Sponsored"))

# Healthy Peer
healthy_peer <- data.frame(id = data[fam_true,]$id, recall = p04)
healthy_peer <- rbind(healthy_peer, data.frame(id = data[fam_true,]$id, recall = p13))
healthy_peer <- rbind(healthy_peer, data.frame(id = data[fam_true,]$id, recall = p22))
healthy_peer <- rbind(healthy_peer, data.frame(id = data[fam_true,]$id, recall = p31))
healthy_peer$product <- factor(2, levels = c(1,2,3), labels = c("Unhealthy", "Healthy","NonFood"))
healthy_peer$endorse <- factor(1, levels = c(1,2,3), labels = c("Peer", "Celebrity","Sponsored"))

# Healthy Celebrity
healthy_celebrity <- data.frame(id = data[fam_true,]$id, recall = p05)
healthy_celebrity <- rbind(healthy_celebrity, data.frame(id = data[fam_true,]$id, recall = p14))
healthy_celebrity <- rbind(healthy_celebrity, data.frame(id = data[fam_true,]$id, recall = p23))
healthy_celebrity <- rbind(healthy_celebrity, data.frame(id = data[fam_true,]$id, recall = p32))
healthy_celebrity$product <- factor(2, levels = c(1,2,3), labels = c("Unhealthy", "Healthy","NonFood"))
healthy_celebrity$endorse <- factor(2, levels = c(1,2,3), labels = c("Peer", "Celebrity","Sponsored"))

# Healthy Sponsored
healthy_sponsored<- data.frame(id = data[fam_true,]$id, recall = p06)
healthy_sponsored<- rbind(healthy_sponsored, data.frame(id = data[fam_true,]$id, recall = p15))
healthy_sponsored<- rbind(healthy_sponsored, data.frame(id = data[fam_true,]$id, recall = p24))
healthy_sponsored<- rbind(healthy_sponsored, data.frame(id = data[fam_true,]$id, recall = p33))
healthy_sponsored$product <- factor(2, levels = c(1,2,3), labels = c("Unhealthy", "Healthy","NonFood"))
healthy_sponsored$endorse <- factor(3, levels = c(1,2,3), labels = c("Peer", "Celebrity","Sponsored"))

# Nonfood: 07:09, 16:18, 25:27, 34:36

# Nonfood Peer
nonfood_peer <- data.frame(id = data[fam_true,]$id, recall = p07)
nonfood_peer <- rbind(nonfood_peer, data.frame(id = data[fam_true,]$id, recall = p16))
nonfood_peer <- rbind(nonfood_peer, data.frame(id = data[fam_true,]$id, recall = p27))
nonfood_peer <- rbind(nonfood_peer, data.frame(id = data[fam_true,]$id, recall = p34))
nonfood_peer$product <- factor(3, levels = c(1,2,3), labels = c("Unhealthy", "Healthy","NonFood"))
nonfood_peer$endorse <- factor(1, levels = c(1,2,3), labels = c("Peer", "Celebrity","Sponsored"))

# Nonfood Celebrity
nonfood_celebrity <- data.frame(id = data[fam_true,]$id, recall = p08)
nonfood_celebrity <- rbind(nonfood_celebrity, data.frame(id = data[fam_true,]$id, recall = p17))
nonfood_celebrity <- rbind(nonfood_celebrity, data.frame(id = data[fam_true,]$id, recall = p26))
nonfood_celebrity <- rbind(nonfood_celebrity, data.frame(id = data[fam_true,]$id, recall = p36))
nonfood_celebrity$product <- factor(3, levels = c(1,2,3), labels = c("Unhealthy", "Healthy","NonFood"))
nonfood_celebrity$endorse <- factor(2, levels = c(1,2,3), labels = c("Peer", "Celebrity","Sponsored"))

# Nonfood Sponsored
nonfood_sponsored <- data.frame(id = data[fam_true,]$id, recall = p09)
nonfood_sponsored <- rbind(nonfood_sponsored, data.frame(id = data[fam_true,]$id, recall = p18))
nonfood_sponsored <- rbind(nonfood_sponsored, data.frame(id = data[fam_true,]$id, recall = p25))
nonfood_sponsored <- rbind(nonfood_sponsored, data.frame(id = data[fam_true,]$id, recall = p35))
nonfood_sponsored$product <- factor(3, levels = c(1,2,3), labels = c("Unhealthy", "Healthy","NonFood"))
nonfood_sponsored$endorse <- factor(3, levels = c(1,2,3), labels = c("Peer", "Celebrity","Sponsored"))

prompted_recall <- rbind(unhealthy_peer, healthy_peer, nonfood_peer, unhealthy_celebrity, healthy_celebrity, nonfood_celebrity, unhealthy_sponsored, healthy_sponsored, nonfood_sponsored)

# Any 2s should be changed to 1, this is where the user incorrectly ticked the product of the opposite gender in addition to their own
prompted_recall[prompted_recall == 2] <- 1

# Convert any of the NAs into 0s
prompted_recall[is.na(prompted_recall)] <- 0

# Change Recall into a factor
prompted_recall$recall <- factor(prompted_recall$recall, levels = c(0,1), labels = c("Forgot", "Recalled"))

# Remove the ID as I don't think we need it. This line can be removed if it turns out we do need it
prompted_recall$id <- NULL


# Delete random stuff
delete <- c("healthy_celebrity", "healthy_peer", "healthy_sponsored", "nonfood_celebrity", "nonfood_peer", "nonfood_sponsored", "p01", "p02", "p03", "p04")
delete <- c(delete, "p05", "p06", "p07")        
delete <- c(delete, "p08", "p09", "p10", "p11", "p12", "p13", "p14", "p15", "p16", "p17", "p18", "p19", "p20", "p21", "p22", "p23", "p24", "p25", "p26")        
delete <- c(delete, "p27", "p28", "p29", "p30", "p31", "p32", "p33", "p34", "p35", "p36", "unhealthy_celebrity", "unhealthy_peer", "unhealthy_sponsored")

rm(list = delete)
rm(delete)
rm(rating_long)


# Factoring

# 1 = 13, 2 = 14
data$age <- factor(data$age, levels = c(1,2,3), labels = c("13","14", "other"))

# 1 = Male, 2 = Female
data$gender <- factor(data$gender, levels = c(1,2), labels = c("Male","Female"))

# Please tell me how often you go online or use the internet, from a computer, a mobile phone, a smartphone, or any other device you may use to go online (please tick one):
data$how.often <- factor(data$how.often, levels = c(1,2,3,4), labels = c("Several times a day", "Daily or almost daily","At least weekly", "Never or almost never"))

# Roughly how many people are you in contact with on social media (Please tick one):
data$contacts <- factor(data$contacts, levels = c(1,2,3,4,5), labels = c("Up to 10",	"11-50",	"51-100",	"101-300",	"More than 300"))
# summary(data$contacts)

# is your profile set to (Please tick one):
data$profile.privacy <- factor(data$profile.privacy, levels = c(1,2,3,4), labels = c("Public, so that everyone\n can see", "Partially private, so that\n friends of friends on your\n network can see", "Private, so that only\n your friends can see", "Don't know"))

# There are lots of things on the internet that are good for children of my age (Please tick one):
data$good.things <- factor(data$good.things, levels = c(1,2,3), labels = c("Not True", "A bit True", "Very True"))
# summary(data$good.things)

# Do you know how Facebook, Instagram or Youtube decide which adverts appear in your news feed?
data$how.adverts.appear <- factor(data$how.adverts.appear, levels = c(1,2), labels = c("Yes", "No"))

