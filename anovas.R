
# Lets have some fun with ANOVAs!

# Load the data
source("load.R", print.eval=TRUE)

# The data of interest is f_recall_sum for free recall and p_recall_sum for prompted recall


# Free Recall First

f_recall_sum

library(psych)
# Overall total
describe(f_recall_sum$rating)
# Totals broken up into categories
describeBy(f_recall_sum$rating, f_recall_sum$profile)
describeBy(f_recall_sum$rating, f_recall_sum$Product)
describeBy(f_recall_sum$rating, f_recall_sum$Endorse)

# Is there a difference in recall for Product only
fit <- aov(rating ~ Product, data=f_recall_sum)
layout(matrix(c(1,2,3,4),2,2)) # optional layout 
plot(fit)
summary(fit) # display Type I ANOVA table
drop1(fit,~.,test="F") # type III SS and F Tests

fit <- aov(rating ~ Endorse, data=f_recall_sum)
layout(matrix(c(1,2,3,4),2,2)) # optional layout 
plot(fit)
summary(fit) # display Type I ANOVA table
drop1(fit,~.,test="F") # type III SS and F Tests

fit <- aov(rating ~ Product*Endorse, data=f_recall_sum)
layout(matrix(c(1,2,3,4),2,2)) # optional layout 
plot(fit)
summary(fit) # display Type I ANOVA table
drop1(fit,~.,test="F") # type III SS and F Tests


# Next Prompted Recall

library(psych)
# Overall total
describe(p_recall_sum$rating)
# Totals broken up into categories
describeBy(p_recall_sum$rating, p_recall_sum$profile)
describeBy(p_recall_sum$rating, p_recall_sum$Product)
describeBy(p_recall_sum$rating, p_recall_sum$Endorse)

# Is there a difference in recall for Product only
fit <- aov(rating ~ Product, data=p_recall_sum)
layout(matrix(c(1,2,3,4),2,2)) # optional layout 
plot(fit)
summary(fit) # display Type I ANOVA table
drop1(fit,~.,test="F") # type III SS and F Tests

fit <- aov(rating ~ Endorse, data=p_recall_sum)
layout(matrix(c(1,2,3,4),2,2)) # optional layout 
plot(fit)
summary(fit) # display Type I ANOVA table
drop1(fit,~.,test="F") # type III SS and F Tests

fit <- aov(rating ~ Product*Endorse, data=p_recall_sum)
layout(matrix(c(1,2,3,4),2,2)) # optional layout 
plot(fit)
summary(fit) # display Type I ANOVA table
drop1(fit,~.,test="F") # type III SS and F Tests
