
# Loading up some data
library(plyr)
library(dplyr)
library(tidyr)
library(psych)
library(BayesFactor)
library(MASS)
library(vcd)
library(vcdExtra)
library(gmodels)

source("load.R", print.eval=TRUE)

# I'm being lazy and reusing the prompted_recall code as is
prompted_recall <- plyr::rename(f_recall, c("recall"="Recall", "product"="Product", "endorse"="Endorsement"))
# Table for prompted Recall
prompted_recall_table <- xtabs(~Product + Endorsement + Recall, data=prompted_recall)


## Analysis

### Responses

# Total responses
table(free_true)

# count the total responses per person (only valid responses)
print(c("Mean of responses", round(mean(free_recall[free_true,]$responses), 2)))
print(c("Standard deviation of responses", round(sd(free_recall[free_true,]$responses),2)))

# Count of correct data
print(c("Mean of correct responses", round(mean(free_recall[free_true,]$count),2)))
print(c("Standard deviation of correct responses", round(sd(free_recall[free_true,]$count),2)))

## 2x2 Breakdowns - Products

### Product: Healthy vs Unhealthy

# Subset the data to remove NonFood so we can compare Unhealthy and Healthy 
data_subset <- subset(prompted_recall, Product!="NonFood")
# Refactor Product to remove the unused factor
data_subset$Product <- factor(data_subset$Product)
# Prepare the data
prompted_recall_table_1 <- xtabs(~ Product + Recall, data_subset)
# Show the data + results
CrossTable(prompted_recall_table_1, chisq = TRUE, expected = TRUE, asresid = TRUE, format = "SPSS", digits = 2)

# Bayes Factors - indepMultinomial, generalised form
BF <- contingencyTableBF(prompted_recall_table_1, sampleType = "indepMulti", fixedMargin = "rows")
BF
print(c("BF10:", format(exp(BF@bayesFactor$bf), scientific=FALSE)))

# This is a Log Linear model and displayed as a mosaic, a little excessive as we're only displaying the table
model_joint_indep <- loglm(formula = ~ Product + Recall, data=prompted_recall_table_1)
plot(model_joint_indep, panel = mosaic, type = "observed", pop=FALSE, shade=FALSE, legend=FALSE)
labeling_cells(text = prompted_recall_table_1, margin = 0)(prompted_recall_table_1)


### Product: Nonfood vs Unhealthy


# Subset the data to remove NonFood so we can compare Unhealthy and Nonfood
data_subset <- subset(prompted_recall, Product!="Healthy")
# Refactor Product to remove the unused factor
data_subset$Product <- factor(data_subset$Product)
# Prepare the data
prompted_recall_table_1 <- xtabs(~Product + Recall, data_subset)
# Show the data + results
CrossTable(prompted_recall_table_1, chisq = TRUE, expected = TRUE, asresid = TRUE, format = "SPSS", digits = 2)
# Bayes Factors
BF <- contingencyTableBF(prompted_recall_table_1, sampleType = "indepMulti", fixedMargin = "rows")
BF
print(c("BF10:", format(exp(BF@bayesFactor$bf), scientific=FALSE)))
# This is a Log Linear model and displayed as a mosaic, a little excessive as we're only displaying the table
model_joint_indep <- loglm(formula = ~ Product + Recall, data=prompted_recall_table_1)
plot(model_joint_indep, panel = mosaic, type = "observed", pop=FALSE, shade=FALSE, legend=FALSE)
labeling_cells(text = prompted_recall_table_1, margin = 0)(prompted_recall_table_1)


### Product: Healthy vs Nonfood

# Subset the data to remove NonFood so we can compare Healthy and Nonfood
data_subset <- subset(prompted_recall, Product!="Unhealthy")
# Refactor Product to remove the unused factor
data_subset$Product <- factor(data_subset$Product)
# Prepare the data
prompted_recall_table_1 <- xtabs(~Product + Recall, data_subset)
# Show the data + Results
CrossTable(prompted_recall_table_1, chisq = TRUE, expected = TRUE, asresid = TRUE, format = "SPSS", digits = 2)
# Bayes Factors
BF <- contingencyTableBF(prompted_recall_table_1, sampleType = "indepMulti", fixedMargin = "rows")
BF
print(c("BF10:", format(exp(BF@bayesFactor$bf), scientific=FALSE)))
# This is a Log Linear model and displayed as a mosaic, a little excessive as we're only displaying the table
model_joint_indep <- loglm(formula = ~ Product + Recall, data=prompted_recall_table_1)
plot(model_joint_indep, panel = mosaic, type = "observed", pop=FALSE, shade=FALSE, legend=FALSE)
labeling_cells(text = prompted_recall_table_1, margin = 0)(prompted_recall_table_1)


## 2x2 Breakdowns - Endorsement

### Endorsement: Peer vs Celebrity

# Subset the data to remove Sponsored so we can compare Peer and Celebrity 
data_subset <- subset(prompted_recall, Endorsement!="Sponsored")
# Refactor Endorsement to remove the unused factor
data_subset$Endorsement <- factor(data_subset$Endorsement)
# Prepare the data
prompted_recall_table_1 <- xtabs(~Endorsement + Recall, data_subset)
# Show the data + results
CrossTable(prompted_recall_table_1, chisq = TRUE, expected = TRUE, asresid = TRUE, format = "SPSS", digits = 2)
# Bayes Factors
BF <- contingencyTableBF(prompted_recall_table_1, sampleType = "indepMulti", fixedMargin = "rows")
BF
print(c("BF10:", format(exp(BF@bayesFactor$bf), scientific=FALSE)))
# This is a Log Linear model and displayed as a mosaic, a little excessive as we're only displaying the table
model_joint_indep <- loglm(formula = ~ Endorsement + Recall, data=prompted_recall_table_1)
plot(model_joint_indep, panel = mosaic, type = "observed", pop=FALSE, shade=FALSE, legend=FALSE)
labeling_cells(text = prompted_recall_table_1, margin = 0)(prompted_recall_table_1)

### Endorsement: Peer vs Sponsored


# Subset the data to remove Celebrity so we can compare Peer and Sponsored 
data_subset <- subset(prompted_recall, Endorsement!="Celebrity")
# Refactor Endorsement to remove the unused factor
data_subset$Endorsement <- factor(data_subset$Endorsement)
# Prepare the data
prompted_recall_table_1 <- xtabs(~Endorsement + Recall, data_subset)
# Show the data + results
CrossTable(prompted_recall_table_1, chisq = TRUE, expected = TRUE, asresid = TRUE, format = "SPSS", digits = 2)
# Bayes Factors
BF <- contingencyTableBF(prompted_recall_table_1, sampleType = "indepMulti", fixedMargin = "rows")
BF
print(c("BF10:", format(exp(BF@bayesFactor$bf), scientific=FALSE)))
# This is a Log Linear model and displayed as a mosaic, a little excessive as we're only displaying the table
model_joint_indep <- loglm(formula = ~ Endorsement + Recall, data=prompted_recall_table_1)
plot(model_joint_indep, panel = mosaic, type = "observed", pop=FALSE, shade=FALSE, legend=FALSE)
labeling_cells(text = prompted_recall_table_1, margin = 0)(prompted_recall_table_1)


### Endorsement: Celebrity vs Sponsored


# Subset the data to remove Peer so we can compare Celebrity and Sponsored
data_subset <- subset(prompted_recall, Endorsement!="Peer")
# Refactor Endorsement to remove the unused factor
data_subset$Endorsement <- factor(data_subset$Endorsement)
# Prepare the data
prompted_recall_table_1 <- xtabs(~Endorsement + Recall, data_subset)
# Show the data + results
CrossTable(prompted_recall_table_1, chisq = TRUE, expected = TRUE, asresid = TRUE, format = "SPSS", digits = 2)
# Bayes Factors
BF <- contingencyTableBF(prompted_recall_table_1, sampleType = "indepMulti", fixedMargin = "rows")
BF
print(c("BF10:", format(exp(BF@bayesFactor$bf), scientific=FALSE)))
# This is a Log Linear model and displayed as a mosaic, a little excessive as we're only displaying the table
model_joint_indep <- loglm(formula = ~ Endorsement + Recall, data=prompted_recall_table_1)
plot(model_joint_indep, panel = mosaic, type = "observed", pop=FALSE, shade=FALSE, legend=FALSE)
labeling_cells(text = prompted_recall_table_1, margin = 0)(prompted_recall_table_1)


## Analysis

# Full interaction of Product * endorsement * Recall


# Table the prompted Recall table
ftable(prompted_recall_table)
table <- ftable(prompted_recall_table)
print("Percentages")
round(prop.table(table, 1),2)

# Xtabs for the prompoted Recall
P1 <- summary(prompted_recall_table)
P1
print(c("P = ", format(P1$p.value, scientific=FALSE)))

BF1 <- contingencyTableBF(prompted_recall_table, sampleType = "indepMulti", fixedMargin = "rows")
BF1
print(c("BF10:", format(exp(BF1@bayesFactor$bf), scientific=FALSE)))




# The Log Linear Analysis


# Table for prompted Recall
prompted_recall_table <- xtabs(~ Product + Endorsement + Recall, data=prompted_recall)

# Saturated model
saturated <- loglm(~ Endorsement * Product * Recall, data=prompted_recall_table)
summary(saturated)
# Three way interaction
threeway <- loglm(~ Endorsement + Product + Recall + Endorsement:Product + Endorsement:Recall + Product:Recall, data=prompted_recall_table)
summary(threeway)
# Compare the models
anova(threeway, saturated)




# Further breakdowns based on residuals

# Subset the data to compare Celebrity and Peer context in the Unhealthy product type
data_subset <- subset(prompted_recall, Product =="Unhealthy")
data_subset <- subset(data_subset, Endorsement!="Sponsored")
# Refactor Product to remove the unused factor
data_subset$Product <- factor(data_subset$Product)
data_subset$Endorsement <- factor(data_subset$Endorsement)
# Prepare the data
prompted_recall_table_1 <- xtabs(~Endorsement + Recall, data_subset)
# Show the data + Results
CrossTable(prompted_recall_table_1, chisq = TRUE, expected = TRUE, asresid = TRUE, format = "SPSS", digits = 2)
# Bayes Factors
BF <- contingencyTableBF(prompted_recall_table_1, sampleType = "indepMulti", fixedMargin = "rows")
BF
print(c("BF10:", format(exp(BF@bayesFactor$bf), scientific=FALSE)))
# This is a Log Linear model and displayed as a mosaic, a little excessive as we're only displaying the table
model_joint_indep <- loglm(formula = ~ Endorsement + Recall, data=prompted_recall_table_1)
plot(model_joint_indep, panel = mosaic, type = "observed", pop=FALSE, shade=FALSE, legend=FALSE)
labeling_cells(text = prompted_recall_table_1, margin = 0)(prompted_recall_table_1)


# Subset the data to compare Sponsored and Peer context in the Unhealthy product type
data_subset <- subset(prompted_recall, Product =="Unhealthy")
data_subset <- subset(data_subset, Endorsement!="Celebrity")
# Refactor Product to remove the unused factor
data_subset$Product <- factor(data_subset$Product)
data_subset$Endorsement <- factor(data_subset$Endorsement)
# Prepare the data
prompted_recall_table_1 <- xtabs(~Endorsement + Recall, data_subset)
# Show the data + Results
CrossTable(prompted_recall_table_1, chisq = TRUE, expected = TRUE, asresid = TRUE, format = "SPSS", digits = 2)
# Bayes Factors
BF <- contingencyTableBF(prompted_recall_table_1, sampleType = "indepMulti", fixedMargin = "rows")
BF
print(c("BF10:", format(exp(BF@bayesFactor$bf), scientific=FALSE)))
# This is a Log Linear model and displayed as a mosaic, a little excessive as we're only displaying the table
model_joint_indep <- loglm(formula = ~ Endorsement + Recall, data=prompted_recall_table_1)
plot(model_joint_indep, panel = mosaic, type = "observed", pop=FALSE, shade=FALSE, legend=FALSE)
labeling_cells(text = prompted_recall_table_1, margin = 0)(prompted_recall_table_1)


# Subset the data to compare Peer and Sponsored context in the Healthy product type
data_subset <- subset(prompted_recall, Product =="Healthy")
data_subset <- subset(data_subset, Endorsement!="Celebrity")
# Refactor Product to remove the unused factor
data_subset$Product <- factor(data_subset$Product)
data_subset$Endorsement <- factor(data_subset$Endorsement)
# Prepare the data
prompted_recall_table_1 <- xtabs(~Endorsement + Recall, data_subset)
# Show the data + Results
CrossTable(prompted_recall_table_1, chisq = TRUE, expected = TRUE, asresid = TRUE, format = "SPSS", digits = 2)
# Bayes Factors
BF <- contingencyTableBF(prompted_recall_table_1, sampleType = "indepMulti", fixedMargin = "rows")
BF
print(c("BF10:", format(exp(BF@bayesFactor$bf), scientific=FALSE)))
# This is a Log Linear model and displayed as a mosaic, a little excessive as we're only displaying the table
model_joint_indep <- loglm(formula = ~ Endorsement + Recall, data=prompted_recall_table_1)
plot(model_joint_indep, panel = mosaic, type = "observed", pop=FALSE, shade=FALSE, legend=FALSE)
labeling_cells(text = prompted_recall_table_1, margin = 0)(prompted_recall_table_1)



# Subset the data to compare Peer and Celebrity context in the Healthy product type
data_subset <- subset(prompted_recall, Product =="Healthy")
data_subset <- subset(data_subset, Endorsement!="Sponsored")
# Refactor Product to remove the unused factor
data_subset$Product <- factor(data_subset$Product)
data_subset$Endorsement <- factor(data_subset$Endorsement)
# Prepare the data
prompted_recall_table_1 <- xtabs(~Endorsement + Recall, data_subset)
# Show the data + Results
CrossTable(prompted_recall_table_1, chisq = TRUE, expected = TRUE, asresid = TRUE, format = "SPSS", digits = 2)
# Bayes Factors
BF <- contingencyTableBF(prompted_recall_table_1, sampleType = "indepMulti", fixedMargin = "rows")
BF
print(c("BF10:", format(exp(BF@bayesFactor$bf), scientific=FALSE)))
# This is a Log Linear model and displayed as a mosaic, a little excessive as we're only displaying the table
model_joint_indep <- loglm(formula = ~ Endorsement + Recall, data=prompted_recall_table_1)
plot(model_joint_indep, panel = mosaic, type = "observed", pop=FALSE, shade=FALSE, legend=FALSE)
labeling_cells(text = prompted_recall_table_1, margin = 0)(prompted_recall_table_1)




# Subset the data to compare Peer and Sponsored context in the NonFood product type
data_subset <- subset(prompted_recall, Product =="NonFood")
data_subset <- subset(data_subset, Endorsement!="Celebrity")
# Refactor Product to remove the unused factor
data_subset$Product <- factor(data_subset$Product)
data_subset$Endorsement <- factor(data_subset$Endorsement)
# Prepare the data
prompted_recall_table_1 <- xtabs(~Endorsement + Recall, data_subset)
# Show the data + Results
CrossTable(prompted_recall_table_1, chisq = TRUE, expected = TRUE, asresid = TRUE, format = "SPSS", digits = 2)
# Bayes Factors
BF <- contingencyTableBF(prompted_recall_table_1, sampleType = "indepMulti", fixedMargin = "rows")
BF
print(c("BF10:", format(exp(BF@bayesFactor$bf), scientific=FALSE)))
# This is a Log Linear model and displayed as a mosaic, a little excessive as we're only displaying the table
model_joint_indep <- loglm(formula = ~ Endorsement + Recall, data=prompted_recall_table_1)
plot(model_joint_indep, panel = mosaic, type = "observed", pop=FALSE, shade=FALSE, legend=FALSE)
labeling_cells(text = prompted_recall_table_1, margin = 0)(prompted_recall_table_1)




# Subset the data to compare Peer and Celebrity context in the Healthy product type
data_subset <- subset(prompted_recall, Product =="NonFood")
data_subset <- subset(data_subset, Endorsement!="Peer")
# Refactor Product to remove the unused factor
data_subset$Product <- factor(data_subset$Product)
data_subset$Endorsement <- factor(data_subset$Endorsement)
# Prepare the data
prompted_recall_table_1 <- xtabs(~Endorsement + Recall, data_subset)
# Show the data + Results
CrossTable(prompted_recall_table_1, chisq = TRUE, expected = TRUE, asresid = TRUE, format = "SPSS", digits = 2)
# Bayes Factors
BF <- contingencyTableBF(prompted_recall_table_1, sampleType = "indepMulti", fixedMargin = "rows")
BF
print(c("BF10:", format(exp(BF@bayesFactor$bf), scientific=FALSE)))
# This is a Log Linear model and displayed as a mosaic, a little excessive as we're only displaying the table
model_joint_indep <- loglm(formula = ~ Endorsement + Recall, data=prompted_recall_table_1)
plot(model_joint_indep, panel = mosaic, type = "observed", pop=FALSE, shade=FALSE, legend=FALSE)
labeling_cells(text = prompted_recall_table_1, margin = 0)(prompted_recall_table_1)



# Subset the data to compare Healthy and Unhealthy product types in the Peer context
data_subset <- subset(prompted_recall, Endorsement =="Peer")
data_subset <- subset(data_subset, Product!="NonFood")
# Refactor Product to remove the unused factor
data_subset$Product <- factor(data_subset$Product)
data_subset$Endorsement <- factor(data_subset$Endorsement)
# Prepare the data
prompted_recall_table_1 <- xtabs(~Product + Recall, data_subset)
# Show the data + Results
CrossTable(prompted_recall_table_1, chisq = TRUE, expected = TRUE, asresid = TRUE, format = "SPSS", digits = 2)

# Subset the data to compare Nonfood and Unhealthy product types in the Peer context
data_subset <- subset(prompted_recall, Endorsement =="Peer")
data_subset <- subset(data_subset, Product!="Healthy")
# Refactor Product to remove the unused factor
data_subset$Product <- factor(data_subset$Product)
data_subset$Endorsement <- factor(data_subset$Endorsement)
# Prepare the data
prompted_recall_table_1 <- xtabs(~Product + Recall, data_subset)
# Show the data + Results
CrossTable(prompted_recall_table_1, chisq = TRUE, expected = TRUE, asresid = TRUE, format = "SPSS", digits = 2)




# Subset the data to compare the Peer verus the non-Peer context in the Unhealthy type
data_subset <- subset(prompted_recall, Product =="Unhealthy")
data_subset$Endorsement <- factor(data_subset$Endorsement, levels=c(levels(data_subset$Endorsement), "Non-Peer"))
data_subset$Endorsement[data_subset$Endorsement!="Peer"] <- "Non-Peer"
# Refactor Product to remove the unused factor
data_subset$Product <- factor(data_subset$Product)
data_subset$Endorsement <- factor(data_subset$Endorsement)
# Prepare the data
prompted_recall_table_1 <- xtabs(~Endorsement + Recall, data_subset)
# Show the data + Results
CrossTable(prompted_recall_table_1, chisq = TRUE, expected = TRUE, asresid = TRUE, format = "SPSS", digits = 2)

# Subset the data to compare the Sponsored verus the non-Sponsored context in the Non-food type
data_subset <- subset(prompted_recall, Product =="NonFood")
data_subset$Endorsement <- factor(data_subset$Endorsement, levels=c(levels(data_subset$Endorsement), "Non-Sponsored"))
data_subset$Endorsement[data_subset$Endorsement!="Sponsored"] <- "Non-Sponsored"
# Refactor Product to remove the unused factor
data_subset$Product <- factor(data_subset$Product)
data_subset$Endorsement <- factor(data_subset$Endorsement)
# Prepare the data
prompted_recall_table_1 <- xtabs(~Endorsement + Recall, data_subset)
# Show the data + Results
CrossTable(prompted_recall_table_1, chisq = TRUE, expected = TRUE, asresid = TRUE, format = "SPSS", digits = 2)


# Subset the data to compare the types within the Peer context
data_subset <- subset(prompted_recall, Endorsement =="Peer")
# Refactor Product to remove the unused factor
data_subset$Product <- factor(data_subset$Product)
data_subset$Endorsement <- factor(data_subset$Endorsement)
# Prepare the data
prompted_recall_table_1 <- xtabs(~Product + Recall, data_subset)
# Show the data + Results
CrossTable(prompted_recall_table_1, chisq = TRUE, expected = TRUE, asresid = TRUE, format = "SPSS", digits = 2)
