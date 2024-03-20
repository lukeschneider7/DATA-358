# Sand Grain size on 28 beaches in Japan w/ presence or
# absence of wolf spider Lycosa Ishikariana on each beach
library(readr)
library(ggplot2)
Beach_data <- read_table("Downloads/DATA 358 Machine Learning/spiders.txt")
Beach_data = data.frame(Beach_data)

i = Spiders=="present"
j = Spiders=='absent'
Spiders[i]=1
Spiders[j]=0
Beach_data$Spiders = Spiders
Beach_data

                                  
# a. Run a log Regression using Grain Size as your X and
# the logit of "present" using 50% of data as training set
#Split data for Training
library(caTools)
set.seed(123)
split = sample.split(Spiders, SplitRatio = .5)
train_set = subset(Beach_data, split == TRUE)
train_set$Spiders = as.numeric(train_set$Spiders)
test_set = subset(Beach_data, split == FALSE)

#Log Regression
glm_spider_present=glm(Spiders~Grain.size, data=train_set, binomial)
summary(glm_spider_present)

# b. Predict the "present" status for the remaining test data
glm_prob = predict(glm_spider_present, test_set, type = "response")

# c. Find the Confusion-Table
glm.pred=rep("0", 14)
glm.pred[glm_prob>.5]="1"
table(glm.pred, test_set$Spiders)

# d. Use confusion table to estimate total error, sensitivity, specificity
# total Error = 4/14 = 29%
# Sensitivity = 9/9 = 100%
# Specificity = 1/5 = 20%

# e. Estimate p that beach w .72 mm grain size wil have spider present
logit = -2.209 + (7.471*.72)
p = exp(logit)/(1+exp(logit))

# f. Create plot showing estimated p of spider habitation 
# for beaches with grain sizes ranging form .2 to 1.1
x=Grain.size
curve(exp(7.471*x-2.209)/(1+exp(7.471*x-2.209)), .2, 1.1, xlab="Grain Size", ylab="pred Spider Present probability")

# g. Describe the relationship between grain size and p of spider "present"
# There is a positive non-linear relationship between grain size and p
# of Spider "present" with a decreasing slope, resembling a stress-strain curve

# h. repeat a-e using 75% of the data, comment on error rate changes
split2 = sample.split(Spiders, SplitRatio = .75)
train_set2 = subset(Beach_data, split2 == TRUE)
train_set2$Spiders = as.numeric(train_set2$Spiders)
test_set2 = subset(Beach_data, split2 == FALSE)

glm_spider_present2=glm(Spiders~Grain.size, data=train_set2, binomial)
summary(glm_spider_present2)

glm_prob2 = predict(glm_spider_present2, test_set2, type = "response")

glm.pred2=rep("0", 7)
glm.pred2[glm_prob2>.5]="1"
table(glm.pred2, test_set2$Spiders)

# total Error = 2/7 = 29%
# Sensitivity = 5/5 = 100%
# Specificity = 0/2 = 0%

logit = -1.315 + (4.357*.72)
p = exp(logit)/(1+exp(logit))


