library(mosaic)
library(dplyr)
library(readr)

#Exercise 1
# Assign Y the exponential of x, copmute natural log of x
x=.7
y=exp(x)
x_ln=log(x, 2.73)

# Write sequence 1 to 100, steps of. Call W and find mean of W*X
W=seq(1, 100, 2)
Z=W*x
Z_mean = mean(Z)

# Find the SD of Z.
Z_sd = sd(Z)


#Exercise 2(Fecundity of Crickets)
`crickets.(2)` <- read.delim("~/Downloads/DATA 358 Machine Learning/crickets (2).txt")
View(`crickets.(2)`)

# Make a Histogram and boxplot of the Eggs variable of the cricket data
crickets = data.frame(`crickets.(2)`)
cricket_data = as.matrix(crickets)
eggs = cricket_data[,1]
hist(eggs)
title("Histogram of # Eggs")
boxplot(eggs)
title("Boxplot of # Eggs")

# Compute the sample mean, and sample variance of the eggs variable
favstats(eggs)


#Exercise 3 Malaria
malaria <- read_csv("Downloads/DATA 358 Machine Learning/malaria.txt")
View(malaria)

#2. Make a histogram of the vector parasites
# Distribution does not look symmetric - Looks Right skewed
parasites = as.matrix(malaria)
boxplot(parasites)
title("Boxplot of malaria parasites")
hist(parasites)
title("Histogram of Malaria Parasites")

#3-4. Make new vector y which has log to parasites counts as values
# Distribution looks symmetric with y instead
y = log(parasites, 2.73)
hist(y)

#5.Sample mean and standard deviation of Y
favstats(y)

#6.Mean +- 2SD?
# This is 95% confidence interval: 7.99 +- 3.70

#7.logarithmic back to parasite counts for 95% confidence interval
num_parasites = 2.73^y
confidence_interval = t.test(num_parasites)
print(confidence_interval)
