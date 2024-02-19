library(readr)
Fish_data_from_Kaggle <- read_csv("Downloads/DATA 358 Machine Learning/Fish data from Kaggle.csv")
View(Fish_data_from_Kaggle)

#Make the data as data frame
Fish = data.frame(Fish_data_from_Kaggle)
#import some libraries
library(dplyr)
library(ggplot2)
library(caTools)
library(corrgram)

hist(Fish[,3])

# Now Check for missing values
any(is.na(Fish))

#Plotting Height vs Weight, data must be df
ggplot(data=Fish, aes(x=Height, y=Weight)) +
  geom_point(aes(color=Species),size=3)

# Correlation Matrix is symmetric
cor(Fish[,2:7])
corrgram(Fish, lower.panel=panel.shade, upper.panel=panel.cor)

#Most correlated variable with Weight


#Build the model by selecting the training and test sample
# Random start
set.seed(42)

sample <- sample.split(Y=Fish$Weight, SplitRatio = .7)
trainSet = subset(x=Fish, sample==TRUE)
testSet = subset(x=Fish, sample==FALSE)

# Full Model-Multiple Regression (~. means against all other variables)
model1= lm(formula=Weight ~ ., data=trainSet)
summary(model1)

# Simple Model - One predictor (if P<.05 it is significant, smaller P inc significance)
model2 = lm(Fish$Weight ~ Fish$Height, data=trainSet)
#weight_hat = -144.38 + 60.49Height_hat
#Residual = Exact - Predicted
summary(model2)
trainSet[1,]

#  Two Variable model
model3 = lm(Fish$Weight ~ Fish$Height+Fish$Length1, data=trainSet)
# If R^2 is >50% it is a good fit
summary(model3)

names(Fish)

modelResiduals = as.data.frame(residuals(model2))
residuals(model2)
predict(model2)
ggplot(modelResiduals, aes(residuals(model2))) + geom_histogram(fill="deepskyblue", color="red")


#Model Evaluations
preds = predict(model2, testSet)
head(testSet)
modelEval = cbind(testSet$Weight, preds)

#Residual
mse = mean(modelEval$Actual ~ modelEval$Predicted)^2
rmse = sqrt(mse)
