# ISLR sixth print ISBN: 978-1-4614-7138-7
# Chapter 3 Linear Regression, Exercises 8 & 10
library(ISLR)
attach(Auto)
library(MASS)

#8A
View(Auto)
lm.fit=lm(mpg~horsepower, data=Auto)
summary(lm.fit)
#(i)There is a relationship btw. horsepower (-.158 B1) and mpg 

#(ii) strong relationship between horsepower and mpg (R^2=.6 >.5), 

#(iii)There is a negative relationship between horsepower and mpg

#(iv) predicted mpg= -.158(98hp) + 39.94 = 24.456 mpg 
predict(lm.fit, data.frame(horsepower=c(98)),
        interval="confidence")
predict(lm.fit, data.frame(horsepower=c(98)),
        interval="prediction")
# 95% Confidence Interval of (23.97, 24.96)
# 95% Prediction Interval (14.81, 34.12)

#B. plot the response and predictor use abline() to show lsr line
plot(horsepower, mpg)
abline(lm.fit, lwd=2, col="red")

#C. use plot() to produce diagnostic plots of the lsr fit. Comment on issues.
# The residuals get larger in magnitude the higher the mpg
par(mfrow=c(1,2))
plot(predict(lm.fit), residuals(lm.fit))
plot(predict(lm.fit), rstudent(lm.fit))

#10
attach(Carseats)
View(Carseats)
#(a) Multiple regression, predict Sales, use Price, Urban, US
carseats_lm1 = lm(Sales~Price+Urban+US, data=Carseats)
summary(carseats_lm1)
#(b) Interpret Each coefficient in model
# Intercept of 13,040 Sales
# -54 sales for every dollar charged for carseat
# -20 sales if urban store
# 1200 more sales if US store

#(c) Write the model in Equation Form
#Y^ = 13.04-.054(Price)-.022(Urban 1 or 0)+1.2(US 1 or 0)

#(d) For which predictor can you reject null Hypothesis? 
# Intercept, Price and US are all significant, urban not as p-value > .05

#(e) Fit a smaller model only using good predictors
carseats_lm2 = lm(Sales~Price+US, data=Carseats)
summary(carseats_lm2)

#(f) How well do models (a) and (e) fit the data?
# Adjusted R^2 of .2335 for a and .2354 for e.
# F-stat of 41.52 for a and 62.43 for e. e better fit

#(g) Using the model for (e) give 95% conf. int. for coefficients
# 95% confint of (11.79, 14.27)Bo, (-.06,-.04)B1, (.69, 1.7)B2.
confint(carseats_lm2)

#(h) Is there evidence of outliers, high leverage observations in e?
par(mfrow=c(2,2))
plot(carseats_lm2)
#There is evidence of one observation with high leverage in e.
