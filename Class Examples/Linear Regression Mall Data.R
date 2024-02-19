library(readxl)

View(Mall)
head(Mall)
Mall=data.frame(Mall) 
names(Mall)
attach(Mall)
#Line 7 and 9 do the same thing
Number=Mall$Number
lm1=lm(Return~Number+Down+Mall,data=Mall)

#Intercept counts as a parameter
#Residual is exact - predicted
#Relationship between y and x must be linear
#Y value in linear regression and residuals must be normal
#Constant Variance
summary(lm1)


# Producing the graph in slide 24-the three  regression lines  
#Mall:  Mall=1, Down =0 : Return=15.52+27.75+.87*Number
#Down:Mall=0, Down =1:Return=15.52+6.67+.87*Number
# Sub : Mall=0, Down =0:Return=15.52+.87*Number

Return1=15.52+27.75+.87*Number
Retun2=15.52+6.67+.87*Number
Retur3=15.52+.87*Number


# Check: 
Number=seq(0,50, by=2)

Return1=15.52+27.75+.87*Number
Return2=15.52+6.67+.87*Number
Return3=15.52+.87*Number
plot(Number,Return1, ylim=c(0,100),type="l", col="red", xlab="Number of Households in 1000", ylab="Annual return")
lines(Number, Return2, col="green", lty=1)
lines(Number, Return3, col="blue", lty=1)
title("Grahical rep of Regression")
legend(40,20,legend=c("Mall","Down" ,"Sub"), col=c("Red","Green", "Blue"))

