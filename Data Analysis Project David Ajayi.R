install.packages("faraway")

library(faraway)
data(debt)
View(debt)
str(debt)

#Changing numerical variables into factors
debt$incomegp<- as.factor(debt$incomegp)
debt$singpar<- as.factor(debt$singpar)
debt$house<- as.factor(debt$house)
debt$agegp<- as.factor(debt$agegp)
debt$bankacc<- as.factor(debt$bankacc)
debt$bsocacc<- as.factor(debt$bsocacc)
debt$cigbuy<- as.factor(debt$cigbuy)
debt$xmasbuy<- as.factor(debt$xmasbuy)

str(debt)

#1.3 Creating a cleaned version of dataset
david.project <- na.omit(debt)
View(david.project)

str(david.project)

#Using GGally plotting function to create a summary of all of the varibales
library(GGally)

ggpairs(data=david.project, columns= c("bankacc","bsocacc","cigbuy","xmasbuy","house","incomegp","children", "agegp","locintrn","prodebt","singpar","manage","ccarduse"))#to get the matrix of bi-variate
ggpairs(david.project, columns= c("incomegp","children", "agegp","locintrn","prodebt","singpar","manage","ccarduse"))                           
                                       


#analysis
mdl1 <- lm(prodebt ~ locintrn, data=debt)
print(summary(mdl1))
mdl2 <- lm(prodebt ~ locintrn+ manage, data=debt)
print(summary(mdl2))
mdl3 <- lm(prodebt ~ locintrn+ manage+children, data=debt)
print(summary(mdl3))
mdl4 <- lm(prodebt ~ locintrn+ manage+children+singpar, data=debt)
print(summary(mdl4))
mdl5 <- lm(prodebt ~ locintrn+ manage+children+singpar+bankacc, data=debt)
print(summary(mdl5))



#using anova function in comparing the models
print(anova(mdl1,mdl2,mdl3,mdl4,mdl5))