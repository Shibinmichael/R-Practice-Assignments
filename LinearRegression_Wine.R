wine = read.csv("wine.csv")
str(wine)
summary(wine)
#linear model with one variable
model1 = lm(Price ~ AGST, data = wine)
summary(model1)
#Calculate SSE
model1$residuals
SSE = sum(model1$residuals^2)
SSE
#model 2 with two variables
model2=lm(Price ~ AGST+HarvestRain, data = wine)
summary(model2)
SSE = sum(model2$residuals^2)
SSE
#model with all varaiables
model3=lm(Price~AGST+HarvestRain+WinterRain+FrancePop+Age, data=wine)
summary(model3)
SSE=sum(model3$residuals^2)
SSE
#model4 without francepop
model4=lm(Price~AGST+HarvestRain+WinterRain+Age, data=wine)
summary(model4)
#check for multicollinearity
cor(wine$WinterRain, wine$Price)
cor(wine$Age,wine$FrancePop)
cor(wine)
#Test model4 with testing data set
wineTest=read.csv("wine_test.csv")
str(wineTest)
predictTest=predict(model4, newdata = wineTest)
predictTest
SSE = sum((wineTest$Price-predictTest)^2)
SST = sum((wineTest$Price-mean(wine$Price))^2)
1-SSE/SST
