quality = read.csv("quality.csv")
str(quality)
table(quality$PoorCare)
#baseline model with most frequent outcme as the outcome
98/131
#need to split data nto training and testing set
install.packages("caTools")
library("caTools")
split = sample.split(quality$PoorCare, SplitRatio = 0.75)
#split will have true false values, with true for training set
qualityTrain = subset(quality, split == TRUE)
qualityTest= subset(quality, split == FALSE)
QualityLog = glm(PoorCare~OfficeVisits+Narcotics,data=qualityTrain, family = binomial)
summary(QualityLog)
predictTrain = predict(QualityLog, type = "response")
summary(predictTrain)
tapply(predictTrain, qualityTrain$PoorCare,mean)
#use classification matric to select threshold value
table(qualityTrain$PoorCare,predictTrain>0.5)
#sensitivity
10/15
#specificity
70/74
table(qualityTrain$PoorCare,predictTrain>0.7)
table(qualityTrain$PoorCare,predictTrain>0.2)
#ROC curves - Receiver operator characteristic
install.packages("ROCR")
library("ROCR")
ROCRpred = prediction(predictTrain,qualityTrain$PoorCare)
ROCRperf = performance(ROCRpred,"tpr","fpr")
plot(ROCRperf, colorize=TRUE)
plot(ROCRperf, colorize=TRUE, print.cutoffs.at = seq(0,1,0.1),text.adj=c(-0.2,1.7))

###Framingham Heart Study
framingham = read.csv("framingham.csv")
str(framingham)
library(caTools)
split = sample.split(framingham$TenYearCHD, SplitRatio = 0.65)
train = subset(framingham, split==TRUE)
test = subset(framingham, split==FALSE)
#use. instead of listing down all varaiables
framinghamLog = glm(TenYearCHD ~ ., data = train, family=binomial)
summary(framinghamLog)
predictTest = predict(framinghamLog, type="response", newdata=test)

# Confusion matrix with threshold of 0.5
table(test$TenYearCHD, predictTest > 0.5)

# Accuracy
(1069+11)/(1069+6+187+11)

# Baseline accuracy
(1069+6)/(1069+6+187+11) 

# Test set AUC 
library(ROCR)
ROCRpred = prediction(predictTest, test$TenYearCHD)
as.numeric(performance(ROCRpred, "auc")@y.values)
