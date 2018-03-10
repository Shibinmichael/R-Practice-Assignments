baseball=read.csv("baseball.csv")
str(baseball)
moneyball = subset(baseball, Year < 2002)
str(moneyball)
moneyball$RD = moneyball$RS - moneyball$RA
plot(moneyball$RD,moneyball$W)
#strong corr between RD and W -model 
WinsReg=lm(W~RD,data = moneyball)
summary(WinsReg)
#model with other varaiables
RunsReg = lm(RS~OBP+SLG+BA, data = moneyball)
summary(RunsReg)
#due to multicollinearity remove BA
RunsReg = lm(RS~OBP+SLG, data = moneyball)
summary(RunsReg)

