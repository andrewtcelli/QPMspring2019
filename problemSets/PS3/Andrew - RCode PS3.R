#Andrew Celli - PS3

# PROBLEM 1 -------------------------------------------------- 
install.packages("faraway")
library("faraway")
data("newhamp")
NewHampH = newhamp[newhamp$votesys == "H",]
NewHampD = newhamp[newhamp$votesys == "D",]
plot(NewHampH$pObama, NewHampH$Dean, col = "blue", main = "Proportion of votes in hand-counted counties",ylab = "DEAN",xlab = "OBAMA")
points(NewHampD$pObama, NewHampD$Dean, col="red", main = "Proportion of votes in machine-counted counties",ylab = "DEAN", xlab = "OBAMA")
legend("topright", inset=0, c("Hand-counted - Blue", "Machine-counted - Red"), cex=.65)
# PROBLEM 2 -------------------------------------------------- 
set.seed(20)
par(mfrow=c(1,1)) 
x = seq(-4,4,.1)
dnormx = dnorm(x)
dev.off
dt1 = dt(x, 1)
dt3 = dt(x, 3)
dt20 = dt(x, 20)
plot(x, col = "white", xlab="x value", cex = 1, ylim = c(0,.38), xlim = c(-4,4))
lines(x, dnormx, col = "black")
lines(x, dt1, col = "blue")
lines(x, dt3, col = "green")
lines(x, dt20, col = "red")
title(main = "t Distribution with varying Degrees of Freedom (df)", xlab = "Occurence value; N = 100")
legend("topright", inset=0, c("normal - black", "df of 1 - blue","df of 3 - green","df of 20 - red"), cex=.95)
# PROBLEM 3 -------------------------------------------------- 
dir()
setwd("Desktop")
voteincome = read.csv("voteincome.csv")
age = as.numeric(unlist(voteincome["age"]))#learned from StackOverflow
MeanAge = mean(age)
MeanAge
sd(age)
StandardErrorAge = sd(age)/(sqrt(1500)) #EQUALS 0.4511027
ZScoreAge = (50-MeanAge)/StandardErrorAge #EQUALS 1.637469
pnorm(ZScoreAge, lower.tail = FALSE)*2
NinetyFivePCTInterval = c(MeanAge-qnorm(.975)*StandardErrorAge, MeanAge+qnorm(.975)*StandardErrorAge) #48.37717 ; 50.14549
NinetyFivePCTInterval
# PROBLEM 4 -------------------------------------------------- 
SELib = 1.2/sqrt(16)
TscoreLib = (9.5-10)/SELib #EQUALS -1.666667
pt(-1.666667, df = 15, lower.tail = TRUE)
pnorm(-1.666667, lower.tail = TRUE)#0.04779032
# PROBLEM 5 -------------------------------------------------- 
estimateProportion = 341/(341+357) #EQUALS 0.4885387
SDofProp = sqrt((estimateProportion*(1-estimateProportion))/(341+357))#EQUALS 0.01892031
c(estimateProportion-1.96*SDofProp, estimateProportion+1.96*SDofProp) #EQUALS (0.4514549, 0.5256225)
# PROBLEM 7 -------------------------------------------------- 
TVSe = sqrt(0.070^2+.075^2) #EQUALS 0.1025914
TvZ = (2.99-2.86)/TVSe #EQUALS 1.2671
pnorm(-TvZ)*2 #0.2050972
pnorm(0, mean = 2.99, sd = 2.34)#EQUALS .10, so more than the .025
# PROBLEM 8 -------------------------------------------------- 
SDSmallSample = sqrt(
  (((11-1)*2.34^2)+((16-1)*2.22^2))
  /
  (11+16-2)
  )
SESmallSample = SDSmallSample * sqrt(((1/11)+(1/16)))
TSmall = (2.99-2.86)/SESmallSample #EQUALS .14
pt(-TSmall, df = 25)*2 #0.8848619


