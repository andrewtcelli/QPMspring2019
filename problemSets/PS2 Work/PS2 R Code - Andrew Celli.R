#PS 2 - Andrew Celli
#Problem 4
set.seed(12345)
ab = rnorm(100000, mean = 50, sd = 6)
plot(density(a))
pnorm(57.75, mean = 50, sd = 6) #a
1-pnorm(50.45, mean = 50, sd = 6) #b
pnorm(59.4, mean = 50, sd = 6) - pnorm(52.4, mean = 50, sd = 6) #c

ddd = c(35,38,38,90)
summary(ddd)
boxplot(ddd)
sd(ddd)

#Problem 5
set.seed(12345)
salaries <- rnorm(n=10000,mean=40000,sd=15000)
plot(density(salaries), main = "Salaries Distribution")

#Problem 6
a = density(rnorm(100000, mean = 0, sd = .04^.5))
b = density(rnorm(100000, mean = 0, sd = 3^.5))
c = density(rnorm(100000, mean = 3, sd = 3^.5))
d = density(rnorm(100000, mean = 3, sd = .4^.5))
e = density(rnorm(100000, mean = -2, sd = .4^.5))
f = density(rnorm(100000, mean = -2, sd = .25^.5))
#par(mfrow=c(2,3)) 
plot(c, main = "Problem 6 Graph", ylim=c(0, 2), col = "red")
lines(a, main = "a", col = "navy")
lines(b, main = "b", col = "yellow")
lines(d, main = "d", col = "blue")
lines(e, main = "e", col = "black")
lines(f, main = "f", col = "green")
legend("topright", inset=0, c("A - navy","B - yellow","C - red", "D - blue", "E - black", "F - green" ))

#Problem 7
dir()
drugCov = read.csv("drugCoverage.csv")
#a
hist(drugCov$drugsmedia, breaks = 196, main = "New's Coverage of Drug")
#b
boxplot(drugCov$approval, main = "Approval Rating")
boxplot(drugCov$drugsmedia, main = "Drugs Coverage")
#c
plot(y=drugCov$drugsmedia, x =drugCov$unemploy, xlab = "unemployment rate",ylab="media coverage" )
plot(y=drugCov$drugsmedia, x = drugCov$approval, xlab = "approval rating",ylab="media coverage" )
#d
plot(x = drugCov$Year, y = drugCov$drugsmedia,cex.axis=.59, main = "Drug Media",  type = "l")
lines(drugCov$drugsmedia, col = "black")

plot(x = drugCov$Year, y = drugCov$approval, cex.axis=.59, main = "Approval Rating", type = "l")
lines(drugCov$approval, col = "black")

#Problem 8
wnom = read.csv("wnominatehouse.csv")
#A
dem88 = wnom$wnominate[wnom$congress == 88 & wnom$party == 100]
dem107 = wnom$wnominate[wnom$congress == 107 & wnom$party == 100]
rep88 = wnom$wnominate[wnom$congress == 88 & wnom$party == 200]
rep107 = wnom$wnominate[wnom$congress == 107 & wnom$party == 200]
#B
median(dem88)
median(dem107)
#C
median(rep88)
median(rep107)
#d
sd(dem88)
sd(dem107)
#e
sd(rep88)
sd(rep107)
#f
hist(rep88, col = "red", xlab = "Ideology", main = "W-Nominate of Democrats and Republicans in 88th Congress", xlim = c(-1,1))
hist(dem88, add = TRUE, col = "blue")
legend("topright", inset=0, cex=.5, c("Democrats - Blue, Republicans - Red" ))
#g
hist(rep107, col = "red",xlab = "Ideology", main = "W-Nominate of Democrats and Republicans in 107th Congress", xlim = c(-1,1))
hist(dem107, add = TRUE, col = "blue")
legend("topright", inset=0, cex=.5, c("Democrats - Blue, Republicans - Red" ))

