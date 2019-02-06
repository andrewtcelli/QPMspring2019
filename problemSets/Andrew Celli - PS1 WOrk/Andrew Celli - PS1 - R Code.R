#Andrew Celli - Problem Set 1 Work



################################################################################
#PROBLEM 4
menAge = c(56, 60, 50, 26, 45, 35, 41, 43, 34, 42, 37, 39, 33, 28, 52, 48, 27, 20, 44, 32)
womenAge = c(47, 49, 20, 46, 43, 44, 45, 60, 57, 28, 55, 27, 25, 50, 52, 48, 23, 42, 33, 59)
meanMen = mean(menAge)
meanWomen = mean(womenAge)
#Plot Men Age
boxplot(sort(menAge), ylab = "Age", xlab = "All Men")
#Plot Women Age
boxplot(sort(womenAge), ylab = "Age", xlab = "All Women")


################################################################################
#PROBLEM 6
probPlaySports = .45
probPlayInstrument = .34
probAttendCollege = .69
#a
noPlaySports = 1-probPlaySports
noPlaySports
#b
playsStuff = probPlaySports*probPlayInstrument
playsStuff
#c
collegeAthlete = probPlaySports*probAttendCollege
collegeAthlete
