#PROBLEM 1 ---------
FOs = matrix( data = c(14,6,7,7,7,1), nrow = 2, byrow = TRUE)
dimnames(prob1data) <- list(Class=c("Upper", "Lower"),
                           Treatment=c("Stopped", "Not Stopped", "Bribe Requested"))
prob1data
FEs = 
  matrix(data = c((27/42)*21 , (27/42)*13, (27/42)*8,
                  (15/42)*21, (15/42)*13, (15/42)*8), nrow = 2)
X2 = sum((((FOs-FEs)^2)/FEs))
chisq.test(prob1data)
chisq.test(FOs)
pchisq(X2, df = 2, lower.tail = FALSE)
rowprop = matrix(data = c(27/42, 27/42, 27/42, 15/42, 15/42, 15/42), nrow =2, byrow = TRUE)
colprop = matrix(data = c(21/42, 13/42, 8/42, 21/42, 13/42, 8/42), nrow =2, byrow = TRUE)
stdResiduals = matrix(
  data = c((FOs - FEs)/(sqrt(FEs*(1-rowprop)*(1-colprop))))
  , byrow = TRUE, nrow = 2)
stdResiduals
