## 1. Write the names of all group members.

#Andrew Celli
#Marcus Meyer

## 2. Read in the Trump Job Approval poll data.
TrumpApproval = read.csv("TrumpApproval.csv")
##    Variables are as follows:
##    - Approve = Proportion of the respondents who approve Trump
approve = TrumpApproval$Approve
##    - 
survey_house = TrumpApproval$survey_house
##    - end_date = Date the survey ended
end_date = TrumpApproval$end_date
##    - sample_subpopulation = Sample type
sample_subpopulation = TrumpApproval$sample_subpopulation
##    - observations = Number of observations
observations = TrumpApproval$observations
##    - mode = Survey method
mode = TrumpApproval$mode

## 3. Plot a histogram of the Trump job approval rates. 
hist(approve)
?plot

## 4. Suppose you only have the "Gallup" poll from "2/19/2017". If we know that
##    the population variance is 0.25, what is your estimate of the sampling 
##    distribution? 
set.seed(1)
gallup = TrumpApproval[TrumpApproval$survey_house=="Gallup"&TrumpApproval$end_date == "2/19/2017",]
John = rnorm(1500, .42, sqrt(.25))
density((rnorm(1500, .42, sqrt(.25)))))
quantiles(density(John), c(.20,.7))

##    Hint: Find this poll using two conditions (survey_house, end_date)



## 5. According to your answer in Q4, what are the 20th and 75th quantiles of 
##    the distribution?
quantiles quantiles(density((rnorm(1500, .42, sqrt(.25)))), c(.20,70), type =1)
quantile(John, c(0.20, 0.75), type = 1)



## 6. Suppose a new poll suggests that the Trump approval rate is 47%. 
##    According to your answer in Q4, what is the probability of a poll showing 
##    support for Trump higher than this?



##################### OPTIONAL #####################

## We would like to know the long-term trends in Trum approval from
## Gallup, SurveyMonkey, and YouGov/Economist.
## Using "Approve" and "end_date", create a line plot that summarizes overtime 
## changes in Trump approval rates by survey company (draw three separate lines 
## for the three companies). Which one is most supportive of Trump?

# First, you have to run the following line (change the name of the data):
polls$end_date <- as.Date(polls$end_date, "%m/%d/%Y")

