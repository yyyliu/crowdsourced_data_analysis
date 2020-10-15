# For the complete code transcription, please check this link:
# https://docs.google.com/document/d/1_Jiew5-9F8s8utFChQjI3iH2Q8-dofWOHWMuftc4L1M/edit?usp=sharing
# Note: cannot reproduce the reported regression coefficient

# Second Hypthesis: Higher status participants are more verbose than lower status participants.
#load necessary packages
library(dplyr)

#read in data set
edge <- read.csv("../data/edge1.1_anonymized.csv")

# remove NA values
edge <- edge[complete.cases(edge), ]

# select columns related to 2nd Hypothesis
edge.2hyp <- edge %>%
  select(ContributionsThisYear, Discipline, Years_from_PhD, PhD_Institution_SR_Bin,
         PhD_Institution_US_IR_Bin, PhD_Institution_US_Bin, ThreadsThisYear,
         Job_Title_S, HavePhD)
  
# let's get summary statistics of Threads This Year vs Job Title
edge.2hyp.fit <- lm(ThreadsThisYear ~ Job_Title_S, data = edge.2hyp)
summary(edge.2hyp.fit)
