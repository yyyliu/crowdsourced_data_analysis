# For the complete code transcription, please check this link:
# https://docs.google.com/document/d/1dN_BpYBDH_TbckBnjXNqosTiCsiJQFftRj9VoMS_rTM/edit
# Note: cannot reproduce, possibly due to random data partition

## load packages
library(tidyverse)
library(caret)
library(lme4)

## read data
d <- read_csv("../data/edge1.1_anonymized.csv")

## The relevant unit of analysis for this hypothesis is speakers not
## conversations.

# split into training and test data sets based on
# the contributions this year variable
set.seed(456)
trainIndex_2 <- createDataPartition(d$ContributionsThisYear, p = .6, 
                                  list = FALSE, 
                                  times = 1)

d.train_2 <- d[ trainIndex_2,]
d.test_2  <- d[-trainIndex_2,]

# fit to test data
m3.lmer.test <- lmer(log(Number.Characters) ~ Workplace_US_Bin + Workplace_SR_Bin + log(Total_Citations) + 
                  (1|Id) + (1|ThreadId),
                data = d.test_2)

summary(m3.lmer.test) 
