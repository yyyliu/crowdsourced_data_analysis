# For the complete code transcription, please check this link:
# https://docs.google.com/document/d/1dN_BpYBDH_TbckBnjXNqosTiCsiJQFftRj9VoMS_rTM/edit

## load packages
rm(list=ls())
library(tidyverse)
library(magrittr)
library(caret)
# library(GGally)
library(lme4)

## read data
#d <- read_csv("~/Documents/work/CDA-2/Boba/MinimalCodes/Minimal-Code-Sample/edge1.1_anonymized.csv")
d <- read_csv('../data/edge1.1_anonymized.csv')

## Test Hypothesis 1: “A woman’s tendency to participate actively 
## in the conversation correlates positively with the number of 
## females in the discussion.”

## subset to just keep the conversation level data
d.conversation <- d %>% distinct(ThreadId, .keep_all = T) 

## create training, query, and test data sets to facilitate exploration
## to do this I need to select an outcome variable on which to split
## here, I use the percentage of female participation in that thread

set.seed(123)
trainIndex <- createDataPartition(d.conversation$FemaleParticipation, p = .6, 
                                  list = FALSE, 
                                  times = 1)

d.train <- d.conversation[ trainIndex,]
d.test  <- d.conversation[-trainIndex,]

# fit linear model on training data
m1 <- d.train %>% 
  filter(UniqueFemaleContributors >= 1 & UniqueContributors > 1) %>%
  lm(FemaleParticipation ~ UniqueFemaleContributors, data = .)

# fit linear model to test data
# m1.test <- d.test %>% 
m1.test <- d.conversation %>%
  filter(UniqueFemaleContributors >= 1 & UniqueContributors > 1) %>%
  lm(FemaleParticipation ~ UniqueFemaleContributors, data = .)

summary(m1.test) # model reported by the analyst
