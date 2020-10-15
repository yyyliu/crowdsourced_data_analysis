# For the complete code transcription, please check this link:
# https://docs.google.com/document/d/1UMx-rdRhSKv3aNxn9wdT2EBqWdLq3Ts5mbomH8pBitM/edit

library(readr)
library(dplyr)
library(lme4)

# data loading and exploration
dat <- read_csv("../data/edge1.1_anonymized.csv")

# filter for commentators
dat2 <- dat %>% 
  filter(Role == 2) 

# Don't take the mean of AcademicHierarchy 
dat_sum <- dat2 %>% 
  mutate(AllContributions = PreviousContributions + ContributionsThisYear) %>% 
  group_by(Id, AcademicHierarchyStrict, Female) %>% 
  # Academic Status might change, so taking the mean. This is not ideal though.
  summarise(
            comment_length = mean(Number.Characters),
            number_comments = mean(AllContributions))

# we want academic hierarchy as a predictor.
# code ordinal variable as numeric, as Andrew Gelman suggests here: http://andrewgelman.com/2009/10/06/coding_ordinal/
dat_sum <- dat_sum %>% mutate(AH = as.numeric(as.character(AcademicHierarchyStrict)))

# simple linear model
# comment_length
mod1 <- lm(comment_length ~ AH + as.factor(Female), data = dat_sum)
summary(mod1) 
