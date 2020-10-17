# For the complete code transcription, please check this link:
# https://docs.google.com/document/d/14QYMLwqs8eWzX1jevFhwugkglYvRRGLiLU8aTQoUIT4/edit

rm(list = ls())
library(tidyverse)
library(car)

#Load data.
df <-read_csv("../data/edge1.1_anonymized.csv")

status <- df %>% 
  filter(
    Limited_Information == 0,
    HavePhD == 1, 
    !(ThreadId == 342 & Id == 283) # remove a specific outlier
  )

wpt_dat <- df %>%
  group_by(ThreadId, Id) %>%
  summarise(total_wpt = sum(WC)) %>%
  group_by(Id) %>%
  summarise(mean_wpt = sum(total_wpt)/n())

reg_dat <- left_join(status, wpt_dat, by = "Id") %>%
  group_by(Id) %>%
  slice(1) %>%
  ungroup()

#What if we regress against another status metric by itself?
fit6 <- lm(mean_wpt ~ AcademicHierarchyStrict, data = reg_dat)
summary(fit6)
