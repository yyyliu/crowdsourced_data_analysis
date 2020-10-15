# For the complete code transcription, please check this link:
# https://docs.google.com/document/d/14QYMLwqs8eWzX1jevFhwugkglYvRRGLiLU8aTQoUIT4/edit

rm(list = ls())
library(tidyverse)
library(car)

#Load data.
dat <-read_csv("../data/edge1.1_anonymized.csv")

status <- dat %>% 
  filter(Limited_Information == 0) %>%
  dplyr::select(Title, Id, Role, Female, Male, Academic, Discipline,
                HavePhD, Years_from_PhD, PhD_Institution_SR_Bin, Workplace_SR_Bin,
                PhD_Institution_US_IR_Bin, Workplace_US_IR_Bin, Total_Citations, H_Index, i10_Index,
                Citations_Year, Citations_Cumulative, AcademicHierarchyStrict)

#Select a subset of predictors to reduce collinearity.
status <- status %>%
  dplyr::select(Title, Id, 
                H_Index, 
                AcademicHierarchyStrict,
                Workplace_US_IR_Bin,
                PhD_Institution_US_IR_Bin,
                HavePhD)

wpt_dat <- dat %>%
  group_by(ThreadId, Id) %>%
  summarise(total_wpt = sum(WC)) %>%
  group_by(Id) %>%
  summarise(mean_wpt = sum(total_wpt)/n())

reg_dat <- left_join(wpt_dat, status, by = "Id") %>%
  group_by(Id) %>%
  slice(1)
reg_dat <- reg_dat %>% ungroup() %>% select(-Title, -Id)

#Try subsetting to the group with PhDs anyway:
phds <- reg_dat %>% filter(HavePhD == 1) %>% select(-HavePhD)

#VIF looks okay. Point 155 definitely needs to go, though!
phds1 <- phds[-155,]

#What if we regress against another status metric by itself?
fit6 <- lm(mean_wpt ~ AcademicHierarchyStrict, data = phds1)
summary(fit6)
