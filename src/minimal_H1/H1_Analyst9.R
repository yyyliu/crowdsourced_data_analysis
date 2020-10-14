# For the complete code transcription, please check this link: 
# https://docs.google.com/document/d/1r9cxc5uzAj3Ya0-BphlhTiC6_3p6UzfWi2Ntfg0l5h4/edit

# --------------------------------
# Loading, setting up
# --------------------------------

library(tidyverse)
library(corrr) #added by reviewer

df <- read_csv('../data/edge1.1_anonymized.csv')

# --------------------------------
# Analysis for RQ #1
# --------------------------------

to_model <- df %>% 
  group_by(ThreadId) %>% 
  select(Female, Order, WC, ThreadId) %>%
  mutate(Female_mp = cummean(Female)) %>% 
  filter(Female == 1)

m1 <- lm(WC ~ I(Female_mp * 100), data = to_model)
summary(m1)
