# For the complete code transcription, please check this link:
# https://docs.google.com/document/d/1-eJCRFQk_UKChWZAAUw4PR4ljS1PSaqVfWv7Q_Gc4vc/edit

library(tidyverse)
library(magrittr)
library(forcats)

edge <- read_csv('../data/edge1.1_anonymized.csv')

###############################################################################
# Hypothesis 1: “A woman’s tendency to participate actively in the conversation 
# correlates positively with the number of females in the discussion.”
###############################################################################

# create a thread-level data frame (1 row/thread)
threads <- edge %>% 
  select(
    ThreadId,
    Type,
    DebateSize,
    Female_Contributions,
    Male_Contributions,
    UniqueContributors,
    UniqueFemaleContributors,
    UniqueMaleContributors,
    Live
  ) %>% 
  distinct

# *restrictions?*
# -- limit the conversations to have at least 2 participants
# -- ensure that there is at least one female present in each thread
# this ensures we can at least measure a response
threads <- threads %>% 
  filter(
    UniqueContributors > 1,
    UniqueFemaleContributors >= 1
  )

# if the thread is ever marked as live, set as 1, otherwise leave it as-is
# then, unique the data
threads <- threads %>% 
  group_by(ThreadId) %>% 
  mutate(
    Live_ever = as.numeric(ifelse(any(Live == 1), 1, Live)),
    live_and_not_live = as.numeric(n() > 1)
  ) %>% 
  select(-Live) %>% 
  ungroup %>% 
  distinct

# if Live_ever == 1, that means there's at least some aspect of the thread that
# was transcribed.
# live_and_not_live indicates the thread has a mix of transcribed and written 
# content

threads$prop_f_contributors <- with(threads, UniqueFemaleContributors / UniqueContributors)
threads$debsize_z <- with(threads, (DebateSize - mean(DebateSize)) / sd(DebateSize))

thread_fit_poisson <- glm(
  Female_Contributions ~
    Live_ever + 
    debsize_z +
    prop_f_contributors,
  data = threads,
  family = poisson
)

library(broom)

# get the incident rate ratios and their confidence intervals
irr <- thread_fit_poisson %>% 
  tidy %>% 
  bind_cols(confint_tidy(thread_fit_poisson)) %>% 
  select(term, estimate, contains("conf")) %>% 
  mutate_each(funs(exp), -term) %>% 
  mutate_each(funs(round(., 4)), -term)

# print
irr
