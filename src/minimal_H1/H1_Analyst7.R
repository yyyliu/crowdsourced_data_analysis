# For the complete code transcription, please check this link: 
# https://docs.google.com/document/d/1DWtmVmYy_4gLuvWGLP9QuUcX_Bvth6G8T6uT4JS2Kys/edit

library(dplyr)
library(tidyr)

df <- read.csv('../data/edge1.1_anonymized.csv')

### Hypothesis no 1 ###

# Model the gender of a commenter based on
# female participation in the conversation 
# so far, eg. in all previous comments.

run <- df %>%
  filter(Type == 2 & DebateSize > 1) %>%
  select(ThreadId, Order, Id_num, Female, Male, Discipline) %>%
  arrange(ThreadId, Order) %>%
  group_by(ThreadId) %>% 
  mutate(running_female_count = cumsum(Female) - Female,
         running_female_participation = 
           (cumsum(Female) - Female) / (row_number() - 1)) %>%
  ungroup()

# Original interaction model submitted by the analyst:
# m1 <- glm(Female ~ running_female_count * Discipline,
#           data = run, family = "binomial")
# summary(m1)

m1 <- glm(Female ~ running_female_count + Discipline,
          data = run, family = "binomial")
summary(m1)
