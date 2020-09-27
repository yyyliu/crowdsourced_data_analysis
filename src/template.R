# --- (BOBA_CONFIG)
{
  "decisions": [
    {"var": "filter", "options": [
      "Female == 1",
      "UniqueFemaleContributors >= 1 & DebateSize > 1",
      "Type == 2"
    ]},
    {"var": "DV", "options": [
      "ContributionsbyAuthor",
      "FemaleParticipation",
      "WC",
      "CommentsChange"
    ]},
    {"var": "IV", "options": [
      "UniqueFemaleContributors",
      "FemaleCumulativeProportion"
    ]},
    {"var": "covariates", "options": [
      "+ PreviousContributions + HavePhD + Total_Citations",
      "+ UniqueContributors",
      ""
    ]}
  ],
  "constraints": [
    {"variable": "DV", "option": "ContributionsbyAuthor", 
      "condition": "filter.index == 0 and Unit != thread"},
    {"variable": "DV", "option": "FemaleParticipation",
      "condition": "Unit != comment"},
    {"variable": "DV", "option": "CommentsChange", 
      "condition": "filter.index == 0"},
    {"variable": "DV", "option": "WC", 
      "condition": "filter.index == 0"},
    {"variable": "IV", "option": "FemaleCumulativeProportion",
      "condition": "Unit == comment"},
    {"variable": "covariates", "index": 0,
      "condition": "Unit != thread"}
  ],
  "before_execute": "rm -rf results && mkdir results"
}
# --- (END)

library(readr)
library(tidyverse)
library(broom.mixed)
source('../../boba_util.R')

df <- read.csv(file='../../../data/edge1.1_anonymized.csv')

# augment the dataset with additional variables
# cumulative proportion of females in each conversation
df <- df %>% 
  group_by(ThreadId) %>% 
  mutate(FemaleCumulativeProportion = cummean(Female) * 100) %>% 
  ungroup

# difference between female comments in current conversation and previous conversation
tmp <- df %>%
  group_by(ThreadId, Id) %>% 
  summarise(comments_now = n(),
            PreviousContributions = mean(PreviousContributions),
            PreviousThreads = mean(PreviousThreads),
            ContributionsThisYear = mean(ContributionsThisYear),
            ThreadsThisYear = mean(ThreadsThisYear)) %>%
  mutate(cont_per_thread = (PreviousContributions + ContributionsThisYear)/(PreviousThreads + ThreadsThisYear),
         CommentsChange = (comments_now - cont_per_thread)*100/cont_per_thread) %>%
  select(CommentsChange, ThreadId, Id)

df = left_join(df, tmp, by=c('Id' = 'Id', 'ThreadId' = 'ThreadId'))

# filtering
df <- df %>%
  filter({{filter}})

# todo: remove the outlier in A6?

# --- (Unit) comment

# --- (Unit) thread
# first perform necessary per-thread aggregation, then keep one row per thread
df <- df %>%
  group_by(ThreadId) %>%
  mutate(
    CommentsChange = mean(CommentsChange),
    WC = mean(WC)
  ) %>%
  ungroup %>%
  distinct(ThreadId, .keep_all = TRUE)

# --- (Unit) author
df <- df %>%
  mutate(
    WC = mean(WC)
  ) %>%
  ungroup %>%
  distinct(ThreadId, Id, .keep_all = TRUE)

# # --- (Unit) custom_A2
# # calculate the DV and IV for A2
# # number of female contributors ordered by time of commenting (first, second, third female contributor, etc)
# df <- df %>%
#   filter(Female == 1) %>%
#   arrange(ThreadId, Id_num, Order) %>% 
#   group_by(ThreadId, Id_num) %>%
#   mutate(first_post = c(1, rep(0,n()-1))) %>% # mark 1st post 1, others 0
#   ungroup() %>%
#   arrange(ThreadId, Order) %>% 
#   group_by(ThreadId) %>% 
#   mutate(n_females = lag(cumsum(first_post)),
#          n_females = ifelse(is.na(n_females), 0, n_females)
#   ) %>%
#   ungroup() %>%
#   group_by(ThreadId, n_females) %>%
#   summarise(n_posts= n()) %>%
#   ungroup()

# --- (Model) lm
model <- lm({{DV}} ~ {{IV}} {{covariates}}, data=df)
summary(model)

# --- (O)
# wrangle results
result <- tidy(model, conf.int = TRUE) %>%
  filter(term == '{{IV}}')

# output
write_csv(result, '../results/estimate_{{_n}}.csv')
