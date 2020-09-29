# --- (BOBA_CONFIG)
{
  "decisions": [
    {"var": "filter", "options": [
      "Type == 2",
      "DebateSize > 1",
      "Type == 2 & DebateSize > 1",
      "UniqueFemaleContributors >= 1 & UniqueContributors > 1", 
      "Female_Contributions==UniqueFemaleContributors",
      "UniqueFemaleParticipation > 0 & UniqueFemaleParticipation < 1",
      ""
    ]},
    {"var": "DV", "options": [
      "ContributionsbyAuthor",
      "FemaleParticipation",
      "WC",
      "CommentsChange",
      "NextFemale",
      "Female",
      "Female_Contributions",
      "MeanFemaleComments"
    ]},
    {"var": "IV", "options": [
      "UniqueFemaleContributors",
      "FemaleCumulativeProportion",
      "FemaleCurrentCount",
      "FemalePreviousCount",
      "FemaleParticipation",
      "UniqueFemaleParticipation"
    ]},
    {"var": "covariates", "options": [
      "+ PreviousContributions + HavePhD + Total_Citations",
      "+ UniqueContributors",
      "+ Discipline",
      "+ LiveEver + DebateSizeZ",
      "+ AuthorPreviousComments",
      ""
    ]},
    {"var": "random_term", "options": [
      "+ (1|ThreadId)",
      "+ (1 + UniqueFemaleParticipation | Id_num) + (1 | ThreadId)",
      ""
    ]},
    {"var": "glm_call", 
      "options": ["glmer", "glmer", "glm"]},
    {"var": "female_only", 
      "options": ["Female == 1", "", "Female == 1", "Female == 1", "", "", "", ""]}
  ],
  "constraints": [
    {"link": ["DV", "female_only"]},
    {"link": ["random_term", "glm_call"]},
    {"variable": "DV", "option": "ContributionsbyAuthor", 
      "condition": "Unit != thread and IV != FemaleCurrentCount and IV != FemalePreviousCount and IV != FemaleParticipation"},
    {"variable": "DV", "option": "FemaleParticipation",
      "condition": "Unit != comment and (IV == UniqueFemaleContributors or IV == UniqueFemaleParticipation)"},
    {"variable": "DV", "option": "NextFemale",
      "condition": "Unit == comment and Model == logistic and (IV == FemaleCurrentCount or IV == FemalePreviousCount)"},
    {"variable": "DV", "option": "Female",
      "condition": "Unit == comment and Model == logistic and IV == FemalePreviousCount"},
    {"variable": "DV", "option": "Female_Contributions", 
      "condition": "IV != FemaleCurrentCount and IV != FemalePreviousCount and IV != FemaleCumulativeProportion"},
    {"variable": "DV", "option": "MeanFemaleComments",
      "condition": "Unit != comment and filter.index == 5 and (IV == UniqueFemaleParticipation or IV == UniqueFemaleContributors)"},
    {"variable": "IV", "option": "FemaleCumulativeProportion",
      "condition": "Unit == comment"},
    {"variable": "IV", "option": "FemaleCurrentCount",
      "condition": "Unit == comment"},
    {"variable": "IV", "option": "FemalePreviousCount",
      "condition": "Unit == comment"},
    {"variable": "covariates", "index": 0,
      "condition": "Unit != thread"},
    {"variable": "covariates", "index": 2,
      "condition": "Unit != thread"},
    {"variable": "covariates", "index": 3,
      "condition": "Unit == thread"},
    {"variable": "covariates", "index": 4,
      "condition": "Unit != thread and DV == ContributionsbyAuthor"},
    {"variable": "random_term", "index": 1,
      "condition": "Unit != thread and IV == UniqueFemaleParticipation"},
    {"block": "Model", "option": "logistic",
      "condition": "DV == NextFemale or DV == Female"}
  ],
  "before_execute": "rm -rf results && mkdir results",
  "after_execute": "boba merge estimate_{}.csv -b results --out estimate.csv",
  "visualizer": "visualizer_config.json"
}
# --- (END)

library(readr)
library(tidyverse)
library(broom.mixed)
source('../../boba_util.R')

df <- read.csv(file='../../../data/edge1.1_anonymized.csv')

# augment the dataset with additional variables
# FemaleCumulativeProportion: cumulative proportion of females in each conversation
# MeanFemaleComments: proxy for average # of comments made by each woman in a conversation
# FemaleCurrentCount: cumulative sum of female comments in a thread, including the current comment
# FemalePreviousCount: cumulative sum of female comments in a thread, excluding the current comment
# NextFemale: odds of next contributor to conversation being a woman
df <- df %>% 
  group_by(ThreadId) %>% 
  mutate(
    FemaleCumulativeProportion = cummean(Female) * 100
  ) %>% 
  arrange(Order) %>%
  mutate(
    MeanFemaleComments = Female_Contributions/UniqueFemaleContributors,
    FemaleCurrentCount = cumsum(Female),
    FemalePreviousCount = cumsum(Female) - Female,
    NextFemale =lead(Female)) %>%
  ungroup

# AuthorPreviousComments: mean number of previous comments per thread, avoiding division by zero
df <- df %>%
  mutate(
    AuthorPreviousComments = PreviousContributions / (PreviousThreads + 0^PreviousThreads)
  )

# CommentsChange: difference between female comments in current conversation and previous conversation
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

# LiveEver: whether the thread has ever been live
tmp <- df %>% 
  select(ThreadId, Live) %>%
  distinct %>%
  group_by(ThreadId) %>%
  mutate(
    LiveEver = as.numeric(ifelse(any(Live == 1), 1, Live))
  ) %>%
  select(LiveEver, ThreadId) %>%
  distinct

df = left_join(df, tmp, by=c('ThreadId' = 'ThreadId'))

# filtering
df <- df %>%
  filter({{filter}})

# DebateSizeZ: appeared in A12, must be done after filtering
df$DebateSizeZ <- with(df, (DebateSize - mean(DebateSize)) / sd(DebateSize))

# todo: remove the outlier in A6?
# todo: remove the outlier in A5?

# hack for the constraint to work
formula = '{{IV}} {{DV}}'

# some IV/DV requires us to include only female comments
df <- df %>%
  filter({{female_only}})

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

# compute z score
result <- tidy(model, conf.int = TRUE) %>%
  filter(term == '{{IV}}') %>%
  mutate(
    z = qnorm(p.value),
    # make z the same sign as the estimate
    z = ifelse(sign(z)==sign(estimate), z, -z)
  )

# --- (Model) logistic
model <- glm({{DV}} ~ {{IV}} {{covariates}}, data=df, family = binomial)
summary(model)

# compute z score
result <- tidy(model, conf.int = TRUE) %>%
  filter(term == '{{IV}}') %>%
  mutate(
    z = estimate/((estimate - conf.low) / qnorm(.975)),
    # make z the same sign as the estimate
    z = ifelse(sign(z)==sign(estimate), z, -z)
  )

# --- (Model) pearson
model = cor.test(df${{IV}}, df${{DV}})
summary(model)

# compute z score
result <- tidy(model, conf.int = TRUE) %>%
  mutate(
    fr = 0.5 * log((1 + estimate)/(1 - estimate)),
    se = sqrt(1/(parameter - 3)),
    z = fr/se
  ) %>%
  select(-fr, -se)

# --- (Model) kendall
model = cor.test(df${{IV}}, df${{DV}}, method = 'kendall')
summary(model)

# todo: calculate z score
result <- tidy(model, conf.int = TRUE) %>%
  mutate(
    z = NA
  )

# --- (Model) poisson
library(lme4)

# hack for constraint to work
tmp = '{{random_term}}'

model = {{glm_call}}({{DV}} ~ {{IV}} {{covariates}} {{random_term}},
  data = df, family = poisson)
summary(model)

result <- tidy(model, conf.int = TRUE) %>%
  filter(term == '{{IV}}') %>%
  mutate(z = statistic)

# --- (O)

# output
write_csv(result, '../results/estimate_{{_n}}.csv')
