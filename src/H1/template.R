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
      "FemaleParticipation < 0.20, Female_Contributions <= 22.5",
      "!(ThreadId == 98 & Id == 90)",
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
      "MeanFemaleComments",
      "NumPosts"
    ]},
    {"var": "IV", "options": [
      "UniqueFemaleContributors",
      "FemaleCumulativeProportion",
      "FemaleCurrentCount",
      "FemalePreviousCount",
      "FemaleParticipation",
      "UniqueFemaleParticipation",
      "NumFemale"
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
      "options": ["Female == 1", "", "Female == 1", "Female == 1", "", "", "", "", ""]}
  ],
  "constraints": [
    {"link": ["DV", "female_only"]},
    {"link": ["random_term", "glm_call"]},
    {"variable": "DV", "option": "Female_Contributions", 
      "condition": "Unit != author and IV != FemaleCurrentCount and IV != FemalePreviousCount and IV != FemaleCumulativeProportion"},
    {"variable": "DV", "option": "ContributionsbyAuthor", 
      "condition": "filter.index != 4 and filter.index != 6 and IV != FemaleCurrentCount and IV != FemalePreviousCount and IV != FemaleParticipation and IV != FemaleCumulativeProportion"},
    {"variable": "DV", "option": "WC",
      "condition": "filter.index != 6"},
    {"variable": "DV", "option": "NextFemale",
      "condition": "Unit == comment and Model == logistic and (IV == FemaleCurrentCount or IV == FemalePreviousCount or IV == FemaleCumulativeProportion)"},
    {"variable": "DV", "option": "NumPosts",
      "condition": "Unit == custom"},
    {"variable": "DV", "option": "CommentsChange",
      "condition": "filter.index != 6 and Unit != comment and Model != poisson and IV != FemaleCurrentCount and IV != FemalePreviousCount and IV != FemaleCumulativeProportion"},
    {"variable": "DV", "option": "Female",
      "condition": "Unit == comment and Model == logistic and IV == FemalePreviousCount"},
    {"variable": "DV", "option": "FemaleParticipation",
      "condition": "Unit == thread and (IV == UniqueFemaleContributors or IV == UniqueFemaleParticipation)"},
    {"variable": "DV", "option": "MeanFemaleComments",
      "condition": "Unit == thread and filter.index != 4 and (IV == UniqueFemaleParticipation or IV == UniqueFemaleContributors)"},
    {"variable": "IV", "option": "UniqueFemaleParticipation",
      "condition": "Unit == comment or Unit == thread"},
    {"variable": "IV", "option": "FemaleCurrentCount",
      "condition": "Unit == comment"},
    {"variable": "IV", "option": "NumFemale",
      "condition": "Unit == custom"},
    {"variable": "IV", "option": "FemaleParticipation",
      "condition": "Unit == comment or Unit == thread"},
    {"variable": "IV", "option": "FemalePreviousCount",
      "condition": "Unit == comment"},
    {"variable": "IV", "option": "FemaleCumulativeProportion",
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
    {"block": "Unit", "option": "custom",
      "condition": "DV == NumPosts and IV == NumFemale"},
    {"block": "Model", "option": "logistic",
      "condition": "DV == NextFemale or DV == Female"},
    {"block": "A", "skippable": true, "condition": "filter.index == 6"}
  ],
  "before_execute": "rm -rf results && mkdir results",
  "after_execute": "cp ../after_execute.sh ./ && sh after_execute.sh",
  "visualizer": "visualizer_config.json"
}
# --- (END)

suppressPackageStartupMessages(library(readr))
suppressPackageStartupMessages(library(tidyverse))
suppressPackageStartupMessages(library(broom.mixed))
source('../../../boba_util.R')

df <- read.csv(file='../../../data/edge1.1_anonymized.csv')

# hack for the constraint to work
formula = '{{filter}} {{IV}} {{DV}}'

# --- (A)
# removing any rows with NAs, must be done before adding new variables
# we will tie this filtering to another option in "filter", because A5 did both
df <- df %>%
  filter(complete.cases(df))

# --- (B)
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
    ContributionsbyAuthor = mean(ContributionsbyAuthor),
    CommentsChange = mean(CommentsChange),
    WC = mean(WC)
  ) %>%
  ungroup %>%
  distinct(ThreadId, .keep_all = TRUE)

# --- (Unit) author
df <- df %>%
  group_by(ThreadId, Id) %>%
  mutate(
    WC = mean(WC)
  ) %>%
  ungroup %>%
  distinct(ThreadId, Id, .keep_all = TRUE)

# --- (Unit) custom
# A custom unit of analysis by A2. se also hardcode other decisions here.
# NumFemale: number of female contributors in the thread before this comment
# NumPosts: proxy for number of comments by each female contributor
df <- df %>%
  filter(Female == 1) %>%
  arrange(ThreadId, Id_num, Order) %>% 
  group_by(ThreadId, Id_num) %>%
  mutate(first_post = c(1, rep(0,n()-1))) %>% # mark 1st post 1, others 0
  ungroup() %>%
  arrange(ThreadId, Order) %>% 
  group_by(ThreadId) %>% 
  mutate(NumFemale = lag(cumsum(first_post)),
         NumFemale = ifelse(is.na(NumFemale), 0, NumFemale)
  ) %>%
  ungroup() %>%
  group_by(ThreadId, NumFemale) %>%
  summarise(NumPosts= n()) %>%
  ungroup() %>%
  filter(NumFemale > 0)

# --- (Model) lm
model <- lm({{DV}} ~ {{IV}} {{covariates}}, data=df)
summary(model)

# compute z score
result <- tidy(model, conf.int = TRUE) %>%
  filter(term == '{{IV}}') %>%
  mutate(
    # they seem to calculate p value from t distribution when the model summary
    # do not report the exact p-value, usually because it's very small
    p = pt(statistic, df.residual(model), lower.tail=FALSE),
    p = ifelse(p.value > 1e-14, p.value, p),
    z = qnorm(p),
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
    z = statistic
  )

# --- (Model) pearson
model = cor.test(df${{IV}}, df${{DV}})
model

# compute z score
result <- tidy(model, conf.int = TRUE) %>%
  mutate(
    # they seem to calculate p value from t distribution when the model summary
    # do not report the exact p-value, usually because it's very small
    p = pt(statistic, parameter, lower.tail=FALSE),
    p = ifelse(p.value > 1e-14, p.value, p),
    # A4 uses this method to compute z score, probably because p value is 0
    fr = 0.5 * log((1 + estimate)/(1 - estimate)),
    se = sqrt(1/(parameter - 3)),
    z = ifelse(p.value == 0, fr/se, qnorm(p)),
    # make z the same sign as the estimate
    z = ifelse(sign(z)==sign(estimate), z, -z)
  ) %>%
  select(-fr, -se)

# --- (Model) kendall
model = cor.test(df${{IV}}, df${{DV}}, method = 'kendall')
model

# calculate z score
result <- tidy(model, conf.int = TRUE) %>%
  mutate(
    z = statistic
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
