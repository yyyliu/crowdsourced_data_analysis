# --- (BOBA_CONFIG)
{
  "decisions": [
    {"var": "filter", "options": [
      "",
      "Limited_Information == 0, HavePhD == 1, !(ThreadId == 342 & Id == 283)"
    ]},
    {"var": "DV", "options": [
      "LogNumChar",
      "ThreadsThisYear",
      "MeanWC"
    ]},
    {"var": "IV", "options": [
      "AcademicHierarchyStrict",
      "Job_Title_S"
    ]},
    {"var": "random_term", "options": [
      ""
    ]},
    {"var": "covariates", "options": [
      "",
      "+ Female + Academic"
    ]},
    {"var": "IV_alias", "options": [
      "AcademicHierarchyStrict",
      "Job_Title_SChaired Professor"
    ]}
  ],
  "constraints": [
    {"link": ["IV", "IV_alias"]},
    {"variable": "DV", "option": "LogNumChar",
      "condition": "Unit == comment"},
    {"variable": "DV", "option": "ThreadsThisYear",
      "condition": "Unit == comment"},
    {"variable": "DV", "option": "MeanWC",
      "condition": "Unit == author"}
  ],
  "before_execute": "rm -rf results && mkdir results",
  "after_execute": "cp ../after_execute.sh ./ && sh after_execute.sh"
}
# --- (END)

library(readr)
library(tidyverse)
library(broom.mixed)
source('../../../boba_util.R')

df <- read.csv(file='../../../data/edge1.1_anonymized.csv')

# LogNumChar: the natural log of Number.Characters
df <- df %>%
  mutate(LogNumChar=log(Number.Characters))

# MeanWC: average # words for each contributor in a single conversation
tmp <- df %>%
  group_by(ThreadId, Id) %>%
  summarise(total_wpt = sum(WC)) %>%
  group_by(Id) %>%
  summarise(MeanWC = sum(total_wpt)/n())

df <- left_join(df, tmp, by = "Id")

# filtering
df <- df %>%
  filter({{filter}})

# todo: exclude all NAs as in A5?

# hack
tmp = '{{random_term}}'

# --- (Unit) comment

# --- (Unit) author
df <- df %>%
  group_by(Id) %>%
  slice(1) %>%
  ungroup()

# --- (Model) lm
model <- lm({{DV}} ~ {{IV}} {{covariates}}, data=df)
summary(model)

# compute z score
result <- tidy(model, conf.int = TRUE) %>%
  filter(term == '{{IV_alias}}') %>%
  mutate(
    # they seem to calculate p value from t distribution when the model summary
    # do not report the exact p-value, usually because it's very small
    p = pt(statistic, df.residual(model), lower.tail=FALSE),
    p = ifelse(p.value > 1e-14, p.value, p),
    z = qnorm(p),
    # make z the same sign as the estimate
    z = ifelse(sign(z)==sign(estimate), z, -z)
  )

# --- (O)
# output
write_csv(result, '../results/estimate_{{_n}}.csv')
