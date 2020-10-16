# --- (BOBA_CONFIG)
{
  "decisions": [
    {"var": "filter", "options": [
      ""
    ]},
    {"var": "DV", "options": [
      "LogNumChar",
      "ThreadsThisYear"
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
    {"link": ["IV", "IV_alias"]}
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

# todo: exclude all NAs as in A5?

# hack
tmp = '{{random_term}} {{filter}}'

# --- (Unit) comment

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