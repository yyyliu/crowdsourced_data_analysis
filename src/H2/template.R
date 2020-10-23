# --- (BOBA_CONFIG)
{
  "decisions": [
    {"var": "filter", "options": [
      "",
      "Limited_Information == 0, HavePhD == 1, !(ThreadId == 342 & Id == 283)",
      "Role == 2",
      "DebateSize > 1",
      "Academic==1, TotalCommentsById < 100"
    ]},
    {"var": "DV", "options": [
      "ThreadsThisYear",
      "MeanWC",
      "WC",
      "ScaledWC",
      "NumCharacters",
      "WPS"
    ]},
    {"var": "IV", "options": [
      "AcademicHierarchyStrict",
      "Job_Title_S",
      "LogCitations",
      "PhdRanking",
      "CustomHierarchy",
      "ScaledTotalCitations",
      "Status",
      "H_Index",
      "Workplace_US_Bin"
    ]},
    {"var": "random_term", "options": [
      "+ (1 | Id_num) + (1 + ScaledTotalCitations | ThreadId)",
      "+ (1 | ThreadId) + (1 | Id)",
      "+ (1 | Live)"
    ]},
    {"var": "covariates", "options": [
      "",
      "+ Female + Academic",
      "+ AcademicHierarchyStrict + Discipline",
      "+ CustomDiscipline + Male",
      "+ Female",
      "+ 1 + WorkplaceMeanRank + OrderedAcademicHierarchy",
      "+ Role + Female + Type",
      "+ Female + Years_from_PhD + LogTotalCitations + AcademicHierarchyStrict",
      "+ Workplace_SR_Bin + LogTotalCitations"
    ]},
    {"var": "IV_alias", "options": [
      "AcademicHierarchyStrict",
      "Job_Title_SChaired Professor",
      "LogCitations",
      "PhdRanking",
      "CustomHierarchy6",
      "ScaledTotalCitations",
      "Status",
      "H_Index",
      "Workplace_US_Bin"
    ]}
  ],
  "constraints": [
    {"link": ["IV", "IV_alias"]},
    {"variable": "DV", "option": "ThreadsThisYear",
      "condition": "Unit == comment"},
    {"variable": "DV", "option": "NumCharacters",
      "condition": "Unit == comment or Unit == custom_A23"},
    {"variable": "DV", "option": "MeanWC",
      "condition": "Unit == author or Unit == custom_A12"},
    {"variable": "DV", "option": "WC",
      "condition": "Unit == comment or Unit == author"},
    {"variable": "DV", "option": "ScaledWC",
      "condition": "Unit == comment"},
    {"variable": "DV", "option": "WPS",
      "condition": "Unit == comment and IV == H_Index"},
    {"variable": "DV", "option": "NumCharacters",
      "condition": "Unit == comment or Unit == custom_A23"},
    {"variable": "IV", "option": "PhdRanking",
      "condition": "Unit == author"},
    {"variable": "IV", "option": "CustomHierarchy",
      "condition": "Unit == custom_A12"},
    {"variable": "covariates", "index": 3,
      "condition": "Unit == custom_A12"},
    {"variable": "random_term", "index": 0,
      "condition": "IV == ScaledTotalCitations"},
    {"block": "A", "condition": "IV == Status", "skippable": true},
    {"block": "Unit", "option": "custom_A23",
      "condition": "IV == AcademicHierarchyStrict and DV == NumCharacters"},
    {"block": "Transform", "option": "log",
      "condition": "DV == WC or DV == NumCharacters"}
  ],
  "before_execute": "rm -rf results && mkdir results",
  "after_execute": "cp ../after_execute.sh ./ && sh after_execute.sh"
}
# --- (END)

library(readr)
library(lmerTest)
library(tidytext)
library(tidyverse)
library(broom.mixed)
source('../../../boba_util.R')

df <- read.csv(file='../../../data/edge1.1_anonymized.csv', stringsAsFactors = FALSE)

# LogCitations: the natural log of Citations_Cumulative, used as an IV
# LogTotalCitations: the natural log of Total_Citations, used as a covariate
# PhdRanking: combined ranking of whether they have PhD and the rank of their academic workplace
# CustomHierarchy: a reordered, factorized version of AcademicHierarchyStrict, by A12
# WorkplaceMeanRank: mean workplace rank
# ScaledTotalCitations: Total_Citations divided by 1000
# ScaledWC: word count divided by 100
# OrderedAcademicHierarchy: AcademicHierarchyStrict as an ordered factor
df <- df %>%
  mutate(
    NumCharacters = Number.Characters,
    LogCitations = log(Citations_Cumulative),
    LogTotalCitations = log(Total_Citations),
    PhdRanking = ifelse(HavePhD == 1, ifelse(!is.na(Workplace_SR_Bin), 
      Workplace_SR_Bin, "no_rank"), "no_phd"),
    PhdRanking = forcats::fct_relevel(PhdRanking, "no_phd", "no_rank", 
      "7", "6", "5", "4", "3", "2", "1"),
    PhdRanking = as.numeric(PhdRanking),
    CustomHierarchy = AcademicHierarchyStrict %>%ifelse(is.na(.), 7, .),
    CustomHierarchy = factor(CustomHierarchy, levels=c(5, 7, 1:4, 6)),
    WorkplaceMeanRank = rowMeans(.[,c("Workplace_SR_Bin", "Workplace_US_IR_Bin")], na.rm = TRUE),
    WorkplaceMeanRank = ordered(WorkplaceMeanRank),
    ScaledTotalCitations = Total_Citations / 1000,
    ScaledWC = WC / 100,
    OrderedAcademicHierarchy = ordered(AcademicHierarchyStrict)
  )

# MeanWC: average # words for each contributor in a single conversation
tmp <- df %>%
  group_by(ThreadId, Id) %>%
  summarise(total_wpt = sum(WC)) %>%
  group_by(Id) %>%
  summarise(MeanWC = sum(total_wpt)/n())

df <- left_join(df, tmp, by = "Id")

# TotalCommentsById: total comments for each id
tmp <- df %>%
  group_by(Id) %>%
  summarise(TotalCommentsById = n())

df <- left_join(df, tmp, by = "Id")

# filtering
df <- df %>%
  filter({{filter}})

# todo: exclude all NAs as in A5?

# hack
tmp = '{{IV}} {{DV}}'

# --- (A)
# All this mess is for creating the custom variable Status, used in A10
z. <- function(X){
  (X - mean(X, na.rm=T))/sd(X, na.rm=T)
}
ind <- df$Id

phd <- df$HavePhD
phd[is.na(phd)] <- 0 # Assumes no info equals no PhD
phd <- tapply(phd, ind, mean)

academic <- as.numeric(df$Academic)
academic[is.na(academic)] <- 0 # Assumes no info equals not academic
academic <- tapply(academic, ind, mean)

phdBin <- df$PhD_Institution_US_IR_Bin
phdBin[is.na(phdBin)] <- 0
phdBin <- tapply(phdBin, ind, mean)
phdBin <- z.(phdBin)

workBin <- df$Workplace_US_IR_Bin
workBin[is.na(workBin)] <- 0
workBin <- tapply(workBin, ind, mean)
workBin <- z.(workBin)

threadPart <- tapply(df$ThreadId, df$Id_num, function(x) length(unique(x))) # How many threads in total?
threadPart <- z.(threadPart)

# Citation indexes correlates highly with total citations
citation <- df$Total_Citations
citation[is.na(citation)] <- 0 # Assumes no info equals no citations
citation <- tapply(citation, ind, mean)
citation <- z.(citation)

acadHier <- df$AcademicHierarchyStrict
acadHier[is.na(acadHier)] <-0 # Again
acadHier <- tapply(acadHier, ind, mean)
acadHier <- z.(acadHier)

statusVar <- data.frame(phd, academic, phdBin, workBin, threadPart, citation, acadHier)

# 'loadings' weights come from factor analysis
loadings <- as.numeric(factanal(statusVar, factors=1)$loadings)
status <- apply(statusVar, 1, function(x) sum(x * loadings)/length(loadings))
status <- status[as.numeric(factor(df$Id))]
status <- z.(status)

df <- df %>% add_column(Status = status)

# --- (Unit) comment

# --- (Unit) author
df <- df %>%
  group_by(Id) %>%
  mutate(
    WC = mean(WC, na.rm = T),
    PhdRanking = mean(PhdRanking)
  ) %>%
  slice(1) %>%
  ungroup()

# --- (Unit) custom_A12
tmp = df %>%
  distinct(Id_num,Academic,Discipline,AcademicHierarchyStrict,USA_Ranking_Dif, .keep_all=TRUE) %>%
  mutate(
    CustomDiscipline = ifelse(is.na(Discipline), "missing", Discipline)
  ) %>%
  select(-MeanWC)

persons <- df %>% 
  filter(TwoAuthors != 1) %>% 
  select(ThreadId, Id_num, Text) %>% 
  distinct %>% 
  group_by(Id_num, ThreadId) %>% 
  mutate(comment_num = row_number()) %>% 
  ungroup %>% 
  unnest_tokens(word, Text) %>%
  count(Id_num, ThreadId, comment_num) %>%
  group_by(Id_num) %>%
  summarize(MeanWC = mean(n))

df = left_join(persons, tmp, by=c("Id_num"))

# though this is a filter, the variable is only available here
df = df %>%
  filter(MeanWC <= 3000)

# --- (Unit) custom_A23
df <- df %>% 
  group_by(Id, AcademicHierarchyStrict, Female) %>% 
  # Academic Status might change, so taking the mean. This is not ideal though.
  summarise(NumCharacters = mean(NumCharacters)) %>%
  ungroup %>%
  mutate(AcademicHierarchyStrict = as.numeric(as.character(AcademicHierarchyStrict)))

# --- (Transform) none

# --- (Transform) log
df <- df %>%
  mutate({{DV}} = log({{DV}}))

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

# --- (Model) lmer
model <- lmer({{DV}} ~ {{IV}} {{covariates}} {{random_term}}, data=df)
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
