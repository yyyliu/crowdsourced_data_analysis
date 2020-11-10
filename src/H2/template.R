# --- (BOBA_CONFIG)
{
  "decisions": [
    {"var": "filter", "options": [
      "",
      "Limited_Information == 0, HavePhD == 1, !(ThreadId == 342 & Id == 283)",
      "Role == 2",
      "DebateSize > 1",
      "Academic==1, TotalCommentsById < 100",
      "!is.na(AcademicHierarchyStrict)",
      "complete.cases(df)"
    ]},
    {"var": "DV", "options": [
      "ThreadsThisYear",
      "MeanWC",
      "WC",
      "ScaledWC",
      "CustomWC",
      "NumCharacters",
      "WPS",
      "SpokePCA",
      "ContributionsThisYear2013"
    ]},
    {"var": "IV", "options": [
      "AcademicHierarchyStrict",
      "OrderedAcademicHierarchy",
      "AcademicHierarchyStrict2013",
      "CustomHierarchy",
      "LogCitations",
      "ScaledTotalCitations",
      "Job_Title_S",
      "PhdRanking",
      "H_Index",
      "Workplace_US_Bin",
      "Status",
      "StatusPCA"
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
      "+ Workplace_SR_Bin + LogTotalCitations",
      "+ Lead + Year + Female + Loc + (Loc):(StatusPCA + Lead + Year + Female)"
    ]},
    {"var": "IV_alias", "options": [
      "AcademicHierarchyStrict",
      "OrderedAcademicHierarchy.L",
      "AcademicHierarchyStrict2013",
      "CustomHierarchy6",
      "LogCitations",
      "ScaledTotalCitations",
      "Job_Title_SChaired Professor",
      "PhdRanking",
      "H_Index",
      "Workplace_US_Bin",
      "Status",
      "StatusPCA"
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
    {"variable": "DV", "option": "SpokePCA",
      "condition": "Unit == custom_A18"},
    {"variable": "DV", "option": "ContributionsThisYear2013",
      "condition": "Unit == author"},
    {"variable": "IV", "option": "AcademicHierarchyStrict2013",
      "condition": "Unit == author"},
    {"variable": "IV", "option": "PhdRanking",
      "condition": "Unit == author"},
    {"variable": "IV", "option": "CustomHierarchy",
      "condition": "Unit == custom_A12"},
    {"variable": "IV", "option": "StatusPCA",
      "condition": "Unit == custom_A18"},
    {"variable": "covariates", "index": 1,
      "condition": "filter.index != 4 and filter.index != 6 and IV != StatusPCA and IV != Status"},
    {"variable": "covariates", "index": 2,
      "condition": "IV.index > 3 and IV != StatusPCA and IV != Status"},
    {"variable": "covariates", "index": 3,
      "condition": "Unit == custom_A12"},
    {"variable": "covariates", "index": 4,
      "condition": "IV != StatusPCA"},
    {"variable": "covariates", "index": 5,
      "condition": "IV.index > 3 and IV != StatusPCA and IV != Status"},
    {"variable": "covariates", "index": 6,
      "condition": "filter.index != 2 and IV != StatusPCA"},
    {"variable": "covariates", "index": 7,
      "condition": "IV != LogCitations and IV != ScaledTotalCitations and IV != StatusPCA and IV != Status"},
    {"variable": "covariates", "index": 8,
      "condition": "IV != LogCitations and IV != ScaledTotalCitations and IV != StatusPCA and IV != Status"},
    {"variable": "covariates", "index": 9,
      "condition": "Unit == custom_A18 and DV == SpokePCA"},
    {"variable": "random_term", "index": 0,
      "condition": "IV == ScaledTotalCitations and Unit != author"},
    {"variable": "random_term", "index": 1,
      "condition": "Unit != author"},
    {"variable": "random_term", "index": 2,
      "condition": "filter.index != 2"},
    {"block": "A", "condition": "IV == Status", "skippable": true},
    {"block": "B", "condition": "DV == CustomWC", "skippable": true},
    {"block": "C", "skippable": true, 
      "condition": "DV == ContributionsThisYear2013 or IV == AcademicHierarchyStrict2013"},
    {"block": "Unit", "option": "custom_A23",
      "condition": "IV == AcademicHierarchyStrict and DV == NumCharacters"},
    {"block": "Unit", "option": "custom_A18",
      "condition": "DV == SpokePCA and IV == StatusPCA"},
    {"block": "Model", "option": "spearman", 
      "condition": "IV != Job_Title_S and IV != OrderedAcademicHierarchy and IV != CustomHierarchy"},
    {"block": "Model", "option": "kendall", 
      "condition": "IV != Job_Title_S and IV != PhdRanking and IV != Workplace_US_Bin and IV != OrderedAcademicHierarchy and IV != CustomHierarchy"},
    {"block": "Transform", "option": "log",
      "condition": "DV == WC or DV == NumCharacters"}
  ],
  "before_execute": "rm -rf results && mkdir results",
  "after_execute": "cp ../after_execute.sh ./ && sh after_execute.sh"
}
# --- (END)

suppressPackageStartupMessages(library(readr))
suppressPackageStartupMessages(library(reshape))
suppressPackageStartupMessages(library(lmerTest))
suppressPackageStartupMessages(library(tidytext))
suppressPackageStartupMessages(library(tidyverse))
suppressPackageStartupMessages(library(broom.mixed))
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
# CustomWC: word count computed by regex. Here it's a dummy variable; it'll be computed later
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
    CustomWC = WC,
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

# hack
tmp = '{{IV}} {{DV}}'

# --- (C)
# two weird variables, used in A22
# ContributionsThisYear2013: contributions in the year 2013, where missing value is assigned 0
tmp=cast(df, Id_num ~ Year,value=c('ContributionsThisYear'))
names(tmp)=paste0("ContributionsThisYear",names(tmp))
tmp = tmp %>%
  rename(Id_num = ContributionsThisYearId_num) %>%
  select(Id_num, ContributionsThisYear2013)
df <- left_join(df, tmp, by = "Id_num")

# AHS2013: mean AcademicHierarchyStrict in the year 2013, with imputation
tmp=cast(df, Id_num ~ Year, value=c('AcademicHierarchyStrict'), fun=mean)
names(tmp)=paste0("AHS",names(tmp))
tmp = tmp %>%
  rename(Id_num = AHSId_num) %>%
  rowwise() %>% 
  mutate(
    imputed = mean(c(AHS2012, AHS2014), na.rm=TRUE),
    AHS2013 = ifelse(is.na(AHS2013), imputed, AHS2013)
  ) %>%
  select(Id_num, AHS2013) %>%
  rename(AcademicHierarchyStrict2013 = AHS2013)
df <- left_join(df, tmp, by = "Id_num")

# --- (B)
# Create a custom word count variable used in A21
# Made a block for it because it is slow
countWords = function(x) {
  length(unlist(strsplit(as.character(x), "\\W+")))
}

df = df %>%
  rowwise() %>%
  mutate(CustomWC = countWords(Text)) %>%
  ungroup

# --- (A)
# All this mess is for creating the custom variable Status, used in A10
z. <- function(X){
  (X - mean(X, na.rm=T))/sd(X, na.rm=T)
}
ind <- df$Id

phd <- df$HavePhD
phd[is.na(phd)] <- 0 # Assumes no info equals no PhD
phd <- tapply(phd, ind, mean)

academic <- as.numeric(factor(df$Academic))
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
    CustomWC = mean(CustomWC),
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

# --- (Unit) custom_A18
wrangle <- function (df) {
  tmp <- df %>%
    group_by(ThreadId) %>%
    mutate(
      debate_size = n(),
      tota_chars = sum(Number.Characters),
      unique_ids = n_distinct(Id),
      row_id = row_number()
    ) %>%
    ungroup %>%
    group_by(Id, ThreadId) %>%
    mutate(
      Lead = if_else(min(Order) == 1, 1, 0),
      FracTimesSpoke = n() / debate_size,
      FracCharSpoke = sum(Number.Characters) / tota_chars,
      UniqueContributors = unique_ids,
      DebateSize = debate_size
    ) %>%
    arrange(row_id) %>%
    slice(1) %>%
    ungroup %>%
    select(Id,ThreadId,Lead,FracTimesSpoke,FracCharSpoke,Year,UniqueContributors,
          DebateSize,Female,Academic,HavePhD,PhD_Year,PhD_Institution_SR_Bin,Workplace_SR_Bin,
          PhD_Institution_US_IR_Bin,Workplace_US_IR_Bin,Total_Citations,H_Index,i10_Index,AcademicHierarchyStrict,
          PreviousCitations,Limited_Information) %>%
    filter(Limited_Information==0, FracTimesSpoke < 1, !is.na(Female))

  tmp[tmp == 'Not Available'] <- NA

  tmp <- tmp %>%
    mutate_all(as.numeric) %>%
    filter(Academic == 1, !is.na(PhD_Year), !is.na(AcademicHierarchyStrict)) %>%
    mutate_all(as.numeric)

  scaled<-apply( X =tmp[,4:22],FUN = scale, center = TRUE, scale = TRUE ,MARGIN = 2)
  tmp[,4:22]<-scaled

  tmp <- tmp %>%
    mutate(
      SpokePCA = prcomp(x=cbind(tmp$FracCharSpoke, tmp$FracTimesSpoke), retx=T, center=T, scale.=T)$x[,1],
      StatusPCA = prcomp(x=cbind(tmp$PhD_Year, tmp$AcademicHierarchyStrict), retx=T, center=T, scale.=T)$x[,1]
    )

  # add back other fields from df
  ori <- df %>%
    group_by(Id, ThreadId) %>%
    slice(1) %>%
    select(-Year,-UniqueContributors,-PreviousCitations,-Limited_Information,-AcademicHierarchyStrict,
          -DebateSize,-Female,-Academic,-HavePhD,-PhD_Year,-PhD_Institution_SR_Bin,-Workplace_SR_Bin,
          -PhD_Institution_US_IR_Bin,-Workplace_US_IR_Bin,-Total_Citations,-H_Index,-i10_Index)

  tmp = left_join(tmp, ori, by = c("Id" = "Id", "ThreadId" = "ThreadId"))

  return(tmp)
}

Academics_IRL = df %>%
  filter(Type == 2, Live == 1, UniqueContributors > 1) %>%
  wrangle %>%
  mutate(Loc = 1)

Academics_Online <- df %>%
  filter(Type == 2, Live == 0, UniqueContributors > 1) %>%
  wrangle%>%
  mutate(Loc = 2)

df<-rbind(Academics_IRL, Academics_Online)

# --- (Unit) custom_A23
df <- df %>% 
  group_by(Id, AcademicHierarchyStrict, Female) %>% 
  # Academic Status might change, so taking the mean. This is not ideal though.
  mutate(NumCharacters = mean(NumCharacters)) %>%
  slice(1) %>%
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

# --- (Model) kendall
model = cor.test(df${{IV}}, df${{DV}}, method = 'kendall')
model

# calculate z score
result <- tidy(model, conf.int = TRUE) %>%
  mutate(
    z = statistic
  )

# --- (Model) spearman
model=cor.test(df${{DV}}, df${{IV}}, method = 'spearman')
model

result <- tidy(model, conf.int = TRUE) %>%
  mutate(
    z = qnorm(p.value),
    z = ifelse(sign(z)==sign(estimate), z, -z)
  )

# --- (Model) anova
model = aov({{DV}} ~ {{IV}}, data = df)
summary(model)

result = tidy(model, conf.int = TRUE) %>%
  filter(term == '{{IV}}') %>%
  mutate(
    z = qnorm(p.value),
    z = abs(z)  # I don't know how to handle the sign of the z ...
  )

# --- (O)
# output
write_csv(result, '../results/estimate_{{_n}}.csv')
