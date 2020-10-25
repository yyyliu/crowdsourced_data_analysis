# For the complete code transcription, please check this link:
# https://docs.google.com/document/d/1Ezdls-z78FJxVIGCZlgPpTldQ4w0gWo0j9eJ3ddvneA/edit


library(readr)
library(dplyr)
library(psych)
library(lme4)
library(lmerTest)

wrangle <- function (df) {
  tmp <- df %>%
    group_by(ThreadId) %>%
    mutate(
      debate_size = n(),
      tota_chars = sum(Number.Characters),
      unique_ids = n_distinct(Id_num),
      row_id = row_number()
    ) %>%
    ungroup %>%
    group_by(Id_num, ThreadId) %>%
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
    select(Id_num,ThreadId,Lead,FracTimesSpoke,FracCharSpoke,Year,UniqueContributors,
          DebateSize,Female,Academic,HavePhD,PhD_Year,PhD_Institution_SR_Bin,Workplace_SR_Bin,
          PhD_Institution_US_IR_Bin,Workplace_US_IR_Bin,Total_Citations,H_Index,i10_Index,AcademicHierarchyStrict,
          PreviousCitations,Limited_Information) %>%
    filter(Limited_Information==0, FracTimesSpoke < 1, !is.na(Female))

  tmp[tmp == 'Not Available'] <- NA

  tmp <- tmp %>%
    filter(Academic == 1, !is.na(PhD_Year), !is.na(AcademicHierarchyStrict)) %>%
    mutate(PhD_Year = as.numeric(PhD_Year))

  scaled<-apply( X =tmp[,4:22],FUN = scale, center = TRUE, scale = TRUE ,MARGIN = 2)
  tmp[,4:22]<-scaled

  tmp <- tmp %>%
    mutate(
      SpokePCA = prcomp(x=cbind(tmp$FracCharSpoke, tmp$FracTimesSpoke), retx=T, center=T, scale.=T)$x[,1],
      StatusPCA = prcomp(x=cbind(tmp$PhD_Year, tmp$AcademicHierarchyStrict), retx=T, center=T, scale.=T)$x[,1]
    )

  return(tmp)
}

edge1_1 <- read_csv("../data/edge1.1_anonymized.csv")

Academics_IRL = edge1_1 %>%
  filter(Type == 2, Live == 1, UniqueContributors > 1) %>%
  wrangle %>%
  mutate(Loc = 1)

Academics_Online <- edge1_1 %>%
  filter(Type == 2, Live == 0, UniqueContributors > 1) %>%
  wrangle %>%
  mutate(Loc = 2)

Academics_final<-rbind(Academics_IRL,Academics_Online)

model<-lmer(SpokePCA ~ StatusPCA + Lead +  Year  + Female + Loc + (Loc):(StatusPCA + Lead +  Year  + Female )  +  (1|Id_num) + (1|ThreadId) ,
           data =  Academics_final)
summary(model) 
