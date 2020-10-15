# For the complete code transcription, please check this link:
# https://docs.google.com/document/d/1GNyp9hRX3hkGKdMf7ViRfHpB1uau7jZaR5e5imqu8jE/edit

library(readr)
library(tidyverse)
mydata <- read_csv("../data/edge1.1_anonymized.csv")

mydata <- mutate(mydata, log_num_char=log(mydata$Number.Characters))

d <- dplyr::select(mydata, log_num_char, Academic, Female, PhD_Institution_US_Bin:PreviousCitations)
d <- d %>% mutate(factor_academic=as.factor(Academic), factor_female=as.factor(Female), factor_aca_hier=as.factor(AcademicHierarchyStrict))

reg2 <- lm(log_num_char~AcademicHierarchyStrict+Female+Academic, data=d)
summary(reg2)
