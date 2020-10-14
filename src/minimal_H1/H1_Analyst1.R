# For the complete code transcription, please check this link:
# https://docs.google.com/document/d/1JJd8ftXAM8OC76UIz_uMF28lniaxEUgwRoMpqIDLHPY/edit

library(readr)
library(tidyverse)
mydata <- read_csv('../data/edge1.1_anonymized.csv')
df <- dplyr::select(mydata, ThreadId:UniqueFemaleParticipation, Id_num, Female, Order)

d <- df %>% filter(Female_Contributions==UniqueFemaleContributors) %>% 
  group_by(ThreadId) %>%
  arrange(Order) %>%
  mutate(females_in_discussion=cumsum(Female), next_female =lead(Female)) %>%
  ungroup()

#logistic regression model
model <- glm (next_female ~ females_in_discussion, data = d, family = binomial)
summary(model)

#odds ratio and confidence interval for the overall model
exp(coef(model))
