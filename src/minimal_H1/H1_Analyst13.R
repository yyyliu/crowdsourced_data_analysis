# For the complete code transcription, please check this link:
# https://docs.google.com/document/d/1yLnKqG0ThkPA3bXLzkNECTn3TqW6j6Y9QOAs9fTKpDw/edit

library(readr)
# library(mosaic)
library(dplyr)

edge1_1 <- read_csv('../data/edge1.1_anonymized.csv')
conversation <- edge1_1 %>%
  filter(Type ==2)

# reduce_dupes
each_convo <- conversation %>% 
  group_by(ThreadId) %>%
  summarize(
    Male_Contribs = mean(Male_Contributions),
    Female_Contribs=mean(Female_Contributions),
    Female_Partic=mean(FemaleParticipation),
    UniqueContribs = mean(UniqueContributors),
    UniqueMales = mean(UniqueMaleContributors),
    UniqueFemales = mean(UniqueFemaleContributors)
  )

m2 <- lm(Female_Partic~UniqueFemales+UniqueContribs, data=each_convo)
summary(m2)
