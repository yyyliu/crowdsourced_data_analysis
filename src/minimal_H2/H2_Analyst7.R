# For the complete code transcription, please check this link:
# https://docs.google.com/document/d/1uOEuHjdjuxNiqb7C6sMkoH3TR0ETYhbnJlZnhJUEkXA/edit

library(dplyr)

df <- read.csv("../data/edge1.1_anonymized.csv")

### Hypothesis no 2 ###

df <- df %>%
  mutate(log_cit = log10(Citations_Cumulative),
         log_char = log10(Number.Characters)) 

m4 <- lm(log_char ~ AcademicHierarchyStrict + Discipline + 
           log_cit, data = df)
summary(m4)
