# For the complete code transcription, please check this link:
# https://docs.google.com/document/d/1-eJCRFQk_UKChWZAAUw4PR4ljS1PSaqVfWv7Q_Gc4vc/edit

# data import -------------------------------------------------------------
library(tidyverse)
library(tidytext)

df <- read_csv("../data/edge1.1_anonymized.csv")

tmp = df %>%
  distinct(Id_num,Academic,Discipline,AcademicHierarchyStrict,USA_Ranking_Dif, .keep_all=TRUE) %>%
  mutate(
    CustomDiscipline = ifelse(is.na(Discipline), "missing", Discipline)
  )

# this gets us an observation for each word-- super large data frame
df <- df %>% 
  filter(TwoAuthors != 1) %>% 
  select(ThreadId, Id_num, Text) %>% 
  distinct %>% 
  group_by(Id_num, ThreadId) %>% 
  mutate(comment_num = row_number()) %>% 
  ungroup %>% 
  unnest_tokens(word, Text) %>%
  count(Id_num, ThreadId, comment_num) %>%
  group_by(Id_num) %>%
  summarize(mean_words = mean(n))

df = left_join(df, tmp, by = c("Id_num"))

# 5 is "Professor", 6 is "Chaired Prof", 7 is NA
# 1 to 4 are: "Graduate Student", "Postdoc", "Asst. Prof", and "Assoc. Prof"
df = df %>%
  mutate(
    CustomHierarchy = AcademicHierarchyStrict %>%ifelse(is.na(.), 7, .),
    CustomHierarchy = factor(CustomHierarchy, levels=c(5, 7, 1:4, 6))
  )

df = df %>%
  filter(mean_words <= 3000)

num_words_mod4b <- lm(mean_words ~ CustomDiscipline + Male + CustomHierarchy,
  data = df)
summary(num_words_mod4b) 
