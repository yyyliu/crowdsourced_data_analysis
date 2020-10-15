# For the complete code transcription, please check this link:
# https://docs.google.com/document/d/1-eJCRFQk_UKChWZAAUw4PR4ljS1PSaqVfWv7Q_Gc4vc/edit

# data import -------------------------------------------------------------
library(tidyverse)
library(magrittr)
library(tidytext)
library(forcats)

edge <- read_csv("../data/edge1.1_anonymized.csv")

# create a person level data frame
# but, ensure that no two-author pieces are slipping in
# these are really rare, and they seem substantively different from posts by
# individuals
persons <- edge %>% 
  filter(
    TwoAuthors != 1,
    !is.na(Male)
  ) %>% 
  select(
    ThreadId,
    Id_num,
    Academic,
    Male,
    Discipline,
    PreviousCitations,
    H_Index,
    AcademicHierarchyStrict,
    Workplace_US_Bin,
    Text
  ) %>% 
  distinct

# this gets us an observation for each word-- super large data frame
p_words <- persons %>% 
  group_by(Id_num, ThreadId) %>% 
  mutate(comment_num = row_number()) %>% 
  ungroup %>% 
  unnest_tokens(word, Text)

num_words <- count(p_words, Id_num, ThreadId, comment_num)

num_words_analytic <- num_words %>%
  group_by(Id_num) %>%
  summarize(
    med_words  = median(n),
    mean_words = mean(n),
    sd_words   = sd(n)
  ) %>%
  left_join(
    edge %>%
      select(
        Id_num,
        Academic,
        Male,
        Discipline,
        AcademicHierarchyStrict,
        Discipline,
        USA_Ranking_Dif,
        Total_Citations
      ) %>%
      distinct %>%
      mutate(
        disc = ifelse(is.na(Discipline), "missing", Discipline),
        disc = factor(disc) %>% relevel(ref = "Social Sciences")
      ),
    by = c("Id_num")
  )

num_words_analytic <- left_join(
  num_words_analytic,
  edge %>%
    select(Id_num, H_Index, i10_Index) %>%
    distinct,
  by = "Id_num"
)

num_words_analytic$AcademicHierarchyStrict <- factor(
  num_words_analytic$AcademicHierarchyStrict,
  levels = c(5, 1:4, 6),
  labels = c("Professor", "Graduate Student", "Postdoc", "Asst. Prof",
             "Assoc. Prof", "Chaired Prof")
)

num_words_analytic$Male <- factor(
  num_words_analytic$Male,
  levels = 0:1,
  labels = c("Female", "Male")
)

num_words_analytic$Academic <- factor(
  num_words_analytic$Academic,
  levels = 0:1,
  labels = c("non-academic", "academic")
)

num_words_mod4b <- lm(
  mean_words ~
    disc +
    Male +
    AcademicHierarchyStrict,
  data = num_words_analytic %>%
    filter(mean_words <= 3000) %>%
    mutate(
      AcademicHierarchyStrict =
        as.character(AcademicHierarchyStrict) %>%
        ifelse(is.na(.), "na", .) %>% 
        factor(levels = c(
          "Professor", "na", "Graduate Student", "Postdoc", "Asst. Prof",
          "Assoc. Prof", "Chaired Prof"
        ))
    )
)

summary(num_words_mod4b) 
