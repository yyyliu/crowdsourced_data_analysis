# For the complete code transcription, please check this link:
# https://docs.google.com/document/d/1zSfeBoGag1nc5Obg5QQ7qSZ4rQvzipRuGEEHD8givv4/edit

#Test hypothesis 1: "A woman’s tendency to participate actively in the conversation correlates positively with the number of females in the discussion".

#Consider making this into a function?
library(tidyverse)
dat <-read_csv('../data/edge1.1_anonymized.csv')

#One thing to try: for each individual woman in the dataset, calculate an average number of comments per discussion that she's in.
#Then, for those women, calculate how that summary statistic relates to number of women in a conversation.
women <- dat %>% filter(Female == 1) %>% 
  select(Title, Name, Id, PreviousContributions, PreviousThreads, ContributionsThisYear, ThreadsThisYear) %>%
  group_by(Title, Id) %>% 
  summarise(comments_now = n(),
            PreviousContributions = mean(PreviousContributions),
            PreviousThreads = mean(PreviousThreads),
            cont_this_year = mean(ContributionsThisYear),
            threads_this_year = mean(ThreadsThisYear)) 
women <- women %>%
  mutate(total_previous_contributions = PreviousContributions + cont_this_year,
         total_previous_threads = PreviousThreads + threads_this_year)

#For each woman, get the number of contributions relative to her mean previous contributions.
women <- women %>% 
  mutate(cont_per_thread = total_previous_contributions/total_previous_threads) %>%
  mutate(comments_now_percent_change = (comments_now - cont_per_thread)*100/cont_per_thread)

#Now, for each conversation, get the number and percent of females. 
number_of_women <- dat %>% 
  group_by(Title) %>%
  slice(1) %>%
  dplyr::select(Title, UniqueFemaleContributors, UniqueFemaleParticipation)

#Join data for regression.
reg_dat <- left_join(women, number_of_women, by = "Title")

#that individual participated very actively in one discussion after previously "lurking" 8 times. 
#Redo regression without her.
fit2 <- lm(comments_now_percent_change ~ UniqueFemaleContributors, data = reg_dat[-325,])
summary(fit2)
