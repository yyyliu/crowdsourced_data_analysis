# For the complete code transcription, please check this link:
# https://docs.google.com/document/d/18KT1hnO2PLrH70lSDP7jxbP3-OWu5BW1QkYekA7Xmo8/edit

#added libraries used:
library(dplyr)
library(effsize)

# Data from edge, effectively a convience sample of "communication" or a cenus of the site, depending perspective
edge <- read.csv('../data/edge1.1_anonymized.csv', stringsAsFactors = FALSE)

h1_data <- edge %>%
  filter(Type == 2, #communications of type conversation
         !is.na(Female), #gender is identified
         Female == 1 #gender is Female
  ) %>%
  arrange(Year, ThreadId, Id_num, Order) %>% 
  group_by(Year, ThreadId, Id_num) %>% #for each person in thread in year in order
  mutate(first_post = c(1, rep(0,n()-1)))  %>% # mark 1st post 1, others 0
  ungroup() %>%
  arrange(Year, ThreadId, Order) %>% 
  group_by(Year, ThreadId) %>% #for each thread in each year in order
  mutate(n_females = lag(cumsum(first_post)), #the cumlative sum at the entry above the 
         n_females = ifelse(is.na(n_females), 0, n_females) #current one is the number
  ) %>%                      #of females at post time, so shifted down to current
  ungroup() %>%
  group_by(Year, ThreadId, n_females) %>% #for each sequence of the same number of females
  summarise(n_posts= n()) %>% #calculate the amount of activity
  ungroup() %>%
  filter(n_females > 0) #posts with no females can only have one post from a female before
# they become posts with one female, so discarding them as there is no variation.
# with a range of numeric values and numeric outcomes, I'm doinga linear regression
h1_result <- lm(n_posts ~ n_females, data=h1_data)

### reporting 
summary(h1_result) 
