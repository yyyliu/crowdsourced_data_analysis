# For the complete code transcription, please check this link:
# https://docs.google.com/document/d/1_2dNqZd6UQqb3EwsZ_dlfWIGiTIv9O_yfWmjoRA39po/edit

#load necessary packages
library(dplyr)

#read in data set
edge <- read.csv('../data/edge1.1_anonymized.csv')

# remove NA values
edge <- edge[complete.cases(edge), ]

# the number of times a woman speaks in a specific conversation = "Female_Contributions"
# number of females in discussion = "Female" (1 = Female, 0 = Male)
# percentage of comments made by a woman = "FemaleParticipation" 

# Let's select columns only  related to the first hypothesis
female <- c("Female_Contributions", "Female", "FemaleParticipation")
edge.1hyp <- edge[, female]

#remove all NA rows
edge.1 <- edge.1hyp[complete.cases(edge.1hyp), ]

# Let's remove all outliers from data 
# Through calculation from summary(edge.1$FemaleParticipation)
# and summary(edge.1$Female_Contributions), I got outlier values.
edge.1 <- edge.1 %>% 
  filter(FemaleParticipation < 0.20, Female_Contributions <= 22.5)

# get the correlation between Female Contributions and Female Participation
edge.1 %>%
  summarize(r = cor(Female_Contributions, FemaleParticipation,
                    use = "pairwise.complete.obs"))
