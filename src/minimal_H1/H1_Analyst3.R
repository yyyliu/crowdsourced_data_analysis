# For the complete code transcription, please check this link: 
# https://docs.google.com/document/d/15-RVmSY00-Ov7Qe2vq14hYEQnP0AKjJE22NiVtEF9iA/edit

#'##Data preparation
#'####Required R libraries
#rm(list = ls())
require(lme4)
require(piecewiseSEM)
require(influence.ME)

#'####Reading in the data
data <- read.csv('../data/edge1.1_anonymized.csv')

# filtering
data = subset(data, Female == 1 & DebateSize > 1)

#'####Computing mean number of contributions per thread, avoiding division by zero
data$ContrPerThread <- data$PreviousContributions/(data$PreviousThreads + 0^data$PreviousThreads)

#'###Rescaling variables
#'Rescaling Unique Female Participation by a factor of 10. Now representing 10% units.
data$UniqueFemaleParticipation <- data$UniqueFemaleParticipation * 10

#'###Defining measurement levels
#'####Defining variables as ordered or unordered factors
data$Female <- as.factor(data$Female)
data$Id_num <- as.factor(data$Id_num)
data$ThreadId <- as.factor(data$ThreadId)

#'##Hypothesis testing
#'###Hypothesis 1
#'**A woman’s tendency to participate actively in the conversation correlates positively with the number of females in the discussion.**

#'Adding random slope for Unique Female Participation. Effect of unique female participation on Contributions by author now allowed to vary accross subjects.
H1glmm3 <- glmer(ContributionsbyAuthor ~ 1 + ContrPerThread + UniqueFemaleParticipation + (1 + UniqueFemaleParticipation | Id_num) + (1 | ThreadId),
                 data, family="poisson", nAGQ = 1)

summary(H1glmm3)
# rsquared(H1glmm3)
