# For the complete code transcription, please check this link:
# https://docs.google.com/document/d/16cX90XoZe0MVjWJF8xn_ZVRuuWj6m0pD_D6t9xSOaMw/edit

library('readr')
library('lme4')
library('dplyr')

# For each female participant, need to find out if 
# active in conversation or not.
# This is probably the crucial step that will change the 
# analysis results. As I have no prior experience about how
# to determine active participation, I will naively use the 
# number of contributions as a proxy.

# Load in data             
edge_table = read_csv('../data/edge1.1_anonymized.csv')

# First, retrieve number of contributions to each conversation for
# female participants
particip.data = filter(edge_table, Female==1) %>%
  group_by(ThreadId, Id) %>%
  summarize(ContributionsbyAuthor=unique(ContributionsbyAuthor),
            UniqueFemaleContributors=unique(UniqueFemaleContributors))

# Fit a random effects model with poisson link function to model
# the (count-data) outcome given the dependent variable
glmer.model = glmer(ContributionsbyAuthor ~ UniqueFemaleContributors + (1|ThreadId),
                    data=particip.data,
                    family=poisson)
# Print summary
summary(glmer.model) ##VAISHALI COMMENT: model used by analyst to report regression coefficient
