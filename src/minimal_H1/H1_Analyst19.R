# For the complete code transcription, please check this link:
# https://docs.google.com/document/d/1MyHyWbkR7ze3SKLAj4rJU4Cbga0UZmi2RSSqdP3AbIc/edit

df <- read.csv(file='../data/edge1.1_anonymized.csv')

# Hypothesis 1: "A woman’s tendency to participate actively in the conversation correlates positively with the number of females in the discussion."
dfwomen <- subset(df, Female == 1)

# regression model
dim(dfwomen)
fit <- lm(ContributionsbyAuthor ~ UniqueFemaleContributors + PreviousContributions + HavePhD + Total_Citations, data=dfwomen)
summary(fit)
