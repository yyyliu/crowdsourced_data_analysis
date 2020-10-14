# For the complete code transcription, please check this link:
# https://docs.google.com/document/d/1ph1GsQfE_NLWcPvvEBqMFmTOXoIxT_cLzAGSRZyZjZ4/edit

#corelation
#figuring out how a womenś tendency to participate acitively in the conversation 
#corelates negatively with the numbers of females/males in the discussion
edge1_1 <- read.csv('../data/edge1.1_anonymized.csv')
cor(edge1_1$UniqueFemaleContributors, edge1_1$Female_Contributions, use = "all.obs", method = "pearson") 
