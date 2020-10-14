# For the complete code transcription, please check this link:
# https://docs.google.com/document/d/1MsVPDCl2W70mATgbcvEbnzVwist2HjySSq7x6HvsOII/edit

library(data.table)

edge1_1 <- read.csv('../data/edge1.1_anonymized.csv')
data <- data.table(edge1_1)

# Prepare data for gender hypothesis testing
data_gender <- aggregate(data, by = list(data$Link), FUN = mean)
data_gender <- data.table(data_gender)

data_gender <- data_gender[!(UniqueFemaleParticipation == 0.0)]
data_gender <- data_gender[!(UniqueFemaleParticipation == 1.0)]

data_gender[, female_power:= Female_Contributions/UniqueFemaleContributors]

# Correlation test 
cor.test(data_gender$UniqueFemaleParticipation, data_gender$female_power, method = "kendall")
