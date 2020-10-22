# For the complete code transcription, please check this link:
# https://docs.google.com/document/d/1bCDP9toRaC6bquWaiUbe9AzFOyhwgyB02LlmxSjIyWw/edit

#'####Required R libraries
require(lme4)

#'####Reading in the data
data <- read.csv("../data/edge1.1_anonymized.csv")

#'####Computing mean workplace rank
data$WorkplaceMeanRank <- rowMeans(data[,c("Workplace_SR_Bin", "Workplace_US_IR_Bin")], na.rm = TRUE)

#'###Rescaling variables
data$Total_Citations <- data$Total_Citations/1000
data$WC <- data$WC/100

#'###Defining measurement levels
data$WorkplaceMeanRank <- ordered(data$WorkplaceMeanRank)
data$AcademicHierarchyStrict <- ordered(data$AcademicHierarchyStrict)

#'Handling missing values by listwise deletion + filtering out non-conversations (single-authored threads)
# data.na.rm = data[!is.na(data$AcademicHierarchyStrict) & !is.na(data$WorkplaceMeanRank) & !is.na(data$Total_Citations) & data$DebateSize > 1,]

# it does not matter to drop NA because lmer will drop them anyways
data.na.rm = data[data$DebateSize > 1,]

#'Adding random slope for Total Citations. Effect of Total Citations on Word Count now allowed to vary accross threads
H2lmm4 <- lmer(WC ~ 1 + WorkplaceMeanRank + AcademicHierarchyStrict + Total_Citations + (1 | Id_num) + (1 + Total_Citations | ThreadId), data.na.rm, REML = TRUE)
summary(H2lmm4)
