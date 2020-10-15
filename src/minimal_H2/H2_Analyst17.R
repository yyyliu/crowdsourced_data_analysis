# For the complete code transcription, please check this link:
# https://docs.google.com/document/d/1hzdXJFsDoO_Y0mfmLoPy-oltu6n37z1TpJn9jUwHwsA/edit

library(data.table)

edge1_1 <- read.csv("../data/edge1.1_anonymized.csv")
data <- data.table(edge1_1)

# Prepare data for 2nd hypothesis “Higher status participants are more verbose than are lower status participants.” 
data_status = data[!is.na(AcademicHierarchyStrict) == TRUE]
cor.test(data_status$AcademicHierarchyStrict, data_status$WC, method = "kendall")
