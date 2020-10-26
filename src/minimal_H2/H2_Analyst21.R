# For the complete code transcription, please check this link:
# https://docs.google.com/document/d/1MAKbGDrQNSoK3wKW-t6v_2qiylAcIfOUMByK9Jv0K8U/edit?usp=sharing


suppressPackageStartupMessages(library(tidyverse))
suppressPackageStartupMessages(library(ez))
suppressPackageStartupMessages(library(heplots))

countWords = function(x) {
  length(unlist(strsplit(as.character(x), "\\W+")))
}

d <- read.csv("../data/edge1.1_anonymized.csv")

dd2 = d %>%
  rowwise() %>%
  mutate(nWords = countWords(Text)) %>%
  ungroup %>%
  group_by(Id) %>%
  mutate(
    ContrTotal = mean(nWords)
  ) %>%
  slice(1) %>%
  ungroup

dd2$AcH2 <- ordered(dd2$AcademicHierarchyStrict)

# Assumption of normality. However, we have ties so this will not work either way.
an <- ezANOVA(data = dd2, dv = ContrTotal, between = AcH2, wid = Id, return_aov = TRUE)
etasq(an$aov)
# an["ANOVA"] # Uncomment this line to get the p-value - added by the reviewer
