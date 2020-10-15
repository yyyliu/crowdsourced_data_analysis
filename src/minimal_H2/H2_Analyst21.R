# For the complete code transcription, please check this link:
# https://docs.google.com/document/d/1MAKbGDrQNSoK3wKW-t6v_2qiylAcIfOUMByK9Jv0K8U/edit?usp=sharing


suppressPackageStartupMessages(library(tidyverse))
suppressPackageStartupMessages(library(ez))
suppressPackageStartupMessages(library(heplots))

d <- read.csv("../data/edge1.1_anonymized.csv")

#' Second hypothesis
d <- cbind(d, nWords = d %>% dplyr::select(Text) %>% as.data.frame() %>% apply(1, function(x)  length(unlist(strsplit(as.character(x), "\\W+"))))) # edited by the reviewer for the code to run

d2 <- d %>% group_by(Id) %>% summarize(ContrTotal = mean(nWords)) 
d2$AcH <- NA
for ( i in 1:nrow(d2)){
  d2$AcH[i] <- d[which(d$Id == d2$Id[i]), ] %>% dplyr::select(AcademicHierarchyStrict) %>% slice(1) %>% unlist() %>% as.numeric()
}

dd2 <- d2 %>% drop_na() 
dd2$AcH2 <- ordered(dd2$AcH)

# Assumption of normality. However, we have ties so this will not work either way.
an <- ezANOVA(data = dd2, dv = ContrTotal, between = AcH2, wid = Id, return_aov = TRUE)
etasq(an$aov)
# an["ANOVA"] # Uncomment this line to get the p-value - added by the reviewer
