# For the complete code transcription, please check this link:
# https://docs.google.com/document/d/1waNsZLM-VL9b14rrsVLrNxeXSoboo-KaHgmsKp0NSqY/edit

library(tidyverse)
library(lme4)
edge <- read.csv('../data/edge1.1_anonymized.csv', stringsAsFactors = FALSE)

# Status is tricky to operationalize. Let's define it by a linear combination of various
# variable obtained by factor analysis

# Building up the 'status' variable
z. <- function(X){
  (X - mean(X, na.rm=T))/sd(X, na.rm=T)
}
ind <- edge$Id

phd <- edge$HavePhD
phd[is.na(phd)] <- 0 # Assumes no info equals no PhD
phd <- tapply(phd, ind, mean)

academic <- as.numeric(factor(edge$Academic))
academic[is.na(academic)] <- 0 # Assumes no info equals not academic
academic <- tapply(academic, ind, mean)

phdBin <- edge$PhD_Institution_US_IR_Bin
phdBin[is.na(phdBin)] <- 0
phdBin <- tapply(phdBin, ind, mean)
phdBin <- z.(phdBin)

workBin <- edge$Workplace_US_IR_Bin
workBin[is.na(workBin)] <- 0
workBin <- tapply(workBin, ind, mean)
workBin <- z.(workBin)

threadPart <- tapply(edge$ThreadId, edge$Id_num, function(x) length(unique(x))) # How many threads in total?
threadPart <- z.(threadPart)

# Citation indexes correlates highly with total citations
citation <- edge$Total_Citations
citation[is.na(citation)] <- 0 # Assumes no info equals no citations
citation <- tapply(citation, ind, mean)
citation <- z.(citation)

acadHier <- edge$AcademicHierarchyStrict
acadHier[is.na(acadHier)] <-0 # Again
acadHier <- tapply(acadHier, ind, mean)
acadHier <- z.(acadHier)

statusVar <- data.frame(phd, academic, phdBin, workBin, threadPart, citation, acadHier)

# 'loadings' weights come from factor analysis
loadings <- as.numeric(factanal(statusVar, factors=1)$loadings)
status <- apply(statusVar, 1, function(x) sum(x * loadings)/length(loadings))
status <- status[as.numeric(factor(edge$Id))]
status <- z.(status)

hyp2df = edge %>% add_column(status = status)

# Use standardized status for easier interpratation
M1 <- (lmer(log(WC)~Role + Female + Type + status + (1|ThreadId) + (1|Id), data=hyp2df))
summary(M1) 
