# For the complete code transcription, please check this link:
# https://docs.google.com/document/d/1qnnzBg-gs9P-dTmRPPPVm9DKeCWwGoQQIf_ojv2d92U/edit

# Loading, setting up
library(tidyverse)
df <- read_csv("../data/edge1.1_anonymized.csv")

# Analysis for RQ 2
df$phd_ranking <- ifelse(df$HavePhD == 1,
                         ifelse(!is.na(df$Workplace_SR_Bin),
                                df$Workplace_SR_Bin, "has_phd_but_not_ranked"), "does_not_have_phd")
df$phd_ranking_f <- forcats::fct_relevel(df$phd_ranking,
                                         "does_not_have_phd", "has_phd_but_not_ranked",
                                         "7", "6", "5", "4", "3", "2", "1")
df$phd_ranking_rc <- as.numeric(df$phd_ranking_f)

df_ss <- df %>% 
  group_by(Id) %>% 
  summarize(mean_WC = mean(WC, na.rm = T),
            mean_phd_ranking_rc = mean(phd_ranking_rc))

m3 <- lm(mean_WC ~ mean_phd_ranking_rc, data = df_ss)
summary(m3)
