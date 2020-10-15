# For the complete code transcription, please check this link:
# https://docs.google.com/document/d/17n3DfU84fO3YptXe_UkFlaLBoUFrcLLrA3RN9gHOMME/edit?usp=sharing

library('readr')
library('lme4')
library('dplyr')

# Load in data
edge_table = read_csv('../data/edge1.1_anonymized.csv')

# Filter out people with many contributions (Edge staff)
reduced.data = group_by(edge_table, Id) %>% 
  summarize(Contribs=n()) %>%
  filter(Contribs < 100)
            
status.table = inner_join(reduced.data, edge_table)  %>%
  select(WPS, Id, Live, Female, Academic, Job_Title_S, Years_from_PhD, 
         Total_Citations, H_Index, AcademicHierarchyStrict) %>%
  mutate(Log.Citations=log(Total_Citations))

# Scale continuous variables
status.table$WPS = scale(status.table$WPS)
status.table$Years_from_PhD = scale(status.table$Years_from_PhD)
status.table$Log.Citations = scale(status.table$Log.Citations)
status.table$H_Index = scale(status.table$H_Index)

# Need to treat academics and non-academics differently.
status.academic = filter(status.table, Academic==1) %>% 
  select(-Job_Title_S, -Academic)

# Fit a random effects model with Gaussian link function
acad.model = lmer(WPS ~ Female + Years_from_PhD + Log.Citations + H_Index + AcademicHierarchyStrict + (1|Live),
                    data=status.academic)

summary(acad.model)
