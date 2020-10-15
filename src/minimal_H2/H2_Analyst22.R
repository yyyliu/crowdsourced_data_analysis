# For the complete code transcription, please check this link:
# https://docs.google.com/document/d/1VAvsg7W7hIhfEpcu8GydX3_569X1f32y7w01ANW-CuY/edit

## Import data
library(readr)
library(reshape)

edge1_1 <- read_csv("../data/edge1.1_anonymized.csv")

# restructure dataset
contributions_by_person=cast(edge1_1, Id_num~ Year,value=c('ContributionsThisYear'))
hierarchy_by_person=cast(edge1_1, Id_num~ Year, value=c('AcademicHierarchyStrict'), fun=mean)
names(contributions_by_person)=paste0("ContributionsThisYear",names(contributions_by_person))
names(hierarchy_by_person)=paste0("AcademicHierarchyStrict",names(hierarchy_by_person))

#12-14
hierarchy_by_person$AcademicHierarchyStrict2013[is.na(hierarchy_by_person$AcademicHierarchyStrict2013)]=rowMeans(data.frame(hierarchy_by_person$AcademicHierarchyStrict2014[is.na(hierarchy_by_person$AcademicHierarchyStrict2013)],hierarchy_by_person$AcademicHierarchyStrict2012[is.na(hierarchy_by_person$AcademicHierarchyStrict2013)]),na.rm=TRUE)
contributions_by_person[contributions_by_person$ContributionsThisYear2012==0&contributions_by_person$ContributionsThisYear2013==0&contributions_by_person$ContributionsThisYear2014==0,18:20]=NA

hyp1=cor.test(y=hierarchy_by_person$AcademicHierarchyStrict2013,x=contributions_by_person$ContributionsThisYear2013, method = "spearman")
hyp1
