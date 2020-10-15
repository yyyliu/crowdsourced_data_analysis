# For the complete code transcription, please check this link:
# https://docs.google.com/document/d/1Ezdls-z78FJxVIGCZlgPpTldQ4w0gWo0j9eJ3ddvneA/edit


library(readr)
library(dplyr)
library(psych)
library(lme4)
library(lmerTest)

edge1_1 <- read_csv("../data/edge1.1_anonymized.csv")

Conversations<-filter(edge1_1, Type == 2, Live == 1, UniqueContributors > 1)
Ids<-as.numeric(row.names(table(Conversations$Id_num)))

Person_x_thread_Data<-as.data.frame(matrix(0,0,22))

for (i in 1:length(Ids)){
  Person  <- Ids[i]
  Convos<-filter(Conversations, Id_num ==  Person )
  Threads<-as.numeric(row.names(table(Convos$ThreadId)))
  for (j in 1:length(Threads)){
    Person_x_thread_Data_temp<-as.data.frame(matrix(0,1,22))########
    
    conv<-filter(Conversations, ThreadId ==  Threads[j] )
    Person_x_thread_Data_temp[1,1]<-Person  # Id
    Person_x_thread_Data_temp[1,2]<-Threads[j] #Thread Id
    conv_Person<-filter(conv, Id_num == Person)
    if(conv_Person$Order[1] == 1){Person_x_thread_Data_temp[1,3]<-1} #Were they the lead? 
    
    Person_x_thread_Data_temp[1,4]<-dim(conv_Person)[1]/dim(conv)[1] #Fraction of total comments this person made
    Person_x_thread_Data_temp[1,5]<-sum(conv_Person$Number.Characters) / sum(conv$Number.Characters) #Fraction of total characters from this person
    Person_x_thread_Data_temp[1,6]<-conv_Person$Year[1] #Year
    Person_x_thread_Data_temp[1,7]<-length(as.numeric(row.names(table(conv$Id_num)))) #Total number of people in the conversation
    Person_x_thread_Data_temp[1,8]<-dim(conv)[1] #Total number of comments made
    Person_x_thread_Data_temp[1,9]<-conv_Person$Female[1] #Female
    Person_x_thread_Data_temp[1,10]<-conv_Person$Academic[1] #Academic
    Person_x_thread_Data_temp[1,11]<-conv_Person$HavePhD[1] #HavePhD
    Person_x_thread_Data_temp[1,12]<-conv_Person$PhD_Year[1] #PhD_Year 
    Person_x_thread_Data_temp[1,13]<-conv_Person$PhD_Institution_SR_Bin [1] #PhD Inst SR 
    Person_x_thread_Data_temp[1,14]<-conv_Person$Workplace_SR_Bin[1] #PhD Work SR 
    Person_x_thread_Data_temp[1,15]<-conv_Person$PhD_Institution_US_IR_Bin[1] #PhD Inst US 
    Person_x_thread_Data_temp[1,16]<-conv_Person$Workplace_US_IR_Bin[1] #PhD Work US 
    Person_x_thread_Data_temp[1,17]<-conv_Person$Total_Citations[1] #Total_Citations 
    Person_x_thread_Data_temp[1,18]<-conv_Person$H_Index[1] #H_Index
    Person_x_thread_Data_temp[1,19]<-conv_Person$i10_Index[1] #i10_index
    Person_x_thread_Data_temp[1,20]<-conv_Person$AcademicHierarchyStrict[1] #AcademicHierarchyStrict 
    Person_x_thread_Data_temp[1,21]<-conv_Person$PreviousCitations[1] #PreviousCitations  
    Person_x_thread_Data_temp[1,22]<-conv_Person$Limited_Information[1] #PreviousCitations 
    
    Person_x_thread_Data<-rbind(Person_x_thread_Data,Person_x_thread_Data_temp)
  }
}
colnames(Person_x_thread_Data)<-c("Id_num","ThreadId","Lead","FracTimesSpoke","FracCharSpoke","Year","UniqueContributors",
                        "DebateSize","Female","Academic","HavePhD","PhD_Year","PhD_Institution_SR_Bin","Workplace_SR_Bin",
                        "PhD_Institution_US_IR_Bin","Workplace_US_IR_Bin","Total_Citations","H_Index","i10_index","AcademicHierarchyStrict",
                        "PreviousCitations","Limited_Information")

##Identify people with some reasonable amount of data on them
Person_x_thread_Data_filtered<-filter(Person_x_thread_Data, Limited_Information==0, FracTimesSpoke < 1, !is.na(Female))
Person_x_thread_Data_filtered[Person_x_thread_Data_filtered == "Not Available" ]<-NA

Academics<-filter(Person_x_thread_Data_filtered, Academic == 1,  !is.na(PhD_Year),  !is.na(AcademicHierarchyStrict))
Academics$PhD_Year<-as.numeric(Academics$PhD_Year)
Academics2<-apply( X =Academics[,4:22],FUN = scale, center = TRUE, scale = TRUE ,MARGIN = 2)
Academics$PhD_Year<-Academics$PhD_Year* -1

Academics[,4:22]<-Academics2
Academics$Id_num2 <- as.factor(Academics$Id_num)
Academics$ThreadId2 <- as.factor(Academics$ThreadId)

Spoke_pca<-prcomp(x =  cbind(Academics$FracCharSpoke, Academics$FracTimesSpoke), retx = TRUE, center = TRUE,
                  scale. =  TRUE  )
Academics$Spoke_pca1<-Spoke_pca$x[,1]

Status_pca<-prcomp(x =  cbind(Academics$PhD_Year, Academics$AcademicHierarchyStrict), retx = TRUE, center = TRUE,
                  scale. =  TRUE  )
Academics$Status_pca1<-Status_pca$x[,1]
Academics_IRL<-Academics

##Comments#########
Online_Conversations<-filter(edge1_1, Type == 2, Live == 0, UniqueContributors > 1)
Ids<-as.numeric(row.names(table(Online_Conversations$Id_num)))

Person_x_thread_Data<-as.data.frame(matrix(0,0,22))

for (i in 1:length(Ids)){
  Person  <- Ids[i]
  Convos<-filter(Online_Conversations, Id_num ==  Person )
  Threads<-as.numeric(row.names(table(Convos$ThreadId)))
  for (j in 1:length(Threads)){
    Person_x_thread_Data_temp<-as.data.frame(matrix(0,1,22))########
    
    conv<-filter(Online_Conversations, ThreadId ==  Threads[j] )
    Person_x_thread_Data_temp[1,1]<-Person  # Id
    Person_x_thread_Data_temp[1,2]<-Threads[j] #Thread Id
    conv_Person<-filter(conv, Id_num == Person)
    if(conv_Person$Order[1] == 1){Person_x_thread_Data_temp[1,3]<-1} #Were they the lead? 
    
    Person_x_thread_Data_temp[1,4]<-dim(conv_Person)[1]/dim(conv)[1] #Fraction of total comments this person made
    Person_x_thread_Data_temp[1,5]<-sum(conv_Person$Number.Characters) / sum(conv$Number.Characters) #Fraction of total characters from this person
    Person_x_thread_Data_temp[1,6]<-conv_Person$Year[1] #Year
    Person_x_thread_Data_temp[1,7]<-length(as.numeric(row.names(table(conv$Id_num)))) #Total number of people in the conversation
    Person_x_thread_Data_temp[1,8]<-dim(conv)[1] #Total number of comments made
    Person_x_thread_Data_temp[1,9]<-conv_Person$Female[1] #Female
    Person_x_thread_Data_temp[1,10]<-conv_Person$Academic[1] #Academic
    Person_x_thread_Data_temp[1,11]<-conv_Person$HavePhD[1] #HavePhD
    Person_x_thread_Data_temp[1,12]<-conv_Person$PhD_Year[1] #PhD_Year 
    Person_x_thread_Data_temp[1,13]<-conv_Person$PhD_Institution_SR_Bin [1] #PhD Inst SR 
    Person_x_thread_Data_temp[1,14]<-conv_Person$Workplace_SR_Bin[1] #PhD Work SR 
    Person_x_thread_Data_temp[1,15]<-conv_Person$PhD_Institution_US_IR_Bin[1] #PhD Inst US 
    Person_x_thread_Data_temp[1,16]<-conv_Person$Workplace_US_IR_Bin[1] #PhD Work US 
    Person_x_thread_Data_temp[1,17]<-conv_Person$Total_Citations[1] #Total_Citations 
    Person_x_thread_Data_temp[1,18]<-conv_Person$H_Index[1] #H_Index
    Person_x_thread_Data_temp[1,19]<-conv_Person$i10_Index[1] #i10_index
    Person_x_thread_Data_temp[1,20]<-conv_Person$AcademicHierarchyStrict[1] #AcademicHierarchyStrict 
    Person_x_thread_Data_temp[1,21]<-conv_Person$PreviousCitations[1] #PreviousCitations  
    Person_x_thread_Data_temp[1,22]<-conv_Person$Limited_Information[1] #PreviousCitations 
    
    Person_x_thread_Data<-rbind(Person_x_thread_Data,Person_x_thread_Data_temp)
  }
}
colnames(Person_x_thread_Data)<-c("Id_num","ThreadId","Lead","FracTimesSpoke","FracCharSpoke","Year","UniqueContributors",
                                  "DebateSize","Female","Academic","HavePhD","PhD_Year","PhD_Institution_SR_Bin","Workplace_SR_Bin",
                                  "PhD_Institution_US_IR_Bin","Workplace_US_IR_Bin","Total_Citations","H_Index","i10_index","AcademicHierarchyStrict",
                                  "PreviousCitations","Limited_Information")

##Identify people with some reasonable amount of data on them
Person_x_thread_Data_filtered<-filter(Person_x_thread_Data, Limited_Information==0, FracTimesSpoke < 1, !is.na(Female))
Person_x_thread_Data_filtered[Person_x_thread_Data_filtered == "Not Available" ]<-NA

Academics<-filter(Person_x_thread_Data_filtered, Academic == 1,  !is.na(PhD_Year),  !is.na(AcademicHierarchyStrict))
Academics$PhD_Year<-as.numeric(Academics$PhD_Year)
Academics2<-apply( X =Academics[,4:22],FUN = scale, center = TRUE, scale = TRUE ,MARGIN = 2)
Academics$PhD_Year<-Academics$PhD_Year* -1

Academics[,4:22]<-Academics2
Academics$Id_num2 <- as.factor(Academics$Id_num)
Academics$ThreadId2 <- as.factor(Academics$ThreadId)

Spoke_pca<-prcomp(x =  cbind(Academics$FracCharSpoke, Academics$FracTimesSpoke), retx = TRUE, center = TRUE,
                  scale. =  TRUE  )
Academics$Spoke_pca1<-Spoke_pca$x[,1]

Status_pca<-prcomp(x =  cbind(Academics$PhD_Year, Academics$AcademicHierarchyStrict), retx = TRUE, center = TRUE,
                   scale. =  TRUE  )
Academics$Status_pca1<-Status_pca$x[,1]
Academics_Online<-Academics

####################
Academics_IRL$Loc<-1
Academics_Online$Loc<-2

Academics_final<-rbind(Academics_IRL,Academics_Online)
Academics_final$Id_num2 <- as.factor(Academics_final$Id_num)
Academics_final$ThreadId2 <- as.factor(Academics_final$ThreadId)

mod3<-lmer(Spoke_pca1 ~ Status_pca1 + Lead +  Year  + Female + Loc + (Loc):(Status_pca1 + Lead +  Year  + Female )  +  (1|Id_num2) + (1|ThreadId2) ,
           data =  Academics_final)
summary(mod3) 
