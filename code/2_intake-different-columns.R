# PROGRESS {{{ ====

# This file is for disparate columns, so we can at least identify which ones we will need to massage into a correct definition

### TO DO
	# Identify columns that are important that are "different"

# }}}

# Workspace setup {{{ ====
# Libraries (load before doing any data analysis)
library(tidyverse)

# }}}

#Intake {{{====

# Read in quickly with read_*** function
cohort1_data <- read_csv("../data/cohort1-short.csv")
cohort2_data <- read_csv("../data/cohort2-short.csv")

# }}}


#Demographics {{{====

#missing ward data from cohort 2
cohort1_data$ward<- cohort1_data$ward

#PHQ-9 and QOL {{{====

#PHQ-9 questions are only present in cohort 2


#Below all the phq codes were recoded so that it will be easy to add them and give people a total score 

#phq1 recoded
cohort2_data$phq1[cohort2_data$s7_phq1==1] <- 0 
cohort2_data$phq1[cohort2_data$s7_phq1==2] <- 1
cohort2_data$phq1[cohort2_data$s7_phq1==3] <- 2 
cohort2_data$phq1[cohort2_data$s7_phq1==4] <- 3 


#phq2 recoded
cohort2_data$phq2[cohort2_data$s7_phq2==1] <- 0 
cohort2_data$phq2[cohort2_data$s7_phq2==2] <- 1
cohort2_data$phq2[cohort2_data$s7_phq2==3] <- 2 
cohort2_data$phq2[cohort2_data$s7_phq2==4] <- 3 

#phq3 recoded
cohort2_data$phq3[cohort2_data$s7_phq3==1] <- 0 
cohort2_data$phq3[cohort2_data$s7_phq3==2] <- 1
cohort2_data$phq3[cohort2_data$s7_phq3==3] <- 2 
cohort2_data$phq3[cohort2_data$s7_phq3==4] <- 3 

#phq4 recoded
cohort2_data$phq4[cohort2_data$s7_phq4==1] <- 0 
cohort2_data$phq4[cohort2_data$s7_phq4==2] <- 1
cohort2_data$phq4[cohort2_data$s7_phq4==3] <- 2 
cohort2_data$phq4[cohort2_data$s7_phq4==4] <- 3 

#phq5 recoded
cohort2_data$phq5[cohort2_data$s7_phq5==1] <- 0 
cohort2_data$phq5[cohort2_data$s7_phq5==2] <- 1
cohort2_data$phq5[cohort2_data$s7_phq5==3] <- 2 
cohort2_data$phq5[cohort2_data$s7_phq5==4] <- 3 

#phq6 recoded
cohort2_data$phq6[cohort2_data$s7_phq6==1] <- 0 
cohort2_data$phq6[cohort2_data$s7_phq6==2] <- 1
cohort2_data$phq6[cohort2_data$s7_phq6==3] <- 2 
cohort2_data$phq6[cohort2_data$s7_phq6==4] <- 3 

#phq7 recoded
cohort2_data$phq7[cohort2_data$s7_phq7==1] <- 0 
cohort2_data$phq7[cohort2_data$s7_phq7==2] <- 1
cohort2_data$phq7[cohort2_data$s7_phq7==3] <- 2 
cohort2_data$phq7[cohort2_data$s7_phq7==4] <- 3 

#phq8 recoded
cohort2_data$phq8[cohort2_data$s7_phq8==1] <- 0 
cohort2_data$phq8[cohort2_data$s7_phq8==2] <- 1
cohort2_data$phq8[cohort2_data$s7_phq8==3] <- 2 
cohort2_data$phq8[cohort2_data$s7_phq8==4] <- 3 

#phq9 recoded
cohort2_data$phq9[cohort2_data$s7_phq9==1] <- 0 
cohort2_data$phq9[cohort2_data$s7_phq9==2] <- 1
cohort2_data$phq9[cohort2_data$s7_phq9==3] <- 2 
cohort2_data$phq9[cohort2_data$s7_phq9==4] <- 3 

#phq10 recoded
cohort2_data$phq10[cohort2_data$s7_phq10==1] <- 0 
cohort2_data$phq10[cohort2_data$s7_phq10==2] <- 1
cohort2_data$phq10[cohort2_data$s7_phq10==3] <- 2 
cohort2_data$phq10[cohort2_data$s7_phq10==4] <- 3 


#new phq_total column which is sum of phq1 to phq9 
cohort2_data$phq_total<- cohort2_data%>% select(phq1,phq2,phq3,phq4,phq5,phq6,phq7,phq8,phq9) %>% transmute(phq_total=(phq1+phq2+phq3+phq4+phq5+phq6+phq7+phq8+phq9))


#phq-9 categories 
cohort2_data$phq_cat[cohort2_data$phq_total >=0 & cohort2_data$phq_total<=4]<- 0 # no depression

cohort2_data$phq_cat[cohort2_data$phq_total >=5 & cohort2_data$phq_total<=9]<- 1 # mild depression

cohort2_data$phq_cat[cohort2_data$phq_total >=10 & cohort2_data$phq_total<=14]<- 2 # moderate depression

cohort2_data$phq_cat[cohort2_data$phq_total >=15 & cohort2_data$phq_total<=19]<- 3 # moderately severe depression

cohort2_data$phq_cat[cohort2_data$phq_total >=20 & cohort2_data$phq_total<=27]<- 4 # severe depression


#mobility
cohort2_data$mobility[cohort2_data$s8_qolmob==1]<-1

cohort2_data$mobility[cohort2_data$s8_qolmob >=2|cohort2_data$s8_qolmob<=4]<- 2 #converting slight/moderate/ severe problems in walking to 1 category to match cohort 1

cohort2_data$mobility[cohort2_data$s8_qolmob ==5]<- 3 #converting unable to walk to same category as cohort 1

cohort2_data$mobility[is.na(cohort2_data$s8_qolmob)]<- NA

cohort1_data$pd_mobility<- cohort1_data$pd_mobility

cohort2_data$pd_mobility<- cohort2_data$mobility #new codes: 1= no problems walking; 2= mild/moderate/severe problems walking 3= unable to walk 



#self care ability
cohort2_data$self_care[cohort2_data$s8_qolself==1]<-1

cohort2_data$self_care[cohort2_data$s8_qolself >=2|cohort2_data$s8_qolself<=4]<- 2 #converting slight/moderate/ severe problems in self care to 1 category to match cohort 1

cohort2_data$self_care[cohort2_data$s8_qolself ==5]<- 3 #converting unable to walk to same category as cohort 1

cohort2_data$self_care[is.na(cohort2_data$s8_qolself)]<- NA

cohort1_data$pd_selfcare<- cohort1_data$pd_selfcare

cohort2_data$pd_selfcare<- cohort2_data$self_care #new codes: 1= no problems ; 2= mild/moderate/severe problems  3= unable to take care of self

#usual activities
cohort2_data$usual_act[cohort2_data$s8_qoluat==1]<-1

cohort2_data$usual_act[cohort2_data$s8_qoluat >=2|cohort2_data$s8_qoluat<=4]<- 2 #converting slight/moderate/ severe problems in doing usual activities to 1 category to match cohort 1

cohort2_data$usual_act[cohort2_data$s8_qoluat ==5]<- 3 #converting unable to walk to same category as cohort 1

cohort2_data$usual_act[is.na(cohort2_data$s8_qoluat)]<- NA

cohort1_data$pd_usualact<- cohort1_data$pd_usualact

cohort2_data$pd_usualact<- cohort2_data$usual_act #new codes: 1= no problems ; 2= mild/moderate/severe problems  3= unable to take care of self

#pain
cohort2_data$pain[cohort2_data$s8_qoldcm==1]<-1

cohort2_data$pain[cohort2_data$s8_qoldcm ==2|cohort2_data$s8_qoldcm==3]<- 2 #converting slight and moderate pain category to match the moderate pain category in cohort 1

cohort2_data$pain[cohort2_data$s8_qoldcm ==4|cohort2_data$s8_qoldcm==5]<- 3 #converting severe and unbearable pain category to match severe pain category in cohort 1

cohort2_data$pain[is.na(cohort2_data$s8_qoldcm)]<- NA

cohort1_data$pd_pain<- cohort1_data$pd_pain

cohort2_data$pd_pain<- cohort2_data$pain #new codes: 1= no problems ; 2= mild/moderate pain 3= severe pain

#depression

cohort2_data$depression[cohort2_data$s8_qolands==1]<-1

cohort2_data$depression[cohort2_data$s8_qolands ==2|cohort2_data$s8_qoldcm==3]<- 2 #converting slight and moderate anxiety/depression category to match the moderate anxiety/depression category in cohort 1

cohort2_data$depression[cohort2_data$s8_qolands ==4|cohort2_data$s8_qolands==5]<- 3 #converting severe and extreme anxiety/depression to match severe anxiety/depression category in cohort 1

cohort2_data$depression[is.na(cohort2_data$s8_qolands)]<- NA

cohort1_data$pd_depression<- cohort1_data$pd_depression

cohort2_data$pd_depression<- cohort2_data$depression 

#1= not anxious or depressed
#2= I am moderately anxious or depressed
#3= I am extremely anxious or depressed



#}}}}

#Lab and Clinical Measures {{{====

#urinary protein
cohort1_data$lab_urin_protein
cohort2_data$lab_urin_protein<- cohort2_data$urine_pro

#urine glucose
cohort1_data$lab_urin_glucose
cohort2_data$lab_urin_glucose<- cohort2_data$urine_sugar

