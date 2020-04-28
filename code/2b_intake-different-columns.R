# PROGRESS {{{ ====

# This file is for disparate columns, so we can at least identify which ones we will need to massage into a correct definition

### TO DO
# Done<- PHQ9 binary socre (>=10 is "depression")
# Done<- Convert TRACE/TRECE to 1, and then use +, ++, +++, ++++ to match with 1-4, Convert NIL to 0, and leave Spill as NA

# }}}

# Demographics {{{====

# ward data cohort 2
cohort1_data$ward <- cohort1_data$ward

#missing ward data from cohort 2 as NA for now

cohort2_data$ward<- NA

# }}}

# PHQ-9 and QOL {{{====

# Below all the phq codes were recoded so that it will be easy to add them and give people a total score

# phq1 recoded
cohort2_data$phq1[cohort2_data$s7_phq1 == 1] <- 0
cohort2_data$phq1[cohort2_data$s7_phq1 == 2] <- 1
cohort2_data$phq1[cohort2_data$s7_phq1 == 3] <- 2
cohort2_data$phq1[cohort2_data$s7_phq1 == 4] <- 3


# phq2 recoded
cohort2_data$phq2[cohort2_data$s7_phq2 == 1] <- 0
cohort2_data$phq2[cohort2_data$s7_phq2 == 2] <- 1
cohort2_data$phq2[cohort2_data$s7_phq2 == 3] <- 2
cohort2_data$phq2[cohort2_data$s7_phq2 == 4] <- 3

# phq3 recoded
cohort2_data$phq3[cohort2_data$s7_phq3 == 1] <- 0
cohort2_data$phq3[cohort2_data$s7_phq3 == 2] <- 1
cohort2_data$phq3[cohort2_data$s7_phq3 == 3] <- 2
cohort2_data$phq3[cohort2_data$s7_phq3 == 4] <- 3

# phq4 recoded
cohort2_data$phq4[cohort2_data$s7_phq4 == 1] <- 0
cohort2_data$phq4[cohort2_data$s7_phq4 == 2] <- 1
cohort2_data$phq4[cohort2_data$s7_phq4 == 3] <- 2
cohort2_data$phq4[cohort2_data$s7_phq4 == 4] <- 3

# phq5 recoded
cohort2_data$phq5[cohort2_data$s7_phq5 == 1] <- 0
cohort2_data$phq5[cohort2_data$s7_phq5 == 2] <- 1
cohort2_data$phq5[cohort2_data$s7_phq5 == 3] <- 2
cohort2_data$phq5[cohort2_data$s7_phq5 == 4] <- 3

# phq6 recoded
cohort2_data$phq6[cohort2_data$s7_phq6 == 1] <- 0
cohort2_data$phq6[cohort2_data$s7_phq6 == 2] <- 1
cohort2_data$phq6[cohort2_data$s7_phq6 == 3] <- 2
cohort2_data$phq6[cohort2_data$s7_phq6 == 4] <- 3

# phq7 recoded
cohort2_data$phq7[cohort2_data$s7_phq7 == 1] <- 0
cohort2_data$phq7[cohort2_data$s7_phq7 == 2] <- 1
cohort2_data$phq7[cohort2_data$s7_phq7 == 3] <- 2
cohort2_data$phq7[cohort2_data$s7_phq7 == 4] <- 3

# phq8 recoded
cohort2_data$phq8[cohort2_data$s7_phq8 == 1] <- 0
cohort2_data$phq8[cohort2_data$s7_phq8 == 2] <- 1
cohort2_data$phq8[cohort2_data$s7_phq8 == 3] <- 2
cohort2_data$phq8[cohort2_data$s7_phq8 == 4] <- 3

# phq9 recoded
cohort2_data$phq9[cohort2_data$s7_phq9 == 1] <- 0
cohort2_data$phq9[cohort2_data$s7_phq9 == 2] <- 1
cohort2_data$phq9[cohort2_data$s7_phq9 == 3] <- 2
cohort2_data$phq9[cohort2_data$s7_phq9 == 4] <- 3

# phq10 recoded
cohort2_data$phq10[cohort2_data$s7_phq10 == 1] <- 0
cohort2_data$phq10[cohort2_data$s7_phq10 == 2] <- 1
cohort2_data$phq10[cohort2_data$s7_phq10 == 3] <- 2
cohort2_data$phq10[cohort2_data$s7_phq10 == 4] <- 3


# new phq_total column which is sum of phq1 to phq9
x <-
	cohort2_data %>%
	select(phq1, phq2, phq3, phq4, phq5, phq6, phq7, phq8, phq9) %>%
	mutate(phq_total = (phq1 + phq2 + phq3 + phq4 + phq5 + phq6 + phq7 + phq8 + phq9))
# Merge back into original data
cohort2_data$phq_total <- x$phq_total

# phq-9 categories
cohort2_data$phq_cat[cohort2_data$phq_total >= 0 & cohort2_data$phq_total <= 4] <- 0 # no depression

cohort2_data$phq_cat[cohort2_data$phq_total >= 5 & cohort2_data$phq_total <= 9] <- 1 # mild depression

cohort2_data$phq_cat[cohort2_data$phq_total >= 10 & cohort2_data$phq_total <= 14] <- 2 # moderate depression

cohort2_data$phq_cat[cohort2_data$phq_total >= 15 & cohort2_data$phq_total <= 19] <- 3 # moderately severe depression

cohort2_data$phq_cat[cohort2_data$phq_total >= 20 & cohort2_data$phq_total <= 27] <- 4 # severe depression

# PHQ binary (>= 10)

cohort2_data$phq_binary[cohort2_data$phq_total>=10]<- 1 #depression

cohort2_data$phq_binary[cohort2_data$phq_total<10]<- 0 #no depression

# For Cohort 1, data is given as null
cohort1_data$phq_total <- cohort1_data$phq_cat <- cohort1_data$phq_binary<- NA



# mobility
cohort2_data$mobility[cohort2_data$s8_qolmob == 1] <- 1

cohort2_data$mobility[cohort2_data$s8_qolmob >= 2 | cohort2_data$s8_qolmob <= 4] <- 2 # converting to equivalent

cohort2_data$mobility[cohort2_data$s8_qolmob == 5] <- 3 # converting to equivalent

cohort2_data$mobility[is.na(cohort2_data$s8_qolmob)] <- NA

cohort1_data$mobility<- cohort1_data$pd_mobility

# new codes: 1= no problems walking; 2= mild/moderate/severe problems walking 3= unable to walk


# self care ability
cohort2_data$self_care[cohort2_data$s8_qolself == 1] <- 1

cohort2_data$self_care[cohort2_data$s8_qolself >= 2 | cohort2_data$s8_qolself <= 4] <- 2 # converting to equivalent

cohort2_data$self_care[cohort2_data$s8_qolself == 5] <- 3 # converting to equivalent

cohort2_data$self_care[is.na(cohort2_data$s8_qolself)] <- NA

cohort1_data$self_care <- cohort1_data$pd_selfcare

 # new codes: 1= no problems ; 2= mild/moderate/severe problems  3= unable to take care of self

# usual activities
cohort2_data$usual_act[cohort2_data$s8_qoluat == 1] <- 1

cohort2_data$usual_act[cohort2_data$s8_qoluat >= 2 | cohort2_data$s8_qoluat <= 4] <- 2 # converting to equivalent

cohort2_data$usual_act[cohort2_data$s8_qoluat == 5] <- 3 # converting to equivalent

cohort2_data$usual_act[is.na(cohort2_data$s8_qoluat)] <- NA

cohort1_data$usual_act<- cohort1_data$pd_usualact

 # new codes: 1= no problems ; 2= mild/moderate/severe problems  3= unable to take care of self

# pain
cohort2_data$pain[cohort2_data$s8_qoldcm == 1] <- 1

cohort2_data$pain[cohort2_data$s8_qoldcm == 2 | cohort2_data$s8_qoldcm == 3] <- 2 # converting to equivalent

cohort2_data$pain[cohort2_data$s8_qoldcm == 4 | cohort2_data$s8_qoldcm == 5] <- 3 # converting to equivalent

cohort2_data$pain[is.na(cohort2_data$s8_qoldcm)] <- NA

cohort1_data$pain <- cohort1_data$pd_pain

# new codes: 1= no problems ; 2= mild/moderate pain 3= severe pain

# depression

cohort2_data$depression[cohort2_data$s8_qolands == 1] <- 1

cohort2_data$depression[cohort2_data$s8_qolands == 2 | cohort2_data$s8_qoldcm == 3] <- 2 # converting to equivalent

cohort2_data$depression[cohort2_data$s8_qolands == 4 | cohort2_data$s8_qolands == 5] <- 3 # cconverting to equivalent

cohort2_data$depression[is.na(cohort2_data$s8_qolands)] <- NA

cohort1_data$depression <- cohort1_data$pd_depression

# 1= not anxious or depressed
# 2= I am mild or moderately anxious or depressed
# 3= I am extremely anxious or depressed


# }}}}

# Lab and Clinical Measures {{{====

# urinary protein
# Convert TRACE/TRECE to 1, and then use +, ++, +++, ++++ to match with 1-4, Convert NIL to 0, and leave Spill as NA
cohort1_data$lab_urin_protein
cohort2_data$lab_urin_protein <- recode(cohort2_data$urine_pro, "NIL" = 0, "+" = 1, "TRACE" = 1, "TRECE" = 1, "++" = 2, "+++" = 3, "++++" = 4, .default = NULL)

# urine glucose
cohort1_data$lab_urin_glucose
cohort2_data$lab_urin_glucose <- recode(cohort2_data$urine_sugar, "NIL" = 0, "+" = 1, "++" = 2, "+++" = 3, "++++" = 4, .default = NULL)
