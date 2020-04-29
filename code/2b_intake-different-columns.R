# PROGRESS {{{ ====

# This file is for disparate columns, so we can at least identify which ones we will need to massage into a correct definition

### TO DO
	# Add in cohort 1 phq scores
	# Don't need to apply functions to cohort2 data, makaes more sense to apply to combined data set

# }}}

# Demographics {{{====

# ward data cohort 2
cohort1_data$ward <- cohort1_data$ward

#missing ward data from cohort 2 as NA for now

cohort2_data$ward<- NA

# }}}

# PHQ-9 and QOL {{{====


#merge phq from both cohorts
cohort1_data$phq1<- cohort1_data$phq_1
cohort2_data$phq1<- cohort2_data$s7_phq1

cohort1_data$phq2<- cohort1_data$phq_2
cohort2_data$phq2<- cohort2_data$s7_phq2

cohort1_data$phq3<- cohort1_data$phq_3
cohort2_data$phq3<- cohort2_data$s7_phq3

cohort1_data$phq4<- cohort1_data$phq_4
cohort2_data$phq4<- cohort2_data$s7_phq4

cohort1_data$phq5<- cohort1_data$phq_5
cohort2_data$phq5<- cohort2_data$s7_phq5

cohort1_data$phq6<- cohort1_data$phq_6
cohort2_data$phq6<- cohort2_data$s7_phq6

cohort1_data$phq7<- cohort1_data$phq_7
cohort2_data$phq7<- cohort2_data$s7_phq7

cohort1_data$phq8<- cohort1_data$phq_8
cohort2_data$phq8<- cohort2_data$s7_phq8

cohort1_data$phq9<- cohort1_data$phq_9
cohort2_data$phq9<- cohort2_data$s7_phq9

cohort1_data$phq10<- cohort1_data$phq_10
cohort2_data$phq10<- cohort2_data$s7_phq10

					

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

# Lab and Clinical Measures {{{ ====

# urinary protein
# Convert TRACE/TRECE to 1, and then use +, ++, +++, ++++ to match with 1-4, Convert NIL to 0, and leave Spill as NA
cohort1_data$lab_urin_protein
cohort2_data$lab_urin_protein <- recode(cohort2_data$urine_pro, "NIL" = 0, "+" = 1, "TRACE" = 1, "TRECE" = 1, "++" = 2, "+++" = 3, "++++" = 4, .default = NULL)

# urine glucose
cohort1_data$lab_urin_glucose
cohort2_data$lab_urin_glucose <- recode(cohort2_data$urine_sugar, "NIL" = 0, "+" = 1, "++" = 2, "+++" = 3, "++++" = 4, .default = NULL)

# }}}
