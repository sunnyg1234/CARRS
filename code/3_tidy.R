# PROGRESS {{{ ====

# This file is for disparate columns, so we can at least identify which ones we will need to massage into a correct definition

### TO DO
	# Convert functions instead of transmute to just simple vector math (e.g. df$vector * df$secondthing)

### DEFINITIONS
	# Clinical covariates
	# Demographics: age, sex, smoking, alcohol
	# Obesity: weight, height, metabolic syndrome

# }}}

# Workspace setup {{{ ====

# Will need to "source" the intake files" to bring up the right R object / variables
# Can do this in RStudio



# }}}

# Merging Cohorts {{{ ====


# Demographic Tidying {{{ ====


#****** Covariates Baseline******


# Sex (male = 1, female = 0), may need transgender category
df$male[df$sex == 1] <- 1 # Males = 1
df$male[df$sex == 2] <- 0 # Females recoded from 2 to 0

# Age categories
df$agecat[df$age < 45] <- 0
df$agecat[df$age >= 45 & df$age < 65] <- 1
df$agecat[df$age >= 65] <- 2
df$agecat[is.na(df$age)] <- NA

# Education Categories
df$educat[df$edu_stat == 6 | df$edu_stat==7] <- 0 #no education
df$educat[df$edu_stat== 5] <- 1 #primary school
df$educat[df$edu_stat==3 | df$edu_stat==4] <- 2 # high school or secondary
df$educat[df$edu_stat == 1 | df$edu_stat==2] <- 3 #college
df$educat[is.na(df$edu_stat)] <- NA

#total household income
df$income[df$hh_income <= 2]<- 0 #income less than 10,000
df$income[df$hh_income >= 3]<- 1 #income more than 10,000
df$income[is.na(df$hh_income)] <- NA

#Smoking/ Tobacco
#tobacco ever used
df$tobaccoever[df$tob_everused ==1]<- 1 #yes
df$tobaccoever[df$tob_everused ==2]<- 0 #no
df$tobaccoever[is.na(df$tob_everused)] <- NA

#tobacco current use
df$tobaccocurrent[df$tob_curcons == 1]<- 1 # yes
df$tobaccocurrent[df$tob_curcons == 2| df$tob_everused == 2]<- 0 #no
df$tobaccocurrent[is.na(df$tob_everused)] <- NA

#tobacco category
df$tobaccocat[df$tobaccoever == 0 & df$tobaccocurrent == 0]<- 0 # no past or current use
df$tobaccocat[df$tobaccoever == 1 & df$tobaccocurrent == 0]<- 1 # Used in past, no current use
df$tobaccocat[df$tobaccoever == 1 & df$tobaccocurrent == 1]<- 2 #Used in past and use currently


#Alcohol current use
df$alcoholcurrent[df$alc_oftenuse==1|df$alc_oftenuse==2]<- 1 #current drinker
df$alcoholcurrent[df$alc_oftenuse >2 |df$alc_oftenuse ==0 ]<- 0 #past or never drinker
df$alcoholcurrent[is.na(df$alc_oftenuse)] <- NA

#alcohol ever use
df$alcoholever[df$alc_everused ==1]<- 1 #ever drinker
df$alcoholever[df$alc_everused ==2]<- 0 #never drinker
df$alcoholever[is.na(df$alc_everused)]<- NA

#alcohol category (3 categories, thus named *cat3)
df$alcoholcat3[df$alc_everused==2]<-0 #never user
df$alcoholcat3[df$alc_oftenuse>=2 | df$alc_oftenuse <=4]<- 1 #past user or current occasional drinker
df$alcoholcat3[df$alc_oftenuse==1]<-2 #current regular user


#Physical Activity
df$sedentarymins<- df %>% select(pa_sit_wkday_hr,pa_sit_wkday_min) %>% transmute(sedentarymins= (pa_sit_wkday_hr*60)+pa_sit_wkday_min) #mutate column to add together hours and minutes column and output in total minutes

df$sedentary_greater5[df$sedentarymins>=300]<- 1 #more than 5 hours of sedentary behavior per day

df$sedentary_greater5[df$sedentarymins<300] <- 0 #less than 5 hours of sedentary behavior per day

#Employement

# Named employ3 as there are 3 categories
df$employ3[df$emp_stat >= 2]<- 0 #not working housewife, student, retired)
df$employ3[df$emp_stat ==1 & df$curr_occ==5 ]<- 1 #unskilled manual labor, landless laborer
df$employ3[df$emp_stat ==1 & df$curr_occ==3|df$curr_occ==4]<- 2 #Skilled manual labourer, small business owner, small farmer,Semi-skilled manual labourer, marginal landowner, rickshaw driver, army jawan, carpenter, fitter
df$employ3[df$emp_stat ==1 & df$curr_occ==1|df$curr_occ==2]<- 1 #*1: Professional, big business, landlord, university teacher, class 1 IAS/services officer, lawyer, Trained, clerical, medium business owner, middle level farmer, teacher, maintenance (in charge), personnel manager ;


#}}}

# Anthropometric measurements tidying {{{====

df$heightm<- transmute(
	df,
	height_cm/100)


#BMI category
df$bmicat[df$bmi<23]<- 0
df$bmicat[df$bmi>=23 & df$bmi<=25]<- 1
df$bmicat[df$bmi>=25 & df$bmi<=30]<- 2
df$bmicat[df$bmi>=30]<- 3
df$bmicat[is.na(df$bmi)]<- NA

#obese (US definition)
df$obese[df$bmicat <=2]<-0 #not obese
df$obese[df$bmicat==3]<-1 #obese

#obese asian
df$obeseasian[df$bmicat <=1]<-0 #not obese asian
df$obeseasian[df$bmicat>=2]<-1 #obese asian

#waist to hip ratio mutation
df$waship<- transmute(
				df,
				waist_cm/hip_cm)

#waist to height ratio mutation
df$waistheightr<- transmute(
				df,
				waist_cm/height_cm)

#high waist/hip ratio?
df$highwaisthip[df$waship >.8] <- 1  #yes
df$highwaisthip[df$waship <=.8] <- 0 #no
df$highwaisthip[is.na(df$waship)] <- NA

#high waist/height ratio?
df$highwaistheight[df$waistheightr >.5] <- 1 #yes
df$highwaistheight[df$waistheightr <=.5] <- 0 #yes
df$highwaistheight[is.na(df$waistheightr)] <- NA


#high waist circumference?
df$highwaist[df$waist_cm>90 & df$male ==1]<- 1 #yes
df$highwaist[df$waist_cm<=90 & df$male==1] <-0 #no

df$highwaist[df$waist_cm>80 & df$male==0]<- 1 #yes
df$highwaist[df$waist_cm<=80 & df$male==0] <-0 #no

#body adiposity index
df$bai<- transmute(df, waist_cm/(height_cm/100)^1.5-18)

df$bai <- df$waist_cm / ((df$height_cm/100)^2)

#visceral adiposity index

# Convert to mmol
df$tgmmol <- df$lab_triglyc/88.57
df$hdlmmol <- df$lab_hdlchol/38.67

# For men
df$vai[df$sex == 1] <-
	(df[df$sex == 1, ]$waist_cm / (39.68 + 1.88*df[df$sex == 1, ]$bmi)) * (df[df$sex == 1, ]$tgmmol/1.03) * (1.31/df[df$sex == 1, ]$hdlmmol)

# For women
df$vai[df$sex == 2] <-
	(df[df$sex == 2, ]$waist_cm / (36.58 + 1.89*df[df$sex == 2, ]$bmi)) * (df[df$sex == 2, ]$tgmmol/0.81) * (1.52/df[df$sex == 2, ]$hdlmmol)

# if male=1 then vai=(waist/(39.68+(1.88*bmi)))*(tgmmol/1.03)*(1.31/hdlmmol);

vai1<- transmute(     #vai male
	df,
	(df$waist_cm/
		39.68+
		(1.88*df$bmi)))
#can't figure out how to add second part of equation to above
#*(df$tgmmol/1.03)*(1.31/df$hdlmmol)

vai2<- transmute(     #vai female
	df,
	(df$waist_cm/
	 	36.58+
	 	(1.88*df$bmi)))

#can't figure out how to add second part of equation to above
#*(df$tgmmol/0.81)*(1.52/df$hdlmmol)

df$vai[df$male==1]<- vai1

df$vai[df$male==0]<- vai2

#indicator for missing height and weight

df$missinghtwt[
	df$height_cm >9999|
		is.na(df$height_cm) &
		df$weight_kg>9999|
		is.na(df$weight_kg)
	]<- 1 #indicates missing height and weight

df$missinghtwt2[
	df$height_cm >9999 &
		df$weight_kg>9999]<- 1
    	#invalid height and weight





# Lab and clinical measures tidying {{{====

#Systolic BP average

#this forms a new column with the sbp averages and ignores na. the new column made doing this does not work with the row binding function for some reason


df$sbp_mean<- rowMeans(
			df[,c(
				"systolic_bp_first",
				"systolic_bp_second",
				"systolic_bp_third"
			)],
			na.rm=TRUE)



#Diastolic BP average

#this forms a new column with the dbp average for cohort 1 and ignores na. same problem with row binding


df$dbp_mean<- rowMeans(
	df[,c(
		"diastolic_bp_first",
		"diastolic_bp_second",
		"diastolic_bp_third"
	)],
	na.rm=TRUE)


#old hypertension cutoff groups
df$hypertension[df$sbp_mean >= 140 | df$dbp_mean >= 90| df$hbp_trt_allopdrug==1]<-1 #yes(old)

df$hypertension[df$sbp_mean < 140 & df$dbp_mean < 90 & df$hbp_trt_allopdrug==2]<-0 #no(old)

#new hypertension cutoff groups
df$hypertension1[df$sbp_mean >= 130 | df$dbp_mean >= 80| df$hbp_trt_allopdrug==1]<-1 #yes(new)

df$hypertension1[df$sbp_mean < 130 & df$dbp_mean < 80 & df$hbp_trt_allopdrug==2]<-0  #no(new)

#diabetes
df$diabetes[df$lab_fasting >= 126 | df$lab_HbA1c >= 6.5| df$dia_trt_allopdrug==1]<-1 #yes

df$diabetes[df$lab_fasting <125 & df$lab_HbA1c < 6.5]<- 0  #no

#prediabetes
df$prediabetes[df$lab_fasting >= 100 |df$dia_trt_allopdrug ==1]<-1 #yes

df$prediabetes[df$lab_fasting <100]<- 0  #no

#High total cholesterol
df$lab_total_chol[df$lab_tchol >= 200 |df$hyp_trt_allopdrug==1]<-1 #yes

df$lab_total_chol[df$lab_tchol <200]<- 0  #no

#High LDL cholesterol
df$lab_ldl_chol[df$lab_ldlchol >= 130 ] <-1 #yes

df$lab_ldl_chol[df$lab_ldlchol < 130 ] <-1 #no

#Low HDL cholesterol
df$lowhdl[df$sex==1 & df$lab_hdlchol <=40] <- 1

df$lowhdl[df$sex==1 & df$lab_hdlchol >40] <- 0

df$lowhdl[df$sex==2 & df$lab_hdlchol <=50 ] <- 1

df$lowhdl[df$sex==2 & df$lab_hdlchol >50] <- 0

#High Triglycerides
df$hightg[df$lab_triglyc >= 150]<- 1

df$hightg[df$lab_triglyc < 150]<- 0


#mMetabolic syndrome

#sums of metabolic syndrome variables
df$metsyn_sum<- rowSums(
	df[,c(
		"prediabetes",
		"hypertension1",
		"highwaist",
		"lowhdl",
		"hightg")],
	na.rm=TRUE
	)

#categories

df$met_syn[df$male==1 & df$metsyn_sum >=3] <- 1 #yes
df$met_syn[df$male==1 & df$metsyn_sum <3] <- 0 #no
df$met_syn[df$male==0 & df$metsyn_sum >=3] <- 1 #yes
df$met_syn[df$male==0 & df$metsyn_sum <3] <- 0 #no

#}}}

# Merge files {{{ ====

#common variables for merging
fvar <- c(
	"pid",
	"male",
	"agecat",
	"educat",
	"income",
	"tobaccoever",
	"tobaccocurrent",
	"tobaccocat",
	"alcoholcurrent",
	"alcoholever",
	"alcoholcat3",
	"sedentary_greater5",
	"employ3",
	"weight_kg",
	"height_cm",
	"heightm",
	"bmi",
	"bmicat",
	"obese",
	"obeseasian",
	"waist_cm",
	"hip_cm",
	"waship",
	"waistheightr",
	"highwaisthip",
	"highwaistheight",
	"highwaist",
	"sbp_mean",
	"dbp_mean",
	"hypertension",
	"hypertension1",
	"bai",
	"tgmmol",
	"diabetes",
	"prediabetes",
	"lab_total_chol",
	"lab_ldl_chol",
	"lowhdl",
	"hightg",
	"metsyn_sum",
	"met_syn",
	"missinghtwt",
	"missinghtwt2"
)

#tidied data
tidy_data<- df[fvar]

# }}}


