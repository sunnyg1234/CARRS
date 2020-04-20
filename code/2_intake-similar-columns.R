# PROGRESS {{{ ====

### TO DO
	# Demographics need to include home ID and city ID and ward ID
	# BMI?
	# Labs?
	# Can we classify CKD better? Baseline creatinine, CKD stage? Urinalysis? Urine protein?
	# Body weight = do we have any other measurements of body types? Any estimates on body fat? Like caliper measurements or arm size, etc?
	# Labs to be added for cohort1 (Anish will find cohort 2)

# }}}

# Workspace setup {{{ ====


# Libraries (load before doing any data analysis)
library(tidyverse)

# }}}

# Intake {{{ ====

# Read in quickly with read_*** function
cohort1_data <- read_csv("../data/cohort1-short.csv")
cohort2_data <- read_csv("../data/cohort2-short.csv")

# }}}

# Sunny/BMI Section {{{ ====

#This adds a column to the data for BMI using the height and weight with proper conversions
cohort1_data<- mutate(cohort1_data,BMI=(weight_kg/(height_cm/100)^2))

# }}}

# Demographics for Merging {{{ ====

# Age
cohort1_data$age <- cohort1_data$pd_age
cohort2_data$age <- cohort2_data$s1a_age

# Sex
cohort1_data$sex <- cohort1_data$pd_sex
cohort2_data$sex <- cohort2_data$s1a_sex

#Educational status
cohort1_data$edu_stat <- cohort1_data$pd_edu_stat
cohort2_data$edu_stat <- cohort2_data$s1a_est

# Education years
cohort1_data$edu_yrs <- cohort1_data$pd_edu_yrs
cohort2_data$edu_yrs <- cohort2_data$s1a_fedu

#Household income
cohort1_data$hh_income <- cohort1_data$pd_hhincome
cohort2_data$hh_income <- cohort2_data$s1a_toin

#employement status
cohort1_data$emp_stat <- cohort1_data$pd_emp_stat
cohort2_data$emp_stat <- cohort2_data$s1a_emst

#current occupation
cohort1_data$curr_occ<- cohort1_data$pd_cur_occu
cohort2_data$curr_occ <- cohort2_data$s1a_coc

# }}}

# Tobacco use for Merging {{{ ====

#Ever used tobacco
cohort1_data$tob_everused<- cohort1_data$tob_everused
cohort2_data$tob_everused <- cohort2_data$s2a_euto

#In what forms used tobacco? Smoking (0=NA)
cohort1_data$tob_smkfrm<- cohort1_data$tob_smkfrm
cohort2_data$tob_smkfrm <- cohort2_data$s2a_tofsmo

#In what forms used tobacco? chewed (0=NA)
cohort1_data$tob_chwfrm<- cohort1_data$tob_chwfrm
cohort2_data$tob_chwfrm <- cohort2_data$s2a_tofchw

#In what forms used tobacco? any other
cohort1_data$tob_other<- cohort1_data$tob_other
cohort2_data$tob_other <- cohort2_data$s2a_tofoth

#Current tobacco use?
cohort1_data$tob_curcons<- cohort1_data$tob_curcons
cohort2_data$tob_curcons <- cohort2_data$s2a_cut

#If current tobacco use, smoking, how often?
cohort1_data$tob_cursmkfrm<- cohort1_data$tob_cursmkfrm
cohort2_data$tob_cursmkfrm <- cohort2_data$s2a_smoft

#If current tobacco use, chewing, how often?
cohort1_data$tob_curchwfrm<- cohort1_data$tob_curchwfrm
cohort2_data$tob_curchwfrm <- cohort2_data$s2a_chwoft

#If current tobacco use, other forms, how often?
cohort1_data$tob_curothfrm<- cohort1_data$tob_curothfrm
cohort2_data$tob_curothfrm <- cohort2_data$s2a_croth

#Duration of cigarette use in years?
cohort1_data$tob_cigdur_yrs<- cohort1_data$tob_cigdur_yrs
cohort2_data$tob_cigdur_yrs <- cohort2_data$s2a_cigy

#Duration of cigarette use in months?
cohort1_data$tob_cigdur_mon<- cohort1_data$tob_cigdur_mon
cohort2_data$tob_cigdur_mon <- cohort2_data$s2a_cigmo

#Usage of cigarettes per month?
cohort1_data$tob_cig_permonth<- cohort1_data$tob_cig_permonth
cohort2_data$tob_cig_permonth <- cohort2_data$s2a_cigpmo

#Duration of beedi use in years?
cohort1_data$tob_bddur_yrs<- cohort1_data$tob_bddur_yrs
cohort2_data$tob_bddur_yrs <- cohort2_data$s2a_beey

#Duration of beedi use in months?
cohort1_data$tob_bddur_mon<- cohort1_data$tob_bddur_mon
cohort2_data$tob_bddur_mon<- cohort2_data$s2a_beey

#Usage of beedi per month?
cohort1_data$tob_bd_permonth<- cohort1_data$tob_bd_permonth
cohort2_data$tob_bd_permonth <- cohort2_data$s2a_beepmo

#Duration of cigar use in years?
cohort1_data$tob_cigardur_yrs<- cohort1_data$tob_cigdur_yrs
cohort2_data$tob_cigardur_yrs <- cohort2_data$s2a_cgry

#Duration of cigar use in months?
cohort1_data$tob_cigardur_mon<- cohort1_data$tob_cigardur_mon
cohort2_data$tob_cigardur_mon <- cohort2_data$s2a_cgry

#usage of cigar per month
cohort1_data$tob_cigarusage<- cohort1_data$tob_cigarusage
cohort2_data$tob_cigarusage <- cohort2_data$s2a_cgrpmo

#duration of hookah use in years
cohort1_data$tob_hukdur_yrs<- cohort1_data$tob_hukdur_yrs
cohort2_data$tob_hukdur_yrs <- cohort2_data$s2a_huky

#duration of hookah use in months
cohort1_data$tob_hukdur_mon<- cohort1_data$tob_hukdur_mon
cohort2_data$tob_hukdur_mon <- cohort2_data$s2a_hukmo

#Usage of hookah per month
cohort1_data$tob_huk_permonth<- cohort1_data$tob_huk_permonth
cohort2_data$tob_huk_permonth <- cohort2_data$s2a_hukpmo

#duration of chewing tob use in years
cohort1_data$tob_tchewdur_yrs<- cohort1_data$tob_tchewdur_yrs
cohort2_data$tob_tchewdur_yrs <- cohort2_data$s2a_tocy

#duration of chewing tob use in months
cohort1_data$tob_tchewdur_mon<- cohort1_data$tob_tchewdur_mon
cohort2_data$tob_tchewdur_mon <- cohort2_data$s2a_tocmo

#Usage of chewing tob per month
cohort1_data$tob_tchew_permonth<- cohort1_data$tob_tchew_permonth
cohort2_data$tob_tchew_permonth <- cohort2_data$s2a_tocpmo

#duration of pan with zarda in yrs
cohort1_data$tob_pandur_yrs<- cohort1_data$tob_pandur_yrs
cohort2_data$tob_pandur_yrs <- cohort2_data$s2a_pazy

#duration of pan with zarda in mons
cohort1_data$tob_pandur_mon<- cohort1_data$tob_pandur_mon
cohort2_data$tob_pandur_mon<- cohort2_data$s2a_pazmo

#Usage of pan with zarda in mons
cohort1_data$tob_pan_permonth<- cohort1_data$tob_pan_permonth
cohort2_data$tob_pan_permonth <- cohort2_data$s2a_pazpmo

#duration of pan with zarda in yrs
cohort1_data$tob_panmasdur_yrs<- cohort1_data$tob_panmasdur_yrs
cohort2_data$tob_panmasdur_yrs <- cohort2_data$s2a_pmwzy

#duration of pan with zarda in mons
cohort1_data$tob_panmasdur_mon<- cohort1_data$tob_panmasdur_mon
cohort2_data$tob_panmasdur_mon<- cohort2_data$s2a_pmwzmo

#Usage of pan with zarda in mons
cohort1_data$tob_panmas_permonth<- cohort1_data$tob_panmas_permonth
cohort2_data$tob_panmas_permonth <- cohort2_data$s2a_pmwzpmo

#duration of snuff in yrs
cohort1_data$tob_snuffdur_yrs<- cohort1_data$tob_snuffdur_yrs
cohort2_data$tob_snuffdur_yrs <- cohort2_data$s2a_sufy

#duration of snuff in mons
cohort1_data$tob_snuffdur_mon<- cohort1_data$tob_snuffdur_mon
cohort2_data$tob_snuffdur_mon<- cohort2_data$s2a_sufmo

#Usage of snuff  in mons
cohort1_data$tob_snuff_permonth<- cohort1_data$tob_snuff_permonth
cohort2_data$tob_snuff_permonth <- cohort2_data$s2a_sufpmo

#duration of gutka in yrs
cohort1_data$tob_gutkhadur_yrs<- cohort1_data$tob_gutkhadur_yrs
cohort2_data$tob_gutkhadur_yrs <- cohort2_data$s2a_sufy

#duration of gutka in mons
cohort1_data$tob_gutkhadur_mon<- cohort1_data$tob_gutkhadur_mon
cohort2_data$tob_gutkhadur_mon<- cohort2_data$s2a_gtkmo

#Usage of gutka in mons
cohort1_data$tob_gutkha_permonth<- cohort1_data$tob_gutkha_permonth
cohort2_data$tob_gutkha_permonth <- cohort2_data$s2a_gtkpmo

#At what age did you start smoking regularly?
cohort1_data$tob_smk_strtage<- cohort1_data$tob_smk_strtage
cohort2_data$tob_smk_strtage <- cohort2_data$s2a_smsage

#At what age did you start using smokeless tobacco regularly?
cohort1_data$tob_smkless_strtage<- cohort1_data$tob_smkless_strtage
cohort2_data$tob_smkless_strtage <- cohort2_data$s2a_smlsage

#Are you exposed to tobacco smoke from others regularly?
cohort1_data$tob_smkexpo<- cohort1_data$tob_smkexpo
cohort2_data$tob_smkexpo<- cohort2_data$s2a_exp

#if exposed to tob smoke, how many days a week?
cohort1_data$tob_smkexpo_wk<- cohort1_data$tob_smkexpo_wk
cohort2_data$tob_smkexpo_wk <- cohort2_data$s2a_epdwe

# }}}

# Alcohol use for merging {{{ ====

#Have you ever used alcohol?
cohort1_data$alc_everused<- cohort1_data$alc_everused
cohort2_data$alc_everused <- cohort2_data$s2b_ealc

#How often do you use alcoholic beverages?
cohort1_data$alc_oftenuse<- cohort1_data$alc_oftenuse
cohort2_data$alc_oftenuse <- cohort2_data$s2b_houal

#Local spirits duration years
cohort1_data$alc_localdur_yrs<- cohort1_data$alc_localdur_yrs
cohort2_data$alc_localdur_yrs<- cohort2_data$s2b_lcy

#local spirits frequency of use per week
cohort1_data$alc_localfreq<- cohort1_data$alc_localfreq
cohort2_data$alc_localfreq <- cohort2_data$s2b_flcw

#local spirits quanity in ml/peg
cohort1_data$alc_localqty<- cohort1_data$alc_localqty
cohort2_data$alc_localqty <- cohort2_data$s2b_qlc

#spirits eg whisky, rum, brandy, gin duration years
cohort1_data$alc_spiritdur_yrs<- cohort1_data$alc_spiritdur_yrs
cohort2_data$alc_spiritdur_yrs <- cohort2_data$s2b_spy

#spirits eg whisky, rum, brandy, gin frequency of use per week
cohort1_data$alc_spiritfreq<- cohort1_data$alc_spiritfreq
cohort2_data$alc_spiritfreq <- cohort2_data$s2b_fspw

#spirits eg whisky, rum, brandy, gin quantity
cohort1_data$alc_spiritqty<- cohort1_data$alc_spiritqty
cohort2_data$alc_spiritqty <- cohort2_data$s2b_qsp

#duration of beer use in years
cohort1_data$alc_beerdur_yrs<- cohort1_data$alc_beerdur_yrs
cohort2_data$alc_beerdur_yrs <- cohort2_data$s2b_bry

#frequency of beer use per week
cohort1_data$alc_beerfreq<- cohort1_data$alc_beerfreq
cohort2_data$alc_beerfreq <- cohort2_data$s2b_fbrw

#beer use quantity
cohort1_data$alc_beerqty<- cohort1_data$alc_beerqty
cohort2_data$alc_beerqty <- cohort2_data$s2b_qbr

#duration of wine use in years
cohort1_data$alc_winedur_yrs<- cohort1_data$alc_winedur_yrs
cohort2_data$alc_winedur_yrs <- cohort2_data$s2b_wiy

#frequency of wine use per week
cohort1_data$alc_winefreq<- cohort1_data$alc_winefreq
cohort2_data$alc_winefreq <- cohort2_data$s2b_fwiw

#quantity of wine use
cohort1_data$alc_wineqty<- cohort1_data$alc_wineqty
cohort2_data$alc_wineqty <- cohort2_data$s2b_qwi

# }}}

# Physical activity for Merging {{{ ====

#In a week, how many days of vigorous physical activity? (# of days)
cohort1_data$pa_vigact_days<- cohort1_data$pa_vigact_days
cohort2_data$pa_vigact_days <- cohort2_data$s2c_vad

#In a day, how many hours do you spend doing vigorous activity?
cohort1_data$pa_vigacttime_min<- cohort1_data$pa_vigacttime_min
cohort2_data$pa_vigacttime_min<- cohort2_data$s2c_vam

#in a day, how many minutes do you spend doing vigrous activity?
cohort1_data$pa_vigacttime_hr<- cohort1_data$pa_vigacttime_hr
cohort2_data$pa_vigacttime_hr<- cohort2_data$s2c_vah


#In a week, on how many days did you do moderate physical activity?
cohort1_data$pa_modact_days<- cohort1_data$pa_modact_days
cohort2_data$pa_modact_days <- cohort2_data$s2c_mad

#In a day, how many hours do you spend doing moderate physical activity?
cohort1_data$pa_modacttime_hr<- cohort1_data$pa_modacttime_hr
cohort2_data$pa_modacttime_hr <- cohort2_data$s2c_mah

#in a day, how many minutes do you spend doing moderate physical activity?
cohort1_data$pa_modacttime_min<- cohort1_data$pa_modacttime_min
cohort2_data$pa_modacttime_min <- cohort2_data$s2c_mam

#In a week, how many days did you walk atleast 10 minutes?
cohort1_data$pa_walk_days<- cohort1_data$pa_walk_days
cohort2_data$pa_walk_days <- cohort2_data$s2c_wad

#In a day, how many hours do you spend walking?
cohort1_data$pa_walktime_hr<- cohort1_data$pa_walktime_hr
cohort2_data$pa_walktime_hr <- cohort2_data$s2c_wah

#In a day, how many minutes do you spend walking?
cohort1_data$pa_walktime_min<- cohort1_data$pa_walktime_min
cohort2_data$pa_walktime_min <- cohort2_data$s2c_wam

# }}}

# Sleep for Merging {{{ ====

#How many hours of sleep do you usually get on weekdays?
cohort1_data$slp_hrswkdays<- cohort1_data$slp_hrswkdays
cohort2_data$slp_hrswkdays <- cohort2_data$s2d_snwo

#How many hours of sleep do you get on weekends?
cohort1_data$slp_hrwkend<- cohort1_data$slp_hrwkend
cohort2_data$slp_hrwkend<- cohort2_data$s2d_snwe

#How often do you have trouble following asleep?
cohort1_data$slp_trblslp<- cohort1_data$slp_trblslp
cohort2_data$slp_trblslp <- cohort2_data$s2d_ts

#How often do you wake up during the night and have trouble following asleep?
cohort1_data$slp_nightdiff<- cohort1_data$slp_nightdiff
cohort2_data$slp_nightdiff <- cohort2_data$s2d_wn

#How often do you feel unrested during the day nomatter how many hours you sleep?
cohort1_data$slp_feelunrest<- cohort1_data$slp_feelunrest
cohort2_data$slp_feelunrest <- cohort2_data$s2d_fun

#how often do you not get enough sleep?
cohort1_data$slp_notenough<- cohort1_data$slp_notenough
cohort2_data$slp_notenough <- cohort2_data$s2d_esl

#How often do you take sleeping pills or other medication to help you sleep?
cohort1_data$slp_pills<- cohort1_data$slp_pills
cohort2_data$slp_pills <- cohort2_data$s2d_spi

# }}}

# Diet for Merging {{{ ====

#Are you a vegetarian?
cohort1_data$dt_veg<- cohort1_data$dt_veg
cohort2_data$dt_veg <- cohort2_data$s2e_v

#Do you eat eggs?
cohort1_data$dt_egg<- cohort1_data$dt_egg
cohort2_data$dt_egg <- cohort2_data$s2e_e

#Are you on a special diet?
cohort1_data$dt_spldiet<- cohort1_data$dt_spldiet
cohort2_data$dt_spldiet <- cohort2_data$s2e_sd

#If yes, are you on a diabetic diet?
cohort1_data$dt_diabdiet<- cohort1_data$dt_diabdiet
cohort2_data$dt_diabdiet <- cohort2_data$s2e_dd

#If yes, are you on a weight reducing diet?
cohort1_data$dt_wtreducediet<- cohort1_data$dt_wtreducediet
cohort2_data$dt_wtreducediet <- cohort2_data$s2e_wrd

# }}}

# HBP for Merging {{{ ====
#Do you have high blood pressure?
cohort1_data$pd_hbp<- cohort1_data$pd_hbp
cohort2_data$pd_hbp <- cohort2_data$s3a1_hbp

#How many years have you had high blood pressure? (YYYY)
cohort1_data$hbp_since_yrs<- cohort1_data$hbp_since_yrs
cohort2_data$hbp_since_yrs <- cohort2_data$s3a1_hbpy

#How many months have you had high blood pressure? (MM)
cohort1_data$hbp_since_mon<- cohort1_data$hbp_since_mon
cohort2_data$hbp_since_mon <- cohort2_data$s3a1_thbpdie

#Are you taking prescribed dietary modifications for your high blood pressure?
cohort1_data$hbp_trt_dietmod<- cohort1_data$hbp_trt_dietmod
cohort2_data$hbp_trt_dietmod <- cohort2_data$s3a1_hbp

#Are you taking any prescribed physical exercise for your high blood pressure?
cohort1_data$hbp_trt_phyexer<- cohort1_data$hbp_trt_phyexer
cohort2_data$hbp_trt_phyexer <- cohort2_data$s3a1_thbpex

#Are you taking any traditional medicine for your high blood pressure?
cohort1_data$hbp_trt_tradmed<- cohort1_data$hbp_trt_tradmed
cohort2_data$hbp_trt_tradmed <- cohort2_data$s3a1_thbpth

#Are you taking any allopathic(modern) drugs for your high blood pressure?
cohort1_data$hbp_trt_allopdrug<- cohort1_data$hbp_trt_allopdrug
cohort2_data$hbp_trt_allopdrug <- cohort2_data$s3a1_thbpdr

#Are you not taking any medication for your high blood pressure?
cohort1_data$hbp_trt_none<- cohort1_data$hbp_trt_none
cohort2_data$hbp_trt_none <- cohort2_data$s3a1_thbpno

# }}}

# Diabetes for Merging {{{ ====

#Do you have diabetes?
cohort1_data$pd_diabetes<- cohort1_data$pd_diabetes
cohort2_data$pd_diabetes <- cohort2_data$s3a1_dia

#How many years have you had diabetes? (YYYY)
cohort1_data$dia_since_yrs<- cohort1_data$dia_since_yrs
cohort2_data$dia_since_yrs <- cohort2_data$s3a1_diay

#How many months have you had diabetes? (MM)
cohort1_data$dia_since_mon<- cohort1_data$dia_since_mon
cohort2_data$dia_since_mon <- cohort2_data$s3a1_diamo

#Are you taking prescribed dietary modifications for your diabetes?
cohort1_data$dia_trt_dietmod<- cohort1_data$dia_trt_dietmod
cohort2_data$dia_trt_dietmod <- cohort2_data$s3a1_tdiadie

#Are you taking any prescribed physical exercise for your diabetes?
cohort1_data$dia_trt_phyexer<- cohort1_data$dia_trt_phyexer
cohort2_data$dia_trt_phyexer <- cohort2_data$s3a1_tdiaex

#Are you taking any traditional medicine for your diabetes?
cohort1_data$dia_trt_tradmed<- cohort1_data$dia_trt_tradmed
cohort2_data$dia_trt_tradmed <- cohort2_data$s3a1_tdiath

#Are you taking any allopathic(modern) drugs for your diabetes?
cohort1_data$dia_trt_allopdrug<- cohort1_data$dia_trt_allopdrug
cohort2_data$dia_trt_allopdrug <- cohort2_data$s3a1_tdiadr

#Are you not taking any medication for your diabetes?
cohort1_data$dia_trt_none<- cohort1_data$dia_trt_none
cohort2_data$dia_trt_none <- cohort2_data$s3a1_tdiano

# }}}

# Hyperlipidemia for Merging {{{ ====

#Do you have hyperlipidemia?
cohort1_data$pd_hyperlip<- cohort1_data$pd_hyperlip
cohort2_data$pd_hyperlip <- cohort2_data$s3a1_hyl

#How many years have you had hyperlipidemia? (YYYY)
cohort1_data$hyp_since_yrs<- cohort1_data$hyp_since_yrs
cohort2_data$hyp_since_yrs <- cohort2_data$s3a1_hyly

#How many months have you had hyperlipidemia? (MM)
cohort1_data$hyp_since_mon<- cohort1_data$hyp_since_mon
cohort2_data$hyp_since_mon <- cohort2_data$s3a1_hylmo

#Are you taking prescribed dietary modifications for your hyperlipidemia?
cohort1_data$hyp_trt_dietmod<- cohort1_data$hyp_trt_dietmod
cohort2_data$hyp_trt_dietmod <- cohort2_data$s3a1_thyldie

#Are you taking any prescribed physical exercise for your hyperlipidemia?
cohort1_data$hyp_trt_phyexer<- cohort1_data$hyp_trt_phyexer
cohort2_data$hyp_trt_phyexer <- cohort2_data$s3a1_thylex

#Are you taking any traditional medicine for your hyperlipidemia?
cohort1_data$hyp_trt_tradmed<- cohort1_data$hyp_trt_tradmed
cohort2_data$hyp_trt_tradmed <- cohort2_data$s3a1_thylth

#Are you taking any allopathic(modern) drugs for your hyperlipidemia?
cohort1_data$hyp_trt_allopdrug<- cohort1_data$hyp_trt_allopdrug
cohort2_data$hyp_trt_allopdrug <- cohort2_data$s3a1_thyldr

#Are you not taking any medication for your hyperlipidemia?
cohort1_data$hyp_trt_none<- cohort1_data$hyp_trt_none
cohort2_data$hyp_trt_none <- cohort2_data$s3a1_thylno

# }}}

# Heart disease for Merging {{{ ====

#Do you have heart disease?
cohort1_data$pd_heart<- cohort1_data$pd_heart
cohort2_data$pd_heart <- cohort2_data$s3a2_hrt

#When did you first come to know you have heart disease?
cohort1_data$ht_first<- cohort1_data$ht_first
cohort2_data$ht_first <- cohort2_data$s3a2_khrtd

#If Dr. said you have heart disease, what did doctor say it was?
cohort1_data$ht_drsay<- cohort1_data$ht_drsay
cohort2_data$ht_drsay <- cohort2_data$s3a2_hrtd1

#If Dr. said you have heart disease, what did doctor say it was?(others)
cohort1_data$ht_drsay_others<- cohort1_data$ht_drsay_others
cohort2_data$ht_drsay_others <- cohort2_data$s3a2_hrtd2

#At what age did you have your 1st heart attack(YYYY)?
cohort1_data$ht_ageattack1_yrs<- cohort1_data$ht_ageattack1_yrs
cohort2_data$ht_ageattack1_yrs <- cohort2_data$s3a2_hrtak1

#Were you hospitalized after your 1st heart attack?
cohort1_data$ht_trt_hospitalized<- cohort1_data$ht_trt_hospitalized
cohort2_data$ht_trt_hospitalized <- cohort2_data$s3a2_hrtht

#Did you have any repeat attacks?
cohort1_data$ht_repeatattack<- cohort1_data$ht_repeatattack
cohort2_data$ht_repeatattack <- cohort2_data$s3a2_reak

#Were you hospitalized for the subsequent attacks?
cohort1_data$ht_hosp_repeatattack<- cohort1_data$ht_hosp_repeatattack
cohort2_data$ht_hosp_repeatattack <- cohort2_data$s3a2_sbakhs

#Are you taking prescribed dietary modifications for your heart disease?
cohort1_data$ht_trt_dietmod<- cohort1_data$ht_trt_dietmod
cohort2_data$ht_trt_dietmod <- cohort2_data$s3a2_thrtdie

#Are you taking any prescribed physical exercise for your heart disease?
cohort1_data$ht_trt_phyexer<- cohort1_data$ht_trt_phyexer
cohort2_data$ht_trt_phyexer <- cohort2_data$s3a2_thrtex

#Are you taking any traditional medicine for your heart disease?
cohort1_data$ht_trt_tradmed<- cohort1_data$ht_trt_tradmed
cohort2_data$ht_trt_tradmed <- cohort2_data$s3a2_thrtth

#Are you taking any allopathic(modern) drugs for your heart disease?
cohort1_data$ht_trt_allopdrug<- cohort1_data$ht_trt_allopdrug
cohort2_data$ht_trt_allopdrug <- cohort2_data$s3a2_thrtdr

#Are you not taking any medication for your heart disease?
cohort1_data$ht_trt_none<- cohort1_data$ht_trt_none
cohort2_data$ht_trt_none <- cohort2_data$s3a2_thrtno

# }}}

# Stroke for Merging {{{ ====

#Have you ever had a stroke?
cohort1_data$pd_stroke<- cohort1_data$pd_stroke
cohort2_data$pd_stroke <- cohort2_data$s3a3_stk

#Is there a residual disability in any part of the body?
cohort1_data$st_res_disability<- cohort1_data$st_res_disability
cohort2_data$st_res_disability <- cohort2_data$s3a3_dabod

#If yes to residual disability, did it involve paralysis of leg/foot?
cohort1_data$st_paralysisleg<- cohort1_data$st_paralysisleg
cohort2_data$st_paralysisleg <- cohort2_data$s3a3_plg

#If yes to residual disability, did it involve paralysis of arm/hand?
cohort1_data$st_paralysisarm<- cohort1_data$st_paralysisarm
cohort2_data$st_paralysisarm <- cohort2_data$s3a3_phd

#If yes to residual disability, did it involve weakness of leg/foot?
cohort1_data$st_weakleg<- cohort1_data$st_weakleg
cohort2_data$st_weakleg <- cohort2_data$s3a3_wlg

#If yes to residual disability, did it involve weakness of arm/hand?
cohort1_data$st_weakarm<- cohort1_data$st_weakarm
cohort2_data$st_weakarm<- cohort2_data$s3a3_whd

#If yes to residual disability, did it involve defect of speech?
cohort1_data$st_defectspeech<- cohort1_data$st_defectspeech
cohort2_data$st_defectspeech <- cohort2_data$s3a3_dsch

#If yes to residual disability, did it involve defect of vison?
cohort1_data$st_defectvision<- cohort1_data$st_defectvision
cohort2_data$st_defectvision <- cohort2_data$s3a3_dvis

#If yes to residual disability, did it involve urinary incontinence?
cohort1_data$st_urineincont<- cohort1_data$st_urineincont
cohort2_data$st_urineincont <- cohort2_data$s3a3_urin

#Were you advised to continue any medication after your paralytic attack?
cohort1_data$st_advmedication<- cohort1_data$st_advmedication
cohort2_data$st_advmedication <- cohort2_data$s3a3_cmed

# }}}

# CKD for Merging {{{ ====

#Have you ever been told by a doctor you have kidney disease?
cohort1_data$pd_kidney<- cohort1_data$pd_kidney
cohort2_data$pd_kidney <- cohort2_data$s3a4_kddis

# }}}

# PVD for Merging {{{ ====

#Do you get pain in either leg on walking?
cohort1_data$pvd_painleg<- cohort1_data$pvd_painleg
cohort2_data$pvd_painleg<- cohort2_data$s3b_plw

#If pain in calves, what part of leg do you feel it?
cohort1_data$pvd_painpartleg<- cohort1_data$pvd_painpartleg
cohort2_data$pvd_painpartleg <- cohort2_data$s3b_plwpt

#do you get pain in your legs while climbing stairs or walking fast?
cohort1_data$pvd_painclimb<- cohort1_data$pvd_painclimb
cohort2_data$pvd_painclimb <- cohort2_data$s3b_clsta

#Do you get pain in your legs if you walk at an ordinary pace on level ground?
cohort1_data$pvd_painwalk<- cohort1_data$pvd_painwalk
cohort2_data$pvd_painwalk <- cohort2_data$s3b_lgro

#What happens to the pain in your legs if you stand still?
cohort1_data$pvd_painstand<- cohort1_data$pvd_painstand
cohort2_data$pvd_painstand <- cohort2_data$s3b_still

#If pain in leg is relieved, how soon <10 min or more than 10 mins)?
cohort1_data$pvd_painrelieve<- cohort1_data$pvd_painrelieve
cohort2_data$pvd_painrelieve <- cohort2_data$s3b_resoon

# }}}

# Diabetes complications for Merging {{{ ====

#Have you ever had a nonhealing ulcer that took more than 4 weeks to heal?
cohort1_data$amp_hadulcer<- cohort1_data$amp_hadulcer
cohort2_data$amp_hadulcer <- cohort2_data$s3d1_eulc

#Do you walk around barefoot?
cohort1_data$amp_barefoot<- cohort1_data$amp_barefoot
cohort2_data$amp_barefoot <- cohort2_data$s3d1_wbft

#Have you had an amputation?
cohort1_data$amp_hadamp <- cohort1_data$amp_hadamp
cohort2_data$amp_hadamp <- cohort2_data$s3d1_amp

#If had amputation, what year(YYYY)?
cohort1_data$amp_yrsbfor <- cohort1_data$amp_yrsbfor
cohort2_data$amp_yrsbfor <- cohort2_data$s3d1_ampy

#If had amputatoin, what month(MM)?
cohort1_data$amp_mnthsbfor<- cohort1_data$amp_mnthsbfor
cohort2_data$amp_mnthsbfor <- cohort2_data$s3d1_ampm

#If had amputation, what was the level of the amputation?
cohort1_data$amp_level<- cohort1_data$amp_level
cohort2_data$amp_level <- cohort2_data$s3d1_ample

#If had amputation, what was the cause of the amputation?
cohort1_data$amp_cause1<- cohort1_data$amp_cause1
cohort2_data$amp_cause1 <- cohort2_data$s3d1_ampcau

#If had amputation, do you have medical records or prescriptions?
cohort1_data$amp_medrecords <- cohort1_data$amp_medrecords
cohort2_data$amp_medrecords <- cohort2_data$s3d1_amprec

#If yes to having medical records, ask to see records and note diagnosis
cohort1_data$amp_diagnosis<- cohort1_data$amp_diagnosis
cohort2_data$amp_diagnosis<- cohort2_data$s3d1_ampdirec

#Do you have difficulty with your eyesight other than your ordinary power glasses(spectacles)?
cohort1_data$amp_eyesightdiff<- cohort1_data$amp_eyesightdiff
cohort2_data$amp_eyesightdiff <- cohort2_data$s3d2_eydif

#If yes to above, what was the diagnosis?(cataract, retinopathy, both, other)
cohort1_data$amp_eye_diagnosis<- cohort1_data$amp_eye_diagnosis
cohort2_data$amp_eye_diagnosis <- cohort2_data$s3d2_eydia

#Have you undergone laser therapy(Photocoagulation) at any time?
cohort1_data$amp_lasertherapy<- cohort1_data$amp_lasertherapy
cohort2_data$amp_lasertherapy <- cohort2_data$s3d2_eyrec

# }}}

# Outpatient treatment for Merging {{{ ====

#Are you undergoing treatment as an outpatient for heart disease?
cohort1_data$op_trt_hd<- cohort1_data$op_trt_hd
cohort2_data$op_trt_hd <- cohort2_data$s5a_hrt

#Are you undergoing treatment as an outpatient for stroke?
cohort1_data$op_trt_stroke<- cohort1_data$op_trt_stroke
cohort2_data$op_trt_stroke <- cohort2_data$s5a_st

#Are you undergoing treatment as an outpatient for diabetes?
cohort1_data$op_trt_diab <- cohort1_data$op_trt_diab
cohort2_data$op_trt_diab <- cohort2_data$s5a_dia

#Are you undergoing treatment as an outpatient for diabetic complications?
cohort1_data$op_trt_diabcomp<- cohort1_data$op_trt_diabcomp
cohort2_data$op_trt_diabcomp <- cohort2_data$s5a_com

#Are you undergoing treatment as an outpatient for high blood pressure?
cohort1_data$op_trt_hbp<- cohort1_data$op_trt_hbp
cohort2_data$op_trt_hbp <- cohort2_data$s5a_hbp

#Are you undergoing treatment as an outpatient for chronic kidney disease?
cohort1_data$op_trt_ckd<- cohort1_data$op_trt_ckd
cohort2_data$op_trt_ckd <- cohort2_data$s5a_ckd

# }}}

# Inpatient Hospitalizations for Merging {{{ ====

#Were you hospitalized for any illness in the past 12 months?
cohort1_data$ip_hosp_past<- cohort1_data$ip_hosp_past
cohort2_data$ip_hosp_past <- cohort2_data$s5b1_hoill

#If you were hospitalized in the past 12 months, how many times?
cohort1_data$ip_hosp_times<- cohort1_data$ip_hosp_times
cohort2_data$ip_hosp_times <- cohort2_data$s5b1_hoillti

#Were you admitted to the hospital in the last 12 months for heart disease?
cohort1_data$ip_admit_hd<- cohort1_data$ip_admit_hd
cohort2_data$ip_admit_hd <- cohort2_data$s5b1_rshrt

#Were you admitted to the hospital in the last 12 months for stroke?
cohort1_data$ip_admit_stroke<- cohort1_data$ip_admit_stroke
cohort2_data$ip_admit_stroke <- cohort2_data$s5b1_rs_str

#Were you admitted to the hospital in the last 12 months for diabetes?
cohort1_data$ip_admit_diab<- cohort1_data$ip_admit_diab
cohort2_data$ip_admit_diab <- cohort2_data$s5b1_rsdia

#Were you admitted to the hospital in the last 12 months for diabetes complications?
cohort1_data$ip_admit_diabcomp<- cohort1_data$ip_admit_diabcomp
cohort2_data$ip_admit_diabcomp <- cohort2_data$s5b1_rscomp

#Were you admitted to the hospital in the last 12 months for high blood pressure?
cohort1_data$ip_admit_hbp<- cohort1_data$ip_admit_hbp
cohort2_data$ip_admit_hbp <- cohort2_data$s5b1_rehbp

#Were you admitted to the hospital in the last 12 months for chronic kidney disease?
cohort1_data$ip_admit_ckd<- cohort1_data$ip_admit_ckd
cohort2_data$ip_admit_ckd <- cohort2_data$s5b1_reckd

#have you undergone any surgial procedure in the last 12 months?
cohort1_data$ip_surg_procedure<- cohort1_data$ip_surg_procedure
cohort2_data$ip_surg_procedure <- cohort2_data$s5b1_ugs_pro

#If yes, what was the procedure? Revascularization/bypass
cohort1_data$ip_surg_bypass<- cohort1_data$ip_surg_bypass
cohort2_data$ip_surg_bypass<- cohort2_data$s5b1_pran

#If yes, what was the procedure? valve repair/replacement
cohort1_data$ip_surg_valve<- cohort1_data$ip_surg_valve
cohort2_data$ip_surg_valve<- cohort2_data$s5b1_prre

#If yes, what was the procedure? Pacemaker
cohort1_data$ip_surg_pacemaker<- cohort1_data$ip_surg_pacemaker
cohort2_data$ip_surg_pacemaker<- cohort2_data$s5b1_prpac

#If yes, what was the procedure? amputation
cohort1_data$ip_surg_amputation<- cohort1_data$ip_surg_amputation
cohort2_data$ip_surg_amputation<- cohort2_data$s5b1_pramp

#If yes, what was the procedure? Abscess/ulcer
cohort1_data$ip_surg_abscess<- cohort1_data$ip_surg_abscess
cohort2_data$ip_surg_abscess<- cohort2_data$s5b1_prulc

#If yes, what was the procedure? renal transplant
cohort1_data$ip_surg_renal<- cohort1_data$ip_surg_renal
cohort2_data$ip_surg_renal<- cohort2_data$s5b1_prretrp

#If yes, what was the procedure? heart transplant
cohort1_data$ip_surg_hrttransplant<- cohort1_data$ip_surg_hrttransplant
cohort2_data$ip_surg_hrttransplant<- cohort2_data$s5b1_prhrttrp

#If yes, what was the procedure? retinal photocoagulation
cohort1_data$ip_surg_retinal<- cohort1_data$ip_surg_retinal
cohort2_data$ip_surg_retinal<- cohort2_data$s5b1_prlt

# }}}

# Hospital costs for Merging {{{ ====

#When were you hospitalized? (YYYY)
cohort1_data$hc1_hosp_yr<- cohort1_data$hc1_hosp_yr
cohort2_data$hc1_hosp_yr<- cohort2_data$s5b2_h1hosy

#When were you hospitalized? (MM)
cohort1_data$hc1_hosp_mon<- cohort1_data$hc1_hosp_mon
cohort2_data$hc1_hosp_mon<- cohort2_data$s5b2_h1hosmo

#How many days did you stay in the hospital?
cohort1_data$hc1_hospstay_days<- cohort1_data$hc1_hospstay_days
cohort2_data$hc1_hospstay_days<- cohort2_data$s5b2_h1hstd

#Type of hospital? (government)
cohort1_data$hc1_hosptype_govt<- cohort1_data$hc1_hosptype_govt
cohort2_data$hc1_hosptype_govt<- cohort2_data$s5b2_h1htyg

#Type of hospital? (private)
cohort1_data$hc1_hosptype_pvt<- cohort1_data$hc1_hosptype_pvt
cohort2_data$hc1_hosptype_pvt<- cohort2_data$s5b2_h1htyp

#Type of hospital? (charity)
cohort1_data$hc1_hosptype_chrty<- cohort1_data$hc1_hosptype_chrty
cohort2_data$hc1_hosptype_chrty<- cohort2_data$s5b2_h1htyc

#Type of hospital? (other)
cohort1_data$hc1_hosptype_othrs<- cohort1_data$hc1_hosptype_othrs
cohort2_data$hc1_hosptype_othrs<- cohort2_data$s5b2_h1htyoth

#What type of treatment/prcedure/surgery did you undergo? Medicines
cohort1_data$hc1_trt_medicine<- cohort1_data$hc1_trt_medicine
cohort2_data$hc1_trt_medicine<- cohort2_data$s5b2_h1tmed

#What type of treatment/prcedure/surgery did you undergo? Thrombolysis
cohort1_data$hc1_trt_thrombolysis<- cohort1_data$hc1_trt_thrombolysis
cohort2_data$hc1_trt_thrombolysis<- cohort2_data$s5b2_h1tth

#What type of treatment/prcedure/surgery did you undergo? angiogram
cohort1_data$hc1_trt_angiogram<- cohort1_data$hc1_trt_angiogram
cohort2_data$hc1_trt_angiogram<- cohort2_data$s5b2_h1tang

#What type of treatment/prcedure/surgery did you undergo? angioplasty
cohort1_data$hc1_trt_angioplasty<- cohort1_data$hc1_trt_angioplasty
cohort2_data$hc1_trt_angioplasty<- cohort2_data$s5b2_h1tapsy

#What type of treatment/prcedure/surgery did you undergo? bypass
cohort1_data$hc1_trt_bypass<- cohort1_data$hc1_trt_bypass
cohort2_data$hc1_trt_bypass<- cohort2_data$s5b2_h1tbps

#What type of treatment/prcedure/surgery did you undergo? brachy
cohort1_data$hc1_trt_brachy<- cohort1_data$hc1_trt_brachy
cohort2_data$hc1_trt_brachy<- cohort2_data$s5b2_h1tbch

#What type of treatment/prcedure/surgery did you undergo? pacemaker
cohort1_data$hc1_trt_pacemaker<- cohort1_data$hc1_trt_pacemaker
cohort2_data$hc1_trt_pacemaker<- cohort2_data$s5b2_h1tpmk

#What type of treatment/prcedure/surgery did you undergo? hrt transplant
cohort1_data$hc1_trt_hrttransplant<- cohort1_data$hc1_trt_hrttransplant
cohort2_data$hc1_trt_hrttransplant<- cohort2_data$s5b2_h1thrttrp

#What type of treatment/prcedure/surgery did you undergo? amputation
cohort1_data$hc1_trt_amputation<- cohort1_data$hc1_trt_amputation
cohort2_data$hc1_trt_amputation<- cohort2_data$s5b2_h1tamp

#What type of treatment/prcedure/surgery did you undergo? ecg
cohort1_data$hc1_trt_ecg<- cohort1_data$hc1_trt_ecg
cohort2_data$hc1_trt_ecg<- cohort2_data$s5b2_h1tecg

#What type of treatment/prcedure/surgery did you undergo? neuroimaging
cohort1_data$hc1_trt_neuroimaging<- cohort1_data$hc1_trt_neuroimaging
cohort2_data$hc1_trt_neuroimaging<- cohort2_data$s5b2_h1tneu

#What type of treatment/prcedure/surgery did you undergo? dialysis
cohort1_data$hc1_trt_dialysis<- cohort1_data$hc1_trt_dialysis
cohort2_data$hc1_trt_dialysis<- cohort2_data$s5b2_h1tdils

#What type of treatment/prcedure/surgery did you undergo? dialysis
cohort1_data$hc1_trt_dialysis<- cohort1_data$hc1_trt_dialysis
cohort2_data$hc1_trt_dialysis<- cohort2_data$s5b2_h1tdils

#What type of treatment/prcedure/surgery did you undergo? kidney transplant
cohort1_data$hc1_trt_kidneytransplant<- cohort1_data$hc1_trt_kidneytransplant
cohort2_data$hc1_trt_kidneytransplant<- cohort2_data$s5b2_h1tkdtrp

#What type of treatment/prcedure/surgery did you undergo? observation
cohort1_data$hc1_trt_observation<- cohort1_data$hc1_trt_observation
cohort2_data$hc1_trt_observation<- cohort2_data$s5b2_h1tobs

#What was the total amount spent on treatment(hosp expenses+ med purchased)?
cohort1_data$hc1_amount_spent<- cohort1_data$hc1_amount_spent
cohort2_data$hc1_amount_spent<- cohort2_data$s5b2_h1amots

#How did you pay for your hospitalization? personal savings
cohort1_data$hc1_pay_ownsave<- cohort1_data$hc1_pay_ownsave
cohort2_data$hc1_pay_ownsave<- cohort2_data$s5b2_h1pyown

#How did you pay for your hospitalization? family members paid
cohort1_data$hc1_pay_familymem<- cohort1_data$hc1_pay_familymem
cohort2_data$hc1_pay_familymem<- cohort2_data$s5b2_h1pyfam

#How did you pay for your hospitalization? employer paid
cohort1_data$hc1_pay_employer<- cohort1_data$hc1_pay_employer
cohort2_data$hc1_pay_employer<- cohort2_data$s5b2_h1pyem

#How did you pay for your hospitalization costs? borrowed from friends, relatives, employer
cohort1_data$hc1_pay_borrowfrnds<- cohort1_data$hc1_pay_borrowfrnds
cohort2_data$hc1_pay_borrowfrnds<- cohort2_data$s5b2_h1pyborf

#How did you pay for your hospitalization costs? borrowbank
cohort1_data$hc1_pay_borrowbank<- cohort1_data$hc1_pay_borrowbank
cohort2_data$hc1_pay_borrowbank<- cohort2_data$s5b2_h1pyborb

#How did you pay for your hospitalization costs? sold house, land, or assets
cohort1_data$hc1_pay_soldhome<- cohort1_data$hc1_pay_soldhome
cohort2_data$hc1_pay_soldhome<- cohort2_data$s5b2_h1pysoho

#How did you pay for your hospitalization costs? health insurance
cohort1_data$hc1_pay_hinsurance<- cohort1_data$hc1_pay_hinsurance
cohort2_data$hc1_pay_hinsurance<- cohort2_data$s5b2_h1pyhin

#How did you pay for your hospitalization costs? other
cohort1_data$hc1_pay_other<- cohort1_data$hc1_pay_other
cohort2_data$hc1_pay_other<- cohort2_data$s5b2_h1pyoth

#How did you pay for your hospitalization costs? Other specify
cohort1_data$hc1_pay_otherspecify<- cohort1_data$hc1_pay_otherspecify
cohort2_data$hc1_pay_otherspecify<- cohort2_data$s5b2_h1pyothos

# }}}

# Merging Cohorts {{{ ====

# Common variables for merging
svar <- c(
	"pid",
	"age",
	"sex",
	"edu_stat",
	"edu_yrs",
	"hh_income",
	"emp_stat",
	"curr_occ",
	"tob_everused",
	"tob_smkfrm",
	"tob_chwfrm",
	"tob_other",
	"tob_curcons",
	"tob_cursmkfrm",
	"tob_curchwfrm",
	"tob_curothfrm",
	"tob_cigdur_yrs",
	"tob_cigdur_mon",
	"tob_cig_permonth",
	"tob_bddur_yrs",
	"tob_bddur_mon",
	"tob_bd_permonth",
	"tob_cigardur_yrs",
	"tob_cigardur_mon",
	"tob_cigarusage",
	"tob_hukdur_yrs",
	"tob_hukdur_mon",
	"tob_huk_permonth",
	"tob_tchewdur_yrs",
	"tob_tchewdur_mon",
	"tob_tchew_permonth",
	"tob_pandur_yrs",
	"tob_pandur_mon",
	"tob_pan_permonth",
	"tob_panmasdur_yrs",
	"tob_panmasdur_mon",
	"tob_panmas_permonth",
	"tob_snuffdur_yrs",
	"tob_snuffdur_mon",
	"tob_snuff_permonth",
	"tob_gutkhadur_yrs",
	"tob_gutkhadur_mon",
	"tob_gutkha_permonth",
	"tob_smk_strtage",
	"tob_smkless_strtage",
	"tob_smkexpo",
	"tob_smkexpo_wk",
	"alc_everused",
	"alc_oftenuse",
	"alc_localdur_yrs",
	"alc_localfreq",
	"alc_localqty",
	"alc_spiritdur_yrs",
	"alc_spiritfreq",
	"alc_spiritqty",
	"alc_beerdur_yrs",
	"alc_beerfreq",
	"alc_beerqty",
	"alc_winedur_yrs",
	"alc_winefreq",
	"alc_wineqty",
	"pa_vigact_days",
	"pa_vigacttime_hr",
	"pa_vigacttime_min",
	"pa_modact_days",
	"pa_modacttime_hr",
	"pa_modacttime_min",
	"pa_walk_days",
	"pa_walktime_hr",
	"pa_walktime_min",
	"slp_hrswkdays",
	"slp_hrwkend",
	"slp_trblslp",
	"slp_nightdiff",
	"slp_feelunrest",
	"slp_notenough",
	"slp_pills",
	"dt_veg",
	"dt_egg",
	"dt_spldiet",
	"dt_diabdiet",
	"dt_wtreducediet",
	"pd_hbp",
	"hbp_since_yrs",
	"hbp_since_mon",
	"hbp_trt_dietmod",
	"hbp_trt_phyexer",
	"hbp_trt_tradmed",
	"hbp_trt_allopdrug",
	"hbp_trt_none",
	"pd_diabetes",
	"dia_since_yrs",
	"dia_since_mon",
	"dia_trt_dietmod",
	"dia_trt_phyexer",
	"dia_trt_tradmed",
	"dia_trt_allopdrug",
	"dia_trt_none",
	"pd_hyperlip",
	"hyp_since_yrs",
	"hyp_since_mon",
	"hyp_trt_dietmod",
	"hyp_trt_phyexer",
	"hyp_trt_tradmed",
	"hyp_trt_allopdrug",
	"hyp_trt_none",
	"pd_heart",
	"ht_first",
	"ht_drsay",
	"ht_drsay_others",
	"ht_ageattack1_yrs",
	"ht_trt_hospitalized",
	"ht_repeatattack",
	"ht_trt_dietmod",
	"ht_trt_phyexer",
	"ht_trt_tradmed",
	"ht_trt_allopdrug",
	"ht_trt_none",
	"pd_stroke",
	"st_res_disability",
	"st_paralysisleg",
	"st_paralysisarm",
	"st_weakleg",
	"st_weakarm",
	"st_defectspeech",
	"st_defectvision",
	"st_urineincont",
	"st_advmedication",
	"pd_kidney",
	"pvd_painleg",
	"pvd_painpartleg",
	"pvd_painclimb",
	"pvd_painwalk",
	"pvd_painstand",
	"pvd_painrelieve",
	"amp_hadulcer",
	"amp_barefoot",
	"amp_hadamp",
	"amp_yrsbfor",
	"amp_mnthsbfor",
	"amp_level",
	"amp_cause1",
	"amp_medrecords",
	"amp_diagnosis",
	"amp_eyesightdiff",
	"amp_eye_diagnosis",
	"amp_lasertherapy",
	"op_trt_hd",
	"op_trt_stroke",
	"op_trt_diab",
	"op_trt_diabcomp",
	"op_trt_hbp",
	"op_trt_ckd",
	"ip_hosp_past",
	"ip_hosp_times",
	"ip_admit_hd",
	"ip_admit_stroke",
	"ip_admit_diab",
	"ip_admit_diabcomp",
	"ip_admit_hbp",
	"ip_admit_ckd",
	"ip_surg_procedure",
	"ip_surg_bypass",
	"ip_surg_valve",
	"ip_surg_pacemaker",
	"ip_surg_amputation",
	"ip_surg_abscess",
	"ip_surg_renal",
	"ip_surg_hrttransplant",
	"ip_surg_retinal",
	"hc1_hosp_yr",
	"hc1_hosp_mon",
	"hc1_hospstay_days",
	"hc1_hosptype_govt",
	"hc1_hosptype_pvt",
	"hc1_hosptype_chrty",
	"hc1_hosptype_othrs",
	"hc1_trt_medicine",
	"hc1_trt_thrombolysis",
	"hc1_trt_angiogram",
	"hc1_trt_angioplasty",
	"hc1_trt_bypass",
	"hc1_trt_brachy",
	"hc1_trt_pacemaker",
	"hc1_trt_hrttransplant",
	"hc1_trt_amputation",
	"hc1_trt_ecg",
	"hc1_trt_neuroimaging",
	"hc1_trt_dialysis",
	"hc1_trt_kidneytransplant",
	"hc1_trt_observation",
	"hc1_amount_spent",
	"hc1_pay_ownsave",
	"hc1_pay_familymem",
	"hc1_pay_employer",
	"hc1_pay_borrowfrnds",
	"hc1_pay_borrowbank",
	"hc1_pay_soldhome",
	"hc1_pay_hinsurance",
	"hc1_pay_other",
	"hc1_pay_otherspecify"
)

# Merge both datasets will require them to have the same columns
# Then they can be merged by rows
c1_merge <- cohort1_data[svar]
c2_merge <- cohort2_data[svar]
combined_data <- bind_rows(c1_merge, c2_merge)

# }}}

#sample code to replace values when needed
#cohort1_data$tob_everused<- replace(cohort1_data$tob_everused, cohort1_data$tob_everused >2 & cohort1_data$tob_everused <4, 2)
