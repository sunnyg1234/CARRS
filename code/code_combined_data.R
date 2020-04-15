setwd(dir= "/Users/sunnygupta/CARRS/data")
cohort1_data<- read.csv("cohort1-short.csv")
cohort2_data<-read.csv("cohort2-short.csv")

library(tidyverse)

#This adds a column to the data for BMI using the height and weight with proper conversions
cohort1_data<- mutate(cohort1_data,BMI=(weight_kg/(height_cm/100)^2))

#This combines the two data sets. Everything from one data set that doesn't fit with the other is just NA
combined_data<-cohort1_data %>% full_join(cohort2_data, by= "pid")

#demographics- This combines the two columsn from the seperate data sets and removes NA if applicable. I'm having to pipe it one variable at a time but i wonder if theres a better way to do it. But this works. Let me know if you can think of a better way

demographics_combined <- combined_data %>% mutate_all(as.character) %>% tidyr::unite("pd_age", s1a_age, pd_age, sep="", na.rm=TRUE) %>% unite("pd_sex", s1a_sex, pd_sex, sep= "", na.rm=TRUE) %>% unite("pd_edu_stat", s1a_est, pd_edu_stat, sep= "", na.rm=TRUE) %>% unite("pd_edu_yrs", s1a_fedu, pd_edu_yrs, sep="", na.rm=TRUE) %>% unite("pd_hhincome", pd_hhincome, s1a_toin, sep= "", na.rm=TRUE) %>% unite("pd_emp_stat", pd_emp_stat, s1a_emst, sep= "", na.rm=TRUE) %>% unite("pd_cur_occu", pd_cur_occu, s1a_coc, sep= "", na.rm=TRUE) %>% select(pid,pd_age,pd_sex, pd_edu_stat,pd_edu_yrs,pd_hhincome, pd_emp_stat, pd_cur_occu, pd_hbp, pd_diabetes,pd_hyperlip, pd_heart )


