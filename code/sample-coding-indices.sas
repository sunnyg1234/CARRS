s/********************************************
* OBESITY INDICES CODING BOOK *
* SPRING 2020 *
********************************************/

Proc import out=CARRS Datafile="C:\Users\npoveda\Documents\Emory\Second year\Research Rotation 2\CARRS12345_01Oct2019.dta";
Run;

Proc contents Data=CARRS;
Run;

ods excel file="C:\Users\npoveda\Box Sync\Natalia Poveda\SAS Analysis\CARRS_SAS_CODEBOOK.xlsx";
Proc contents Data=CARRS order=varnum;
Run;
ods excel close;

*Temporary;
Data "C:\Users\npoveda\Documents\Emory\Second year\Research Rotation 2\CARRS_SAS";
set carrs;
run;

Proc means data=carrs;
Var height_cm f4_ht;
run;


/***** COVARIATES RECODING*****/

* Covariates Baseline;

* 1. Sex;
Data carrs1;
Set carrs;
male = .; *male = 1, female =0, transgender person is missing on this variable;
if pd_sex = 1 then male =1;
if pd_sex = 2 then male =0;

* 2. Age;
agecat3 = .;
if pd_age le 44 then agecat3=0;
if pd_age ge 45 and pd_age le 64 then agecat3 =1;
if pd_age ge 65 then agecat3 = 2;

* 3. Education;
highestedu4 = .; *8 gets coded as missing (probably data entry error);
if pd_edu_stat in (6 7) then highestedu4 =0; *no education;
if pd_edu_stat in (5) then highestedu4 = 1; *primary school;
if pd_edu_stat in (3 4) then highestedu4 = 2; *high school or secondary school;
if pd_edu_stat in (1 2) then highestedu4 = 3; *college;

*three category highest education;
if highestedu4 in (0 1) then  highestedu3 = 0; *no schooling or some primary;
if highestedu4 in (2) then  highestedu3 = 1; *finished high school;
if highestedu4 in (3) then  highestedu3 = 2; *college graduate or higher;

* 4. Income;
incomegt10k =.; *0=missing in this case;
if pd_hhincome in (1 2) then incomegt10k = 0;
if pd_hhincome in (3 4 5 6 7) then incomegt10k =1;

* 5. Smoking;
tobaccoever =.;
if tob_everused = 1 then tobaccoever = 1;
if tob_everused = 2 then tobaccoever = 0;

tobaccocurrent = .;
if tob_curcons = 1 then tobaccocurrent = 1;
if tob_curcons = 2 then tobaccocurrent = 0;
if tob_everused = 2 then tobaccocurrent = 0;

tobaccocat = .;
if tobaccoever = 0 and tobaccocurrent = 0 then tobaccocat = 0;
if tobaccoever = 1 and tobaccocurrent = 0 then tobaccocat = 1;
if tobaccoever = 1 and tobaccocurrent = 1 then tobaccocat = 2;

* 6. Alcohol consumption;
alcoholcurrent = .;
if alc_oftenuse in (1 2) then alcoholcurrent = 1; *current drinker;
if alc_oftenuse in (0 3 4 5) then alcoholcurrent = 0; *past or never drinker;

alcoholever = .;
if alc_everused in (1) then alcoholever = 1; *ever drinker;
if alc_everused in (2) then alcoholever = 0; *never drinker;

alcoholcat2 = .;
if alc_everused = 2 then alcoholcat2 = 0; *never user;
if alc_oftenuse in (2 3 4) then alcoholcat2 = 1; *past user or current occasional drinker;
if alc_oftenuse = 1 then alcoholcat2 = 2; *regular user;

* 7. Physical activity;
sedentarymins = pa_sit_wkday_hr*60 + pa_sit_wkday_min;
if pa_sit_wkday in (2 3) then sedentarymins = .;

sedentaryhrs = sedentarymins/60;

if sedentarymins => 300 then sedentary01 = 1; *more than 5 hours of sedentary behavior per day;
if sedentarymins < 300 then sedentary01 = 0; *less than 5 hours of sedentary behavior per day;

* 8. Religion;
religion3 =.;
if pd_relig =1 then religion3 =0; *hindu;
if pd_relig = 2 then religion3 =1; *Muslim;
if pd_relig in (3 4 5 6) then religion3 =2; *other;
if pd_relig = 0 then religion3 = .;

* 9. Employment;
employ4 =.;
if pd_emp_stat in (2 3 4 5) then employ4 =0; *Not Working (housewife, student, retired);
if pd_emp_stat = 1 and pd_cur_occu in (5) then employ4 =1;
	*Unskilled manual labourer, landless labourer;
if pd_emp_stat = 1 and pd_cur_occu in (3 4) then employ4 =2;
	*3:Skilled manual labourer, small business owner, small farmer,
	4:Semi-skilled manual labourer, marginal landowner, rickshaw driver, army jawan, carpenter, fitter ;
if pd_emp_stat = 1 and pd_cur_occu in (1 2) then employ4 =3;
	*1: Professional, big business, landlord, university teacher, class 1 IAS/services officer, lawyer
	2: Trained, clerical, medium business owner, middle level farmer, teacher, maintenance (in charge), personnel manager ;

* 10. Asset recoding;
array aassets1  pd_tv pd_fridge pd_wmachine pd_micro pd_grinder v56 pd_dvd pd_computer pd_car pd_bike pd_cycle;
array aassets2  tv fridge washer micro grinder mobile dvd computer car bike cycle;

do over aassets1; *setting asset values of "0" to missing;
if aassets1= 0 then aassets1 = .;
end;

do over aassets1; *creating 0-1 variables for assets, fuel and water recoded better in next step;
if aassets1=1 then aassets2= 1;
if aassets1=2 then aassets2= 0;
end;

assetsum = sum(tv, fridge, washer, micro, grinder, mobile, dvd, computer, car, bike, cycle);

if assetsum in (0 1 2 3 4) then assetsumtert = 0;
if assetsum in (5 6 7) then assetsumtert = 1;
if assetsum in (8 9 10 11) then assetsumtert = 2;

* EXPOSURES RECODING *

***** Anthropometric measures Baseline ****
* 1. Height;
heightm = height_cm/100;
if height_cm in (.) or  height_cm > 9999 then heightm = .;

* 2. Weight;
weightkg = weight_kg;
if weight_kg in (.) or  weight_kg > 9999 then weightkg = .;

* 3. BMI;
bmi = weightkg/ (heightm * heightm);

if bmi < 23 then bmicat = 0;
if bmi => 23 and bmi < 25 then bmicat = 1;
if bmi => 25 and bmi < 30 then bmicat = 2;
if bmi => 30 			  then bmicat = 3;
if bmi = . 				  then bmicat = .;

if bmicat in (0 1 2) then obese = 0;
if bmicat = 3 then obese = 1;

if bmicat in (0 1) then obese_asian = 0;
if bmicat in (2 3) then obese_asian = 1;


skipstart**********************************
* 4. Waist circumference and hip circumference;
array aanthro
height_cm
waist_cm
hip_cm
;

array aanthro2
height
waist
hip
;

do over aanthro;
aanthro2 = aanthro;
if aanthro > 9999 or aanthro = . then aanthro2 =.;
end;

* 5. Skinfolds;
array askinfold
st_triceps_1-st_triceps_3
st_ss_1-st_ss_3 /*Note that this says suprascapular but this is really subscapular*/
sp_ss_1-sp_ss_3
;

array askinfold2
triceps1-triceps3
subscap1-subscap3
suprapat1-suprapat3
;

do over askinfold;
askinfold2 = askinfold;
if askinfold > 9999 or askinfold = . then askinfold2 =.;
end;

tricepsmean=mean(of triceps1-triceps3);
subscapmean=mean(of subscap1-subscap3);
suprapatmean=mean(of suprapat1-suprapat3);

skinfoldsum = tricepsmean + subscapmean +suprapatmean;

skinfoldlog = log(tricepsmean + subscapmean + suprapatmean);

* 6. Body fat percentage based on Durnin and Womersely 1974 equations;
        *first, sum of the two skinfolds we have - tricep and subscapular;
		trisubsumlog = log(tricepsmean + subscapmean);

		*next, density for males;
		if male = 1 and 20<=pd_age<=29 then density = 1.1525 - 0.0687 * trisubsumlog;
		if male = 1 and 30<=pd_age<=39 then density = 1.1165 - 0.0484 * trisubsumlog;
		if male = 1 and 40<=pd_age<=49 then density = 1.1519 - 0.0771 * trisubsumlog;
		if male = 1 and 50<=pd_age     then density = 1.1527 - 0.0793 * trisubsumlog;

		*next, density for females;
		if male = 0 and 20<=pd_age<=29 then density = 1.1582 - 0.0813 * trisubsumlog;
		if male = 0 and 30<=pd_age<=39 then density = 1.1356 - 0.0680 * trisubsumlog;
		if male = 0 and 40<=pd_age<=49 then density = 1.1230 - 0.0635 * trisubsumlog;
		if male = 0 and 50<=pd_age     then density = 1.1347 - 0.0742 * trisubsumlog;

		*finally, compute body fat;
		fatdurnin= (495/density - 450) * 100;

skipend**********************************


* 7. WHtR (Waist-height ratio);
waistheightr = waist/height;

highwaistheight = .;
if waistheightr >.5 then highwaistheight =1;
if 0<waistheightr <= .5 then highwaistheight =0;

* 8. WHR (Waist-hip ratio);

waisthipr = waist/hip;

highwaisthip =.;
if waisthipr >.8 then highwaisthip =1;
if waisthipr <=.8 then highwaisthip =0;
if waisthipr =.  then highwaisthip =.;

* 9. High waist circumference;
highwaist=0;
if male =1 and waist => 90 then highwaist = 1;
if male =0 and waist => 80 then highwaist = 1;
if waist = . then highwaist = .;

* 10. BAI (Body Adiposity Index);
bai = (hip/(heightm**1.5))-18;

skipstart**********************************
* 11. VAI (Visceral Adiposity Index);
 * TG and HDL-C are expressed in mmol/L. For triglycerides divide mg/dL by 88.57 and
   For total, HDL, and LDL cholesterol divide mg/dL by 38.67;
tgmmol =  lab_triglyc/88.57;
hdlmmol = lab_hdlchol/38.67;

vai = .;
if male=1 then vai=(waist/(39.68+(1.88*bmi)))*(tgmmol/1.03)*(1.31/hdlmmol);
if male=0 then vai=(waist/(36.58+(1.89*bmi)))*(tgmmol/0.81)*(1.52/hdlmmol);

*Missing indicator for height and weight;
missinghtwt=0;
if (height_cm >9999 or height_cm = .) and (weight_kg >9999 or weight_kg = .)  then missinghtwt = 1;

missinghtwt2=0;
if (height_cm >9999 ) and (weight_kg >9999)  then missinghtwt2 = 1;

* OUTCOMES RECODING;

***** Lab and clinical measures Baseline ****
*1. Blood pressure;
array abp systolic_bp_first systolic_bp_second systolic_bp_third diastolic_bp_first diastolic_bp_second diastolic_bp_third;
array abp2 sbp1-sbp3 dbp1-dbp3;

do over abp;
abp2 = abp;
if abp > 300 then abp2 =.;
end;

sbp_mean = mean(sbp1, sbp2, sbp3);
dbp_mean = mean(dbp1, dbp2, dbp3);

skipend**********************************

* 2. Hypertension;
hypertension=.;
if sbp_mean => 140 or dbp_mean => 90 or hbp_trt_allopdrug=1  then hypertension = 1;
if .<sbp_mean < 140 and .<dbp_mean < 90 and hbp_trt_allopdrug in (0 2 .)  then hypertension = 0;

 *New cut-offs for hypertension;
hypertension1=.;
if sbp_mean => 130 or dbp_mean => 80 or hbp_trt_allopdrug=1  then hypertension1 = 1;
if .<sbp_mean < 130 and .<dbp_mean < 80 and hbp_trt_allopdrug in (0 2 .)  then hypertension1 = 0;


* 3. Diabetes;
diabetes = 0;
if lab_fasting =>126 or lab_HbA1c => 6.5 or dia_trt_allopdrug = 1 then diabetes = 1;
if lab_fasting = . and lab_HbA1c = . then diabetes = .;

 *Prediabetes;
prediabetes = 0;
if lab_fasting =>100 or dia_trt_allopdrug = 1 then prediabetes = 1;
if lab_fasting = . then prediabetes = .;

* 4. High total cholesterol;
if lab_tchol => 200 or hyp_trt_allopdrug=1 then hightc = 1; *only for high total cholesterol, adding in medication;
if lab_tchol < 200 then hightc = 0;
if lab_tchol = . then hightc = .;

* 5. High LDL-cholesterol;
if lab_ldlchol => 130 then highldl = 1;
if lab_ldlchol < 130 then highldl = 0;
if lab_ldlchol =. then highldl = .;

* 6. Low HDL-cholesterol;
if pd_sex = 1 and lab_hdlchol <=40 then lowhdl = 1;
if pd_sex = 1 and lab_hdlchol > 40 then lowhdl = 0;
if pd_sex = 2 and lab_hdlchol <=50 then lowhdl = 1;
if pd_sex = 2 and lab_hdlchol > 50 then lowhdl = 0;
if lab_hdlchol =.  then lowhdl = .;

* 7. High triglycerides levels;
if lab_triglyc => 150 then hightg = 1;
if lab_triglyc < 150 then hightg = 0;
if lab_triglyc = . then hightg = .;


skipstart**********************************
* 8. Self-reported outcomes;
array aself pd_hbp  pd_diabetes pd_hyperlip pd_heart pd_stroke pd_kidney hf_docsaycopd; *hf_docsaycopd comes from the heart failure questions;
array aself2 highbpself diabself hyperlipself heartself strokeself kidneyself copdself; *self-reported outcomes;

do over aself;
if aself = 1 then aself2 = 1;
if aself = 2 then aself2 = 0;
if aself in (0 .) then aself2 = .;
end;

skipend ****************************
* 9. Metabolic syndrome;
metsyn = 0;
if male=1 and sum(highwaist, hypertension1, prediabetes, hightg, lowhdl) =>3 then metsyn=1;
if male=1 and nmiss (highwaist, hypertension1, prediabetes, hightg, lowhdl)=>3 then metsyn=.;
if male=0 and sum(highwaist, hypertension1, prediabetes, hightg, lowhdl) =>3 then metsyn=1;
if male=0 and nmiss (highwaist, hypertension1, prediabetes, hightg, lowhdl)=>3 then metsyn=.;

* 10. CVD Score;
* I need the code for this;

* Missing indicator for lab measurements;
labmissn =nmiss (lab_fasting, lab_tchol, lab_ldlchol, lab_hdlchol, lab_triglyc, lab_HbA1c);

if labmissn > 0 then labmissbin = 1;
if labmissn = 0 then labmissbin =0;

if labmissn > 1 then labmiss = 1; else labmiss = 0;

anymiss = 0;
if nmiss(highestedu4, lab_fasting, lab_tchol, lab_ldlchol, lab_hdlchol, lab_triglyc, lab_HbA1c, height, waist) >0 then anymiss=1;


***** Lab and clinical measures Visit 2 *****;
male2 = .;
if sex=1 then male2=1;
if sex=2 then male2=0; /*1 = male, 0=female*/

*1. Blood pressure visit 2;
array abp21 sbp_fu2_1 sbp_fu2_2 sbp_fu2_3 dbp_fu2_1 dbp_fu2_2 dbp_fu2_3;
array abp22 sbp21-sbp23 dbp21-dbp23;

do over abp21;
abp22 = abp21;
if abp21 > 300 then abp22 =.;
end;

sbp2_mean = mean(sbp21, sbp22, sbp23);
dbp2_mean = mean(dbp21, dbp22, dbp23);

* 2. Hypertension visit 2;
hypertension2=.;
if sbp2_mean => 140 or dbp2_mean => 90 or hbp_allopathic=1  then hypertension2 = 1;
if .< sbp2_mean < 140 and .< dbp2_mean < 90 and hbp_allopathic in (0 2 .)  then hypertension2 = 0;

 *New cut-offs for hypertension visit 2;
hypertension21=.;
if sbp2_mean => 130 or dbp2_mean => 80 or hbp_allopathic=1  then hypertension21 = 1;
if .<sbp2_mean < 130 and .<dbp2_mean < 80 and hbp_allopathic in (0 2 .)  then hypertension21 = 0;

* 3. Diabetes visit 2;
diabetes2 = 0;
if f2_fpg =>126 or f2_hba1c => 6.5 or dia_allopathic = 1 then diabetes2 = 1;
if f2_fpg = . and f2_hba1c = . then diabetes2 = .;

 *Prediabetes visit 2;
prediabetes2 = 0;
if f2_fpg =>100 or dia_allopathic = 1 then prediabetes2 = 1;
if f2_fpg = . then prediabetes2 = .;

* 4. High total cholesterol visit 2;
if f2_tchol => 200 or hyper_allopathic=1 then hightc2 = 1; *only for high total cholesterol, adding in medication;
if f2_tchol < 200 then hightc2 = 0;
if f2_tchol = . then hightc2 = .;

* 5. High LDL-cholesterol visit 2;
if f2_ldl => 130 then highldl2 = 1;
if f2_ldl < 130 then highldl2 = 0;
if f2_ldl =. then highldl2 = .;

* 6. Low HDL-cholesterol visit 2;
if sex = 1 and f2_hdl <=40 then lowhdl2 = 1;
if sex = 1 and f2_hdl > 40 then lowhdl2 = 0;
if sex = 2 and f2_hdl <=50 then lowhdl2 = 1;
if sex = 2 and f2_hdl > 50 then lowhdl2 = 0;
if f2_hdl =.  then lowhdl2 = .;

* 7. High triglycerides levels visit 2;
if f2_trigly => 150 then hightg2 = 1;
if f2_trigly < 150 then hightg2 = 0;
if f2_trigly = . then hightg2 = .;

* 8. Self-reported outcomes visit 2;
array aself21 mh_hbp  mh_diab mh_hyper mh_heart mh_stroke kd_disease;
array aself22 highbpself2 diabself2 hyperlipself2 heartself2 strokeself2 kidneyself2; *self-reported outcomes;

do over aself21;
if aself21 = 1 then aself22 = 1;
if aself21 = 2 then aself22 = 0;
if aself21 in (0 .) then aself22 = .;
end;

* 9. Metabolic syndrome visit 2;
highwaist2=0;
if male2 =1 and waist_fu2 => 90 then highwaist2 = 1;
if male2 =0 and waist_fu2 => 80 then highwaist2 = 1;
if waist_fu2 = . then highwaist2 = .;

metsyn2 = 0;
if male2=1 and sum(highwaist2, hypertension21, prediabetes2, hightg2, lowhdl2) =>3 then metsyn2=1;
if male2=1 and nmiss (highwaist2, hypertension21, prediabetes2, hightg2, lowhdl2)=>3 then metsyn2=.;
if male2=0 and sum(highwaist2, hypertension21, prediabetes2, hightg2, lowhdl2) =>3 then metsyn2=1;
if male2=0 and nmiss (highwaist2, hypertension21, prediabetes2, hightg2, lowhdl2)=>3 then metsyn2=.;

* 10. CVD Score visit 2;

* Missing indicator for lab measurements visit 2;
labmissn2 = nmiss (f2_fpg, f2_tchol, f2_ldl, f2_hdl, f2_vldl, f2_trigly, f2_hba1c);

if labmissn2 > 0 then labmissbin2 = 1;
if labmissn2 = 0 then labmissbin2 =0;

if labmissn2 > 1 then labmiss2 = 1; else labmiss2 = 0;

anymiss2 = 0;
if nmiss(f2_fpg, f2_tchol, f2_ldl, f2_hdl, f2_vldl, f2_trigly, f2_hba1c) >0 then anymiss2=1;


***** Lab and clinical measures Visit 4 *****;
male4 = .;
if f4_t_gender=1 then male4=1;
if f4_t_gender=2 then male4=0; /*1 = male, 0=female*/

*1. Blood pressure visit 4;
array abp41 f4_bp_s1 f4_bp_s2 f4_bp_s3 f4_bp_d1 f4_bp_d2 f4_bp_d3;
array abp42 sbp41-sbp43 dbp41-dbp43;

do over abp41;
abp42 = abp41;
if abp41 > 300 then abp42 =.;
end;

sbp4_mean = mean(sbp41, sbp42, sbp43);
dbp4_mean = mean(dbp41, dbp42, dbp43);

* 2. Hypertension visit 4;
hypertension4=.;
if sbp4_mean => 140 or dbp4_mean => 90 or hbp_trt_all=1  then hypertension4 = 1;
if .<sbp4_mean < 140 and .<dbp4_mean < 90 and hbp_trt_all in (0 2 .)  then hypertension4 = 0;

 *New cut-offs for hypertension visit 4;
hypertension41=.;
if sbp4_mean => 130 or dbp4_mean => 80 or hbp_trt_all=1  then hypertension41 = 1;
if .<sbp4_mean < 130 and .<dbp4_mean < 80 and hbp_trt_all in (0 2 .)  then hypertension41 = 0;

* 3. Diabetes visit 4;
diabetes4 = 0;
if fpg =>126 or hba1c => 6.5 or dia_trt_all= 1 then diabetes4 = 1;
if fpg = . and hba1c = . then diabetes4 = .;

 *Prediabetes visit 4;
prediabetes4 = 0;
if fpg =>100 or dia_trt_all = 1 then prediabetes4 = 1;
if fpg = . then prediabetes4 = .;

* 4. High total cholesterol visit 4;
if chol => 200 or hyle_trt_all=1 then hightc4 = 1; *only for high total cholesterol, adding in medication;
if chol < 200 then hightc4 = 0;
if chol = . then hightc4 = .;

* 5. High LDL-cholesterol visit 4;
if ldl => 130 then highldl4 = 1;
if ldl < 130 then highldl4 = 0;
if ldl =. then highldl4 = .;

* 6. Low HDL-cholesterol visit 4;
if f4_t_gender = 1 and hdl <=40 then lowhdl4 = 1;
if f4_t_gender = 1 and hdl > 40 then lowhdl4 = 0;
if f4_t_gender = 2 and hdl <=50 then lowhdl4 = 1;
if f4_t_gender = 2 and hdl > 50 then lowhdl4 = 0;
if hdl =.  then hdl4 = .;

* 7. High triglycerides levels visit 4;
if tg => 150 then hightg4 = 1;
if tg < 150 then hightg4 = 0;
if tg = . then hightg4 = .;

* 8. Self-reported outcomes visit 4;
array aself41 hbp_dis dia_dis hyle_dis hrt_dis stroke kd_dis;
array aself42 highbpself4 diabself4 hyperlipself4 heartself4 strokeself4 kidneyself4; *self-reported outcomes;

do over aself41;
if aself41 = 1 then aself42 = 1;
if aself41 = 2 then aself42 = 0;
if aself41 in (0 .) then aself42 = .;
end;

* 9. Metabolic syndrome visit 4;
highwaist4=0;
if male4 =1 and f4_bp_waist => 90 then highwaist4 = 1;
if male4 =0 and f4_bp_waist => 80 then highwaist4 = 1;
if f4_bp_waist = . then highwaist2 = .;

metsyn4 = 0;
if male4=1 and sum(highwaist4, hypertension41, prediabetes4, hightg4, lowhdl4) =>3 then metsyn4=1;
if male4=1 and nmiss (highwaist4, hypertension41, prediabetes4, hightg4, lowhdl4)=>3 then metsyn4=.;
if male4=0 and sum(highwaist4, hypertension41, prediabetes4, hightg4, lowhdl4) =>3 then metsyn4=1;
if male4=0 and nmiss (highwaist4, hypertension41, prediabetes4, hightg4, lowhdl4)=>3 then metsyn4=.;

* 10. CVD Score visit 4;
* I need the code;

* Missing indicator for lab measurements visit 4;
labmissn4 = nmiss (fpg, chol, ldl, hdl, vldl, tg, hba1c);

if labmissn4 > 0 then labmissbin4 = 1;
if labmissn4 = 0 then labmissbin4 =0;

if labmissn4 > 1 then labmiss4 = 1; else labmiss4 = 0;

anymiss4 = 0;
if nmiss(fpg, chol, ldl, hdl, tg, hba1c) >0 then anymiss4=1;
run;


**********************************
/***** DATA CHECKING RECODING *****/;

* Missing data;
 * Missing Baseline;
Proc freq data=carrs1;
Table missinghtwt*city missinghtwt2*city/nocol norow nopercent;
run;

Proc freq data=carrs1;
Table male*missinghtwt*city male*missinghtwt2*city/nocol norow nopercent;
run;

Proc freq data=carrs1;
Table labmissn*city/nocol norow nopercent;
run;

Proc freq data=carrs1;
Table male*labmissn*city/nocol norow nopercent;
run;

 * Missing Visit 2;
Proc freq data=carrs1;
Table labmissn2*city/nocol norow nopercent;
run;

Proc freq data=carrs1;
Table male2*labmissn2*city/nocol norow nopercent;
run;

 * Missing Visit 4;
Proc freq data=carrs1;
Table labmissn4*city/nocol norow nopercent;
run;

Proc freq data=carrs1;
Table male4*labmissn2*city/nocol norow nopercent;
run;

* Checking original variables_Baseline;
Proc freq data=carrs1;
Table pd_sex pd_age pd_edu_stat pd_hhincome
tob_everused tob_curcons alc_everused alc_oftenuse
pa_sit_wkday
hbp_trt_allopdrug dia_trt_allopdrug hyp_trt_allopdrug
pd_hbp  pd_diabetes pd_hyperlip pd_heart pd_stroke pd_kidney/missing;
run;

Proc means data=carrs1;
Var pa_sit_wkday_hr pa_sit_wkday_min
height_cm weight_kg waist_cm hip_cm st_triceps_1 st_triceps_2 st_triceps_3
st_ss_1 st_ss_2 st_ss_3 sp_ss_1 sp_ss_2 sp_ss_3
systolic_bp_first systolic_bp_second systolic_bp_third
diastolic_bp_first diastolic_bp_second diastolic_bp_third
lab_fasting
lab_HbA1c
lab_tchol
lab_ldlchol
lab_hdlchol
lab_vldlchol
lab_triglyc;
run;

Proc freq data=carrs1;
Table pd_sex*male
pd_edu_stat*highestedu4
highestedu4*highestedu3
pd_hhincome*incomegt10k
tob_everused*tobaccoever
tob_curcons*tobaccocurrent
alc_everused*alcoholcurrent
alc_everused*alcoholever
alc_oftenuse*alcoholcat2
alc_everused*alcoholcat2
sedentarymins*sedentary01
hbp_trt_allopdrug*hypertension
hbp_trt_allopdrug*hypertension1
bmicat*obese
bmicat*obese_asian
pd_hbp*highbpself
pd_diabetes*diabself
pd_hyperlip*hyperlipself
pd_heart*heartself
pd_stroke*strokeself
pd_kidney*kidneyself
hf_docsaycopd*copdself
/nocol norow nopercent;
run;

Proc means data=carrs1;
class agecat3;
var pd_age;
run;

Proc means data=carrs1;
class bmicat;
var bmi;
run;

Proc means data=carrs1;
class highwaisthip;
var waisthipr;
run;

Proc means data=carrs1;
class highwaistheight;
var waistheightr;
run;

Proc means data=carrs1;
Var height weightkg bmi waist hip tricepsmean subscapmean suprapatmean
skinfoldsum skinfoldlog bai lab_triglyc tgmmol lab_hdlchol hdlmmol vai;
run;

Proc means data=carrs1;
class hypertension;
var sbp_mean dbp_mean;
run;

Proc means data=carrs1;
class hypertension1;
var sbp_mean dbp_mean;
run;

Proc means data=carrs1;
class diabetes;
var lab_fasting lab_hba1c;
run;

Proc means data=carrs1;
class hightc;
var lab_tchol;
run;

Proc means data=carrs1;
class lowhdl male;
var lab_hdlchol;
run;

Proc means data=carrs1;
class highldl;
var lab_ldlchol;
run;

Proc means data=carrs1;
class hightg;
var lab_triglyc;
run;

Proc freq data=carrs1;
table diabetes hightc hypertension;
run;

Proc freq data=carrs1;
table diabetes hightc hypertension diabetes*hightc*hypertension/norow  nopercent;
run;

Proc freq data=carrs1;
table metsyn;
run;

* Checking original variables_Visit 2;
Proc freq data=carrs1;
Table sex
hbp_allopathic dia_allopathic hyper_allopathic
mh_hbp mh_diab mh_hyper mh_heart mh_stroke kd_disease;
run;

Proc means data=carrs1;
Class city;
Var sbp_fu2_1 sbp_fu2_2 sbp_fu2_3 dbp_fu2_1 dbp_fu2_2 dbp_fu2_3
f2_fpg
f2_hba1c
f2_tchol
f2_ldl
f2_hdl
f2_vldl
f2_trigly;
run;

Proc freq data=carrs1;
Table sex*male2
hbp_allopathic*hypertension2
hbp_allopathic*hypertension21
mh_hbp*highbpself2
mh_diab*diabself2
mh_hyper*hyperlipself2
mh_heart*heartself2
mh_stroke*strokeself2
kd_disease*kidneyself2
/nocol norow nopercent;
run;

Proc means data=carrs1;
class hypertension2;
var sbp2_mean dbp2_mean;
run;

Proc means data=carrs1;
class hypertension21;
var sbp2_mean dbp2_mean;
run;

Proc means data=carrs1;
class diabetes2;
var f2_fpg f2_hba1c;
run;

Proc means data=carrs1;
class hightc2;
var f2_tchol;
run;

Proc means data=carrs1;
class lowhdl2 male2;
var f2_hdl;
run;

Proc means data=carrs1;
class highldl2;
var f2_ldl;
run;

Proc means data=carrs1;
class hightg2;
var f2_trigly;
run;

Proc freq data=carrs1;
table diabetes2 hightc2 hypertension2;
run;

Proc freq data=carrs1;
table diabetes2 hightc2 hypertension2 diabetes2*hightc2*hypertension2/norow  nopercent;
run;

Proc freq data=carrs1;
table metsyn2;
run;

* Checking original variables_Visit 4;
Proc freq data=carrs1;
Table f4_t_gender
hbp_trt_all dia_trt_all hyle_trt_all
hbp_dis dia_dis hyle_dis hrt_dis stroke kd_dis;
run;

Proc means data=carrs1;
Class city;
Var f4_bp_s1 f4_bp_s2 f4_bp_s3 f4_bp_d1 f4_bp_d2 f4_bp_d3
fpg
hba1c
chol
ldl
hdl
vldl
tg;
run;

Proc freq data=carrs1;
Table f4_t_gender*male4
hbp_trt_all*hypertension4
hbp_trt_all*hypertension41
hbp_dis*highbpself4
dia_dis*diabself4
hyle_dis*hyperlipself4
hrt_dis*heartself4
stroke*strokeself4
kd_dis*kidneyself4
/nocol norow nopercent;
run;

Proc means data=carrs1;
class hypertension4;
var sbp4_mean dbp4_mean;
run;

Proc means data=carrs1;
class hypertension41;
var sbp4_mean dbp4_mean;
run;

Proc means data=carrs1;
class diabetes4;
var fpg hba1c;
run;

Proc means data=carrs1;
class hightc4;
var chol;
run;

Proc means data=carrs1;
class lowhdl4 male4;
var hdl;
run;

Proc means data=carrs1;
class highldl4;
var ldl;
run;

Proc means data=carrs1;
class hightg4;
var tg;
run;

Proc freq data=carrs1;
table diabetes4 hightc4 hypertension4;
run;

Proc freq data=carrs1;
table diabetes4 hightc4 hypertension4 diabetes4*hightc4*hypertension4/norow  nopercent;
run;

Proc freq data=carrs1;
table metsyn4;
run;

* Create a permanent Data set;
Libname s "C:\Users\npoveda\Documents\Emory\Second year\Research Rotation 2";
run;

Data s.carrs_recode;
set carrs1;
run;
