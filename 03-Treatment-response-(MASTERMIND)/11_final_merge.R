# Author: pcardoso
###############################################################################

# Identify those with diabetes type 2, combine all datasets needed

###############################################################################

rm(list=ls())

# load libraries
library(tidyverse)


###############################################################################

# Today's date for table names

today <- as.character(Sys.Date(), format="%Y%m%d")


###############################################################################

# Get handles to pre-existing data tables

## Get diabetes cohort
load("/home/pcardoso/workspace/SDRN-Cohort-scripts/Interim_Datasets/all_diabetes_cohort.RData")

## Drug info
load("/home/pcardoso/workspace/SDRN-Cohort-scripts/Interim_Datasets/mm_drug_start_stop.RData")
load("/home/pcardoso/workspace/SDRN-Cohort-scripts/Interim_Datasets/mm_combo_start_stop.RData")

## Biomarkers inc. CKD
#load("/home/pcardoso/workspace/SDRN-Cohort-scripts/Interim_Datasets/mm_baseline_biomarkers.RData")
load("/home/pcardoso/workspace/SDRN-Cohort-scripts/Interim_Datasets/mm_response_biomarkers.RData") # includes baseline biomarker values for first instance drug periods so no need to use baseline_biomarkers table
load("/home/pcardoso/workspace/SDRN-Cohort-scripts/Interim_Datasets/mm_ckd_stages.RData")

## Comorbidities and eFI (eFI not inc. right now)
load("/home/pcardoso/workspace/SDRN-Cohort-scripts/Interim_Datasets/mm_comorbidities.RData")

## Non-diabetes meds
load("/home/pcardoso/workspace/SDRN-Cohort-scripts/Interim_Datasets/mm_non_diabetes_meds.RData")

## Smoking status at drug start
load("/home/pcardoso/workspace/SDRN-Cohort-scripts/Interim_Datasets/mm_smoking.RData")

## Alcohol at drug start
load("/home/pcardoso/workspace/SDRN-Cohort-scripts/Interim_Datasets/mm_alcohol.RData")

## Discontinuation
load("/home/pcardoso/workspace/SDRN-Cohort-scripts/Interim_Datasets/mm_discontinuation.RData")

## Glycaemic failure
load("/home/pcardoso/workspace/SDRN-Cohort-scripts/Interim_Datasets/mm_glycaemic_failure.RData")

## Death causes
load("/home/pcardoso/workspace/SDRN-Cohort-scripts/Interim_Datasets/all_patid_death_causes.RData")


###############################################################################

# Make first instance drug period dataset

## Define all diabetes cohort (1 line per patient)
all_diabetes <- diabetes_cohort
#		left_join(townsend_score %>% select(serialno, tds_2011), by = c("serialno")) %>%
#		relocate(tds_2011, .after = imd_decile) %>%
#		filter(with_hes == 1)


## Get info for first instance drug periods for cohort (1 line per serialno-drug_substance period)

all_diabetes_drug_periods <- all_diabetes %>%
		inner_join(drug_start_stop, by = c("serialno")) %>%
		inner_join(combo_start_stop, by = c("serialno", c("dstartdate" = "dcstartdate")))

all_diabetes_drug_periods %>% distinct(serialno) %>% nrow()
#401998

### Keep first instance only
all_diabetes_1stinstance <- all_diabetes_drug_periods %>%
		filter(drug_instance==1)

all_diabetes_1stinstance %>% distinct(serialno) %>% nrow()
#401998 as above



## Merge in biomarkers, comorbidities, eFI, non-diabetes meds, smoking status, alcohol
### Could merge on druginstance too, but quicker not to
### Remove some variables to avoid duplicates
### Make new variables: age at drug start, diabetes duration at drug start

all_diabetes_1stinstance <- all_diabetes_1stinstance %>%
		inner_join((response_biomarkers %>% select(-c(drug_class, drug_instance, timeprevcombo_class))), by = c("serialno", "dstartdate", "drug_substance")) %>%
		inner_join((ckd_stages %>% select(-c(drug_class, drug_instance))), by = c("serialno", "dstartdate", "drug_substance")) %>%
		inner_join((comorbidities %>% select(-c(drug_class, drug_instance))), by = c("serialno", "dstartdate", "drug_substance")) %>%
#		inner_join((efi %>% select(-c(drug_class, drug_instance))), by = c("serialno", "dstartdate", "drug_substance"))
		inner_join((non_diabetes_meds %>% select(-c(drug_class, drug_instance))), by = c("serialno", "dstartdate", "drug_substance")) %>%
		inner_join((smoking %>% select(-c(drug_class, drug_instance))), by = c("serialno", "dstartdate", "drug_substance")) %>%
		inner_join((alcohol %>% select(-c(drug_class, drug_instance))), by = c("serialno", "dstartdate", "drug_substance")) %>%
		left_join(death_causes, by = "serialno") %>%
		mutate(
				dstartdate_age = as.numeric(difftime(dstartdate, dob, units = "days"))/365.25,,
				dstartdate_age_all = dstartdate_age,
				dstartdate_dm_dur = as.numeric(difftime(dstartdate, dm_diag_date_all, units = "days"))/365.25,
				dstartdate_dm_dur_all = dstartdate_dm_dur
#			hosp_admission_prev_year = ifelse(is.na(hosp_admission_prev_year), 0,
#					ifelse(hosp_admission_prev_year==1, 1, NA)),
#			hosp_admission_prev_year_count = ifelse(is.na(hosp_admission_prev_year_count), 0, hosp_admission_prev_year)
		) %>%
		
		### Glycaemic failure and discontinuation are currently by drug class (not substance) only
		#### Glycaemic failure doesn't include all drug periods - only those with HbA1cs
		
		left_join((glycaemic_failure %>% select(-c(dstopdate, timetochange, timetoaddrem, nextdrugchange, nextdcdate, prehba1c, prehba1cdate, threshold_7.5, threshold_8.5, threshold_baseline, threshold_baseline_0.5))), by = c("serialno", "dstartdate", "drug_class")) %>%
		left_join((discontinuation %>% select(-c(drugline_all, dstopdate_class, drug_instance, timeondrug, nextremdrug, timetolastpx))), by = c("serialno", "dstartdate" = "dstartdate_class", "drug_class"))


# Check counts

all_diabetes_1stinstance %>% nrow()
#926897

all_diabetes_1stinstance %>% distinct(serialno) %>% nrow()
#401998

###############################################################################

# Add in 5 years QDiabetes-HF score and QRISK2 score

## Make separate table with additional variables for QRISK2 and QDiabetes-HF

#qscore_vars <- all_diabetes_1stinstance %>%
#		mutate(
#				precholhdl = pretotalcholesterol/prehdl,
#				ckd45 = as.numeric(!is.na(preckdstage) & (preckdstage=="stage_4" | preckdstage=="stage_5")),
#				cvd = as.numeric(predrug_myocardialinfarction==1 | predrug_angina==1 | predrug_stroke==1),
#				sex = ifelse(gender==1, "male", ifelse(gender==2, "female", "NA")),
#				dm_duration_cat = ifelse(dstartdate_dm_dur_all<=1, 0L,
#						ifelse(dstartdate_dm_dur_all<4, 1L,
#								ifelse(dstartdate_dm_dur_all<7, 2L,
#										ifelse(dstartdate_dm_dur_all<11, 3L, 4L)))),
#				earliest_bp_med = pmin(
#						ifelse(is.na(predrug_earliest_ace_inhibitors), as.Date("2050-01-01", origin = "1970-01-01"), predrug_earliest_ace_inhibitors),
#						ifelse(is.na(predrug_earliest_beta_blockers), as.Date("2050-01-01", origin = "1970-01-01"), predrug_earliest_beta_blockers),
#						ifelse(is.na(predrug_earliest_ca_channel_blockers), as.Date("2050-01-01", origin = "1970-01-01"), predrug_earliest_ca_channel_blockers),
#						ifelse(is.na(predrug_earliest_thiazide_diuretics), as.Date("2050-01-01", origin = "1970-01-01"), predrug_earliest_thiazide_diuretics),
#						na.rm = TRUE
#				),
#				latest_bp_med = pmax(
#						ifelse(is.na(predrug_latest_ace_inhibitors), as.Date("1900-01-01", origin = "1970-01-01"), predrug_latest_ace_inhibitors),
#						ifelse(is.na(predrug_latest_beta_blockers), as.Date("1900-01-01", origin = "1970-01-01"), predrug_latest_beta_blockers),
#						ifelse(is.na(predrug_latest_ca_channel_blockers), as.Date("1900-01-01", origin = "1970-01-01"), predrug_latest_ca_channel_blockers),
#						ifelse(is.na(predrug_latest_thiazide_diuretics), as.Date("1900-01-01", origin = "1970-01-01"), predrug_latest_thiazide_diuretics),
#						na.rm = TRUE
#				),
### check this one, if earliest and latests is the same, the drug was being taken during period
#				bp_meds = ifelse(earlist_bp_med!=as.Date("2050-01-01", origin = "1970-01-01") & latest_bp_med!=as.Date("1900-01-01", origin = "1970-01-01") & difftime(dstartdate, latest_bp_med, units = "days")<=28 & earliest_bp_med!=latest_bp_med, 1L, 0L),
#				
#				type1 = 0L,
#				type2 = 1L,
#				surv_5yr = 5L,
#				surv_10yrs = 10L
#		) %>%
#		select(serialno, dstartdate, drug_substance, sex, dstartdate_age, ethnicity_qrisk2, qrisk_smoking_cat, dm_duration_cat, bp_meds, type1, type2, cvd, ckd45, predrug_fh_premature_cvd, predrug_af, predrug_rheumatoidarthritis, prehba1c2yrs, precholhdl, presbp, prebmi, tds_2011, surv_5yr, surv_10yr)




###############################################################################

# Add in kidney risk scores

#
#
# Not coded
#
#


###############################################################################

## Filter just type 2s
t2d_1stinstance <- all_diabetes_1stinstance %>% 
		filter(diabetes_type == "type 2")

### Check unique serial count
t2d_1stinstance %>% distinct(serialno) %>% nrow()
#401998

save(t2d_1stinstance, file = paste0("/home/pcardoso/workspace/SDRN-Cohort-scripts/Final_Datasets/mm_", today, "_t2d_1stinstance.RData"))

###############################################################################

# Make dataset of all drug starts so that can see whether people later initiate SGLT2i/GLP1 etc.
## Add in discontinuation variables

## Just T2s
t2d_all_drug_periods <- all_diabetes %>%
		filter(diabetes_type == "type 2") %>%
		select(serialno) %>%
		inner_join(drug_start_stop, by = c("serialno"))

save(t2d_all_drug_periods, file = paste0("/home/pcardoso/workspace/SDRN-Cohort-scripts/Final_Datasets/mm_", today, "_t2d_all_drug_periods.RData"))






