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

## Comorbidities, non-diabetes meds, smoking and alcohol (04, 06, 07, 08)
## If their interim files are missing (no database access to re-run 04-08), these variables are taken from the
## last saved final dataset (Final_Datasets/mm_{date}_t2d_1stinstance.RData, same NDS_2024 data and same 04-08 code).
## Joined by serialno + dstartdate + drug_substance; drug starts not in the old dataset (e.g. Exenatide starts that the old
## 01 dropped) get missing values for these variables (count printed below)
covariate_files <- paste0("/home/pcardoso/workspace/SDRN-Cohort-scripts/Interim_Datasets/mm_", c("comorbidities", "non_diabetes_meds", "smoking", "alcohol"), ".RData")
use_old_final_covariates <- !all(file.exists(covariate_files))

if (use_old_final_covariates) {

	old_final_files <- sort(list.files("/home/pcardoso/workspace/SDRN-Cohort-scripts/Final_Datasets/", pattern = "^mm_[0-9]{8}_t2d_1stinstance\\.RData$", full.names = TRUE))
	old_final_files <- old_final_files[!grepl(today, old_final_files)]
	print(paste("Covariates from", tail(old_final_files, 1)))
	load(tail(old_final_files, 1))
	old_1stinstance <- t2d_1stinstance
	rm(t2d_1stinstance)

} else {

	## Comorbidities and eFI (eFI not inc. right now)
	load(covariate_files[1])
	## Non-diabetes meds
	load(covariate_files[2])
	## Smoking status at drug start
	load(covariate_files[3])
	## Alcohol at drug start
	load(covariate_files[4])

}

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

### Exclude drug periods starting after or on same day as death (as CPRD 13_mm_final_merge)
### CPRD also excludes drug periods starting within 90 days of registration and sets drugline to missing if diagnosed
### before registration: SDRN has no registration date (see server_checks.R C1 for the start of prescribing data)
all_diabetes_1stinstance <- all_diabetes_1stinstance %>%
		filter(is.na(death_date) | death_date > dstartdate)

all_diabetes_1stinstance %>% nrow()
all_diabetes_1stinstance %>% distinct(serialno) %>% nrow()



## Merge in biomarkers, comorbidities, eFI, non-diabetes meds, smoking status, alcohol
### Could merge on druginstance too, but quicker not to
### Remove some variables to avoid duplicates
### Make new variables: age at drug start, diabetes duration at drug start

all_diabetes_1stinstance <- all_diabetes_1stinstance %>%
		inner_join((response_biomarkers %>% select(-c(drug_class, drug_instance, timeprevcombo_class))), by = c("serialno", "dstartdate", "drug_substance")) %>%
		inner_join((ckd_stages %>% select(-c(drug_class, drug_instance))), by = c("serialno", "dstartdate", "drug_substance"))

if (use_old_final_covariates) {

	## only the variables that are not made by the other (re-run) tables: comorbidities, non-diabetes meds, smoking, alcohol
	new_names <- unique(c(names(all_diabetes_1stinstance), names(death_causes), names(glycaemic_failure), names(discontinuation),
					"dstartdate_age", "dstartdate_age_all", "dstartdate_dm_dur", "dstartdate_dm_dur_all"))
	old_covariates <- old_1stinstance %>%
			select(serialno, dstartdate, drug_substance, all_of(setdiff(names(old_1stinstance), new_names))) %>%
			distinct(serialno, dstartdate, drug_substance, .keep_all = TRUE) %>%
			mutate(in_old_final = 1)
	rm(old_1stinstance)

	all_diabetes_1stinstance <- all_diabetes_1stinstance %>%
			left_join(old_covariates, by = c("serialno", "dstartdate", "drug_substance"))

	tmp <- all_diabetes_1stinstance %>% filter(diabetes_type == "type 2")
	cat("F1 old covariate columns:", ncol(old_covariates) - 4, "\n")
	cat("F2 T2 first-instance starts without old covariates:", sum(is.na(tmp$in_old_final)), "of", nrow(tmp),
			"(", round(100*mean(is.na(tmp$in_old_final)), 1), "% )\n")
	cat("F3 by class (% missing):", paste(tmp %>% group_by(drug_class) %>% summarise(p = round(100*mean(is.na(in_old_final)), 1)) %>%
							mutate(x = paste0(drug_class, " ", p)) %>% pull(x), collapse = "; "), "\n")
	all_diabetes_1stinstance <- all_diabetes_1stinstance %>% select(-in_old_final)
	rm(tmp, old_covariates)

} else {

	all_diabetes_1stinstance <- all_diabetes_1stinstance %>%
			inner_join((comorbidities %>% select(-c(drug_class, drug_instance))), by = c("serialno", "dstartdate", "drug_substance")) %>%
#			inner_join((efi %>% select(-c(drug_class, drug_instance))), by = c("serialno", "dstartdate", "drug_substance"))
			inner_join((non_diabetes_meds %>% select(-c(drug_class, drug_instance))), by = c("serialno", "dstartdate", "drug_substance")) %>%
			inner_join((smoking %>% select(-c(drug_class, drug_instance))), by = c("serialno", "dstartdate", "drug_substance")) %>%
			inner_join((alcohol %>% select(-c(drug_class, drug_instance))), by = c("serialno", "dstartdate", "drug_substance"))

}

all_diabetes_1stinstance <- all_diabetes_1stinstance %>%
		left_join(death_causes, by = "serialno") %>%
		mutate(
				dstartdate_age = as.numeric(difftime(dstartdate, dob, units = "days"))/365.25,
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






