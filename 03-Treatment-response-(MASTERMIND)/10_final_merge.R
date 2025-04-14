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

## Non-diabetes meds (not inc. right now)

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
#		inner_join((non_diabetes_meds %>% select(-c(drug_class, drug_instance))), by = c("serialno", "dstartdate", "drug_substance")) %>%
		inner_join((smoking %>% select(-c(drug_class, drug_instance))), by = c("serialno", "dstartdate", "drug_substance")) %>%
		inner_join((alcohol %>% select(-c(drug_class, drug_instance))), by = c("serialno", "dstartdate", "drug_substance")) %>%
		left_join(death_causes, by = "serialno") %>%
		mutate(
			dstartdate_age = as.numeric(difftime(dstartdate, dob, units = "days"))/365.25,
			dstartdate_dm_dur = as.numeric(difftime(dstartdate, dm_diag_date_all, units = "days"))/365.25
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

# Add in 5 year QDiabetes-HF score and QRISK2 score

#
#
# Not coded
#
#


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






