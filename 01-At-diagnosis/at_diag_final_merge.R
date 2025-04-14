# Author: pcardoso
###############################################################################

# Identify those with diabetes at diagnosis, smoking

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

## Baseline biomarkers plus CKD stage
load("/home/pcardoso/workspace/SDRN-Cohort-scripts/Interim_Datasets/at_diag_baseline_biomarkers.RData")
load("/home/pcardoso/workspace/SDRN-Cohort-scripts/Interim_Datasets/at_diag_ckd_stages.RData")

## Comorbidities
load("/home/pcardoso/workspace/SDRN-Cohort-scripts/Interim_Datasets/at_diag_comorbidities.RData")
#load("/home/pcardoso/workspace/SDRN-Cohort-scripts/Interim_Datasets/at_diag_efi.RData")

## Non-diabetes med
#load("/home/pcardoso/workspace/SDRN-Cohort-scripts/Interim_Datasets/at_diag_non_diabetes_meds.RData")

## Smoking status
load("/home/pcardoso/workspace/SDRN-Cohort-scripts/Interim_Datasets/at_diag_smoking.RData")

## Alcohol status
load("/home/pcardoso/workspace/SDRN-Cohort-scripts/Interim_Datasets/at_diag_alcohol.RData")

## Death causes
load("/home/pcardoso/workspace/SDRN-Cohort-scripts/Interim_Datasets/all_patid_death_causes.RData")


###############################################################################

# Bring together

final_merge <- diabetes_cohort %>%
		filter(!is.na(dm_diag_date_all)) %>%
		left_join((baseline_biomarkers %>% select(-index_date)), by = "serialno") %>%
		left_join(ckd_stages, by = "serialno") %>%
		left_join((comorbidities %>% select(-index_date)), by = "serialno") %>%
#		left_join((efi %>% select(-index_date)), by = "serialno") %>%
		left_join(smoking, by = "serialno") %>%
		left_join(alcohol, by = "serialno") %>%
		left_join(death_causes, by = "serialno")

at_diag_final <- final_merge

save(at_diag_final, file = paste0("/home/pcardoso/workspace/SDRN-Cohort-scripts/Final_Datasets/at_diag_final_", today, ".RData"))







