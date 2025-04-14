# Author: pcardoso
###############################################################################

# Collect CKD stages for combos

###############################################################################

rm(list=ls())

# load libraries
library(tidyverse)

# function
## 'clean_codes' ensures the 3rd and 4th characters are separated by a dot "." (SDRN necessity)
### 'codelist' list of codes for a particular comorbidity
clean_codes <- function(codelist) {
	# iterate through codelist
	for (code in 1:length(codelist)) {
		# if code >3 characters and 4th character is not a dot "."
		if (nchar(codelist[code]) > 3 && substr(codelist[code], 4, 4) != ".") {
			# add a dot after the 3rd character
			codelist[code] <- paste0(substr(codelist[code], 1, 3), ".", substr(codelist[code], 4, nchar(codelist[code])))
		} else {codelist[code]} # return code
	}
	return(codelist) # return list of codes
}

###############################################################################

# Define biomarkers
## Keep HbA1c separate as processed differently
## Missing: haematocrit, albumin_urine, creatinine_urine, acr_from_separate, albumin_blood

## Data has been cleaned by SDRN, but still has weird values sometimes

load("/home/pcardoso/workspace/SDRN-Cohort-scripts/Interim_Datasets/mm_clean_egfr_medcodes.RData")


###############################################################################

# Convert eGFR to CKD stage

ckd_stages_from_all_egfr <- clean_egfr_medcodes %>%
		mutate(
			ckd_stage = ifelse(testvalue<15, "stage_5",
					ifelse(testvalue<30, "stage_4",
							ifelse(testvalue<45, "stage_3b",
									ifelse(testvalue<60, "stage_3a",
											ifelse(testvalue<90, "stage_2",
													ifelse(testvalue>=90, "stage_1", NA))))))
		)


###############################################################################

# Only keep CKD stages if >1 consecutive test with the same stage, and if time between earliest and latest consecutive test with same stage are >=90 days apart

## For each patient:
### A) Define period from current test until next test as having the ckd_stage of current test
### B) Join together consecutive periods with the same ckd_stage
### C) I fperiod contains >1 test, and there is >=90 days between the first and last test in the period, it is 'confirmed'



### A) Define period from current test until next test as having the ckd_stage of current test

#### Add in row labelling within each patient's values + max number of rows for each patient

ckd_stages_from_algorithm <- ckd_stages_from_all_egfr %>%
		group_by(serialno) %>%
		arrange(date) %>%
		mutate(patid_row_id = row_number()) %>%
		mutate(patid_total_rows = max(patid_row_id, na.rm = TRUE)) %>%
		ungroup()

#### From rows where there is a next test, use this as end date; for last row, use start date as end date

ckd_stages_from_algorithm <- ckd_stages_from_algorithm %>%
		mutate(next_row = patid_row_id+1) %>%
		left_join(
			ckd_stages_from_algorithm, by = c("serialno", "next_row" = "patid_row_id")
		) %>%
		mutate(
			ckd_start = date.x,
			ckd_end = ifelse(is.na(date.y), date.x, date.y),
			ckd_stage = ckd_stage.x
		) %>%
		select(serialno, patid_row_id, ckd_stage, ckd_start, ckd_end)


### B) Join together consecutive periods with the same ckd_stage

ckd_stages_from_algorithm <- ckd_stages_from_algorithm %>%
		group_by(serialno, ckd_stage) %>%
		arrange(serialno, ckd_stage, patid_row_id) %>%
		mutate(
			lead_var = lead(ckd_start),
			cummax_var = cummax(ckd_end)
		) %>%
		mutate(compare = cumsum(lead_var>cummax_var)) %>%
		mutate(indx = ifelse(row_number()==1, 0,lag(compare))) %>%
		ungroup() %>%
		group_by(serialno, ckd_stage, indx) %>%
		summarise(
			first_test_date = min(ckd_start, na.rm = TRUE),
			last_test_date = max(ckd_start, na.rm = TRUE),
			end_date = max(ckd_end, na.rm = TRUE),
			test_count = max(patid_row_id, na.rm = TRUE) - min(patid_row_id, na.rm = TRUE)+1
		) %>%
		ungroup()

#ckd_stages_from_algorithm %>% nrow()
## 3473503

#ckd_stages_from_algorithm %>% summarise(total = sum(test_count, na.rm = TRUE))
## 13719859

### C) If period contains >1 test, and there is >=90 days between the first and last test in the period, it is 'confirmed'

ckd_stages_from_algorithm <- ckd_stages_from_algorithm %>%
		filter(test_count>1 & difftime(last_test_date, first_test_date, units = "days")>=90)

#ckd_stages_from_algorithm %>% nrow()
## 1478119



###############################################################################

# Combine with CKD5 medcodes/ICD10/OPCS4 codes

## Get raw CKD5 codes and clean

## connection to database
con <- dbConn("NDS_2023")

# Define comorbidities
## If you add comorbidity to the end of this list, code should run fine to incorporate new comorbidity
## Missing = fh_diabetes, fh_premature_cvd, frailty_simple, hosp_cause_majoramputation, hosp_cause_minoramputation, osteoporosis, photocoagulation, revasc, solidorgantransplant

comorbids <- c("ckd5_code")

###############################################################################

# Pull out all raw code instancas

# Should it count death records too?

for (i in comorbids) {
	
	print(i)
	codelist_name_icd10 <- paste0("/home/pcardoso/workspace/SDRN-Cohort-scripts/Codelists/ICD10/exeter_icd10_", i, ".txt")
	codelist_name_opcs4 <- paste0("/home/pcardoso/workspace/SDRN-Cohort-scripts/Codelists/OPCS4/exeter_opcs4_", i, ".txt")
	
	if (file.exists(codelist_name_icd10)) {
		
		codelist <- clean_codes(read.delim(codelist_name_icd10) %>%
						select(-Term_description) %>%
						unlist())
		
		raw_tablename <- paste0("raw_", i, "_icd10")
		
		mysqlquery <- paste0("
						SELECT o_condition.* 
						FROM o_condition, o_concept_condition 
						WHERE
						o_concept_condition.name = 'icd10' AND o_concept_condition.uid = o_condition.concept_id AND
						o_concept_condition.path LIKE 'smr%' AND o_concept_condition.path NOT LIKE '%causeofdeath%' AND
						(", paste("o_condition.condition_code LIKE ", paste(paste("'", codelist, "%'", sep = ""), collapse = " OR o_condition.condition_code LIKE ")), ")")
		
		data <- dbGetQueryMap(con, mysqlquery)
		
		print(paste("Number of rows", raw_tablename, nrow(data)))
		
		assign(raw_tablename, data)
		
	}
	
	if (file.exists(codelist_name_opcs4)) {
		
		codelist <- clean_codes(read.delim(codelist_name_opcs4) %>%
						select(-Term_description) %>%
						unlist())
		
		raw_tablename <- paste0("raw_", i, "_opcs4")
		
		mysqlquery <- paste0("
						SELECT o_condition.* 
						FROM o_condition, o_concept_condition 
						WHERE
						o_concept_condition.name = 'opcs4' AND o_concept_condition.uid = o_condition.concept_id AND
						o_concept_condition.path LIKE 'smr%' AND o_concept_condition.path NOT LIKE '%causeofdeath%' AND
						(", paste("o_condition.condition_code LIKE ", paste(paste("'", codelist, "%'", sep = ""), collapse = " OR o_condition.condition_code LIKE ")), ")")
		
		data <- dbGetQueryMap(con, mysqlquery)
		
		print(paste("Number of rows", raw_tablename, nrow(data)))
		
		assign(raw_tablename, data)
		
	}
	
}


## disconnect from database
dbDisconnect(con)


## Clean, find earliest date per person
earliest_clean_ckd5 <- raw_ckd5_code_icd10 %>%
		select(serialno, date = startdate) %>%
		union_all((raw_ckd5_code_opcs4 %>% select(serialno, date = startdate))) %>%
		group_by(serialno) %>%
		summarise(first_test_date = min(date, na.rm = TRUE)) %>%
		ungroup()

## Combine CKD5 and other codes
ckd_stages_from_algorithm <- ckd_stages_from_algorithm %>%
		select(serialno, ckd_stage, first_test_date) %>%
		union_all(earliest_clean_ckd5 %>% mutate(ckd_stage = "stage_5"))

#ckd_stages_from_algorithm %>% nrow()
## 1486195

###############################################################################

# Define date of onset for each stage

## For each person, define date of onset of each stage (earliest incident) - assume no returning to less severe stages

ckd_stages_from_algorithm <- ckd_stages_from_algorithm %>%
		group_by(serialno, ckd_stage) %>%
		summarise(ckd_stage_start = min(first_test_date, na.rm = TRUE)) %>%
		ungroup()

## Remove where start date of less severe stage is later than start date of more severe stage
### Reshape wide first

ckd_stages_from_algorithm <- ckd_stages_from_algorithm %>%
		pivot_wider(
			id_cols = serialno,
			names_from = ckd_stage,
			values_from = ckd_stage_start
		) %>%
		mutate(
			stage_1 = ifelse(!is.na(stage_1) & !is.na(stage_2) & stage_1>stage_2, NA, stage_1),
			stage_1 = ifelse(!is.na(stage_1) & !is.na(stage_3a) & stage_1>stage_3a, NA, stage_1),
			stage_1 = ifelse(!is.na(stage_1) & !is.na(stage_3b) & stage_1>stage_3b, NA, stage_1),
			stage_1 = ifelse(!is.na(stage_1) & !is.na(stage_4) & stage_1>stage_4, NA, stage_1),
			stage_1 = ifelse(!is.na(stage_1) & !is.na(stage_5) & stage_1>stage_5, NA, stage_1),
			stage_2 = ifelse(!is.na(stage_2) & !is.na(stage_3a) & stage_2>stage_3a, NA, stage_2),
			stage_2 = ifelse(!is.na(stage_2) & !is.na(stage_3b) & stage_2>stage_3b, NA, stage_2),
			stage_2 = ifelse(!is.na(stage_2) & !is.na(stage_4) & stage_2>stage_4, NA, stage_2),
			stage_2 = ifelse(!is.na(stage_2) & !is.na(stage_5) & stage_2>stage_5, NA, stage_2),
			stage_3a = ifelse(!is.na(stage_3a) & !is.na(stage_3b) & stage_3a>stage_3b, NA, stage_3a),
			stage_3a = ifelse(!is.na(stage_3a) & !is.na(stage_4) & stage_3a>stage_4, NA, stage_3a),
			stage_3a = ifelse(!is.na(stage_3a) & !is.na(stage_5) & stage_3a>stage_5, NA, stage_3a),
			stage_3b = ifelse(!is.na(stage_3b) & !is.na(stage_4) & stage_3b>stage_4, NA, stage_3b),
			stage_3b = ifelse(!is.na(stage_3b) & !is.na(stage_5) & stage_3b>stage_5, NA, stage_3b),
			stage_4 = ifelse(!is.na(stage_4) & !is.na(stage_5) & stage_4>stage_5, NA, stage_4)
		) %>%
		mutate(
			stage_1 = as.Date(stage_1, origin = "1970-01-01"),
			stage_2 = as.Date(stage_2, origin = "1970-01-01"),
			stage_3a = as.Date(stage_3a, origin = "1970-01-01"),
			stage_3b = as.Date(stage_3b, origin = "1970-01-01"),
			stage_4 = as.Date(stage_4, origin = "1970-01-01")
		)


###############################################################################

# Get drug start dates (1 row per drug period)
load("/home/pcardoso/workspace/SDRN-Cohort-scripts/Interim_Datasets/mm_drug_start_stop.RData")

# Merge with CKD stages (1 row per patid)

ckd_stages <- drug_start_stop %>%
		select(serialno, dstartdate, drug_class, drug_substance, drug_instance) %>%
		left_join(ckd_stages_from_algorithm, by = c("serialno")) %>%
		mutate(
			preckdstage = ifelse(!is.na(stage_5) & difftime(stage_5, dstartdate, units = "days")<=7, "stage_5",
					ifelse(!is.na(stage_4) & difftime(stage_4, dstartdate, units = "days")<=7, "stage_4",
							ifelse(!is.na(stage_3b) & difftime(stage_3b, dstartdate, units = "days")<=7, "stage_3b",
									ifelse(!is.na(stage_3a) & difftime(stage_3a, dstartdate, units = "days")<=7, "stage_3a",
											ifelse(!is.na(stage_2) & difftime(stage_2, dstartdate, units = "days")<=7, "stage_2",
													ifelse(!is.na(stage_1) & difftime(stage_1, dstartdate, units = "days")<=7, "stage_1", NA)))))),
			
			preckdstagedate = ifelse(preckdstage=="stage_5", stage_5,
					ifelse(preckdstage=="stage_4", stage_4,
							ifelse(preckdstage=="stage_3b", stage_3b,
									ifelse(preckdstage=="stage_3a", stage_3a,
											ifelse(preckdstage=="stage_2", stage_2,
													ifelse(preckdstage=="stage_1", stage_1, NA)))))),
			preckdstagedate = as.Date(preckdstagedate, origin = "1970-01-01"),
			
			preckdstagedrugdiff = difftime(preckdstagedate, dstartdate, units = "days"),
			
			postckdstage345date = pmin(
				ifelse(!is.na(stage_3a) & difftime(stage_3a, dstartdate, units = "days")>7, stage_3a, as.Date("2050-01-01", origin = "1970-01-01")),
				ifelse(!is.na(stage_3b) & difftime(stage_3b, dstartdate, units = "days")>7, stage_3b, as.Date("2050-01-01", origin = "1970-01-01")),
				ifelse(!is.na(stage_4) & difftime(stage_4, dstartdate, units = "days")>7, stage_4, as.Date("2050-01-01", origin = "1970-01-01")),
				ifelse(!is.na(stage_5) & difftime(stage_5, dstartdate, units = "days")>7, stage_5, as.Date("2050-01-01", origin = "1970-01-01")), na.rm = TRUE
			),
			postckdstage345date = as.Date(postckdstage345date, origin = "1970-01-01"),
			
			postckdstage5date = ifelse(!is.na(stage_5) & difftime(stage_5, dstartdate, units = "days")>7, stage_5, NA),
			postckdstage5date = as.Date(postckdstage5date, origin = "1970-01-01")
			
		) %>%
		mutate(
			postckdstage345date = ifelse(postckdstage345date == as.Date("2050-01-01", origin = "1970-01-01"), as.Date(NA), postckdstage345date),
			postckdstage345date = as.Date(postckdstage345date, origin = "1970-01-01")
		) %>%
		select(serialno, dstartdate, drug_class, drug_substance, drug_instance, preckdstagedate, preckdstagedrugdiff, preckdstage, postckdstage5date, postckdstage345date)

save(ckd_stages, file = "/home/pcardoso/workspace/SDRN-Cohort-scripts/Interim_Datasets/mm_ckd_stages.RData")





