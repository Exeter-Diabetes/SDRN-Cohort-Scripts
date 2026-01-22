# Author: pcardoso
###############################################################################

# Collect baseline biomarkers for combos

###############################################################################

rm(list=ls())

# load libraries
library(tidyverse)


###############################################################################

## connection to database
con <- dbConn("NDS_2024")

###############################################################################

# Define biomarkers
## Keep HbA1c separate as processed differently
## Missing: haematocrit, albumin_urine, creatinine_urine, acr_from_separate, albumin_blood

## Data has been cleaned by SDRN, but still has weird values sometimes


biomarkers <- c("weight", "height", "bmi", "fastingglucose", "hdl", "triglyceride", "creatinine_blood", "ldl", "alt", "ast", "totalcholesterol", "dbp", "sbp", "acr", "bilirubin", "haemoglobin", "pcr")

concept_observation <- c("body_mass-weight", "body_mass-height", "body_mass-bmi", "biochem-bglu_fasting", "biochem-lipid-hdl", "biochem-lipid-trig", "biochem-creatinine", "biochem-lipid-ldl", "biochem-other-alt", "biochem-other-ast", "biochem-lipid-tchol", "blood_pressure-dbp", "blood_pressure-sbp", "biochem-albumin-ratio", "biochem-other-bilirubin", "biochem-other-haemoglobin", "biochem-urine_prot-pcr")

min_limits <- c(40, 0.6, 15, 2.5, 0.2, 0.1, 20, 0.1, 0, 0, 0.5, 20, 40, 0, 1, 30, 0)

max_limits <- c(350, 2.25, 100, 30, 10, 40, 2500, 20, 200, 300, 20, 200, 270, 122, 250, 250, 682)

for (i in 1:length(biomarkers)) {
	
	print(biomarkers[i])
	
	clean_tablename <- paste0("clean_", biomarkers[i], "_medcodes")
	
	mysqlquery <- paste0("
					SELECT o_observation.* 
					FROM o_observation, o_concept_observation 
					WHERE
					o_observation.concept_id = o_concept_observation.uid AND 
					o_concept_observation.path = '", concept_observation[i], "'")
	
	data <- dbGetQueryMap(con, mysqlquery) %>%
			select(serialno, date, "testvalue" = num.value) %>%
			drop_na(testvalue) %>%
			filter(testvalue >= min_limits[i] & testvalue <= max_limits[i])
	
	assign(clean_tablename, data)
	
}


clean_hba1c_medcodes <- dbGetQueryMap(con, "
						SELECT o_observation.*
						FROM o_observation, o_concept_observation
						WHERE
						o_observation.concept_id = o_concept_observation.uid AND
						o_concept_observation.path = 'biochem-hba1c'") %>%
		select(serialno, date, "testvalue" = num.value) %>%
		drop_na(testvalue) %>%
		mutate(testvalue = ifelse(testvalue<20, ((testvalue-2.152)/0.09148), testvalue)) %>%
		filter(testvalue >= 20 & testvalue <= 195) %>%
		group_by(serialno, date) %>%
		summarise(testvalue = mean(testvalue, na.rm = TRUE)) %>%
		ungroup()


# Get diabetes cohort
load("/home/pcardoso/workspace/SDRN-Cohort-scripts/Interim_Datasets/all_diabetes_cohort.RData")


clean_egfr_medcodes <- clean_creatinine_blood_medcodes %>%
		inner_join(diabetes_cohort %>%
						select(serialno, dob, gender), by = c("serialno")) %>%
		mutate(
				age_at_creat = as.numeric(difftime(date, dob, units = "days"))/365.25,
				sex = ifelse(gender==1, "male", ifelse(gender==2, "female", NA))
		) %>%
		select(-c(dob, gender)) %>%
		## CKD epi 2021 egfr
		mutate(
				creatinine_mgdl = testvalue*0.0113,
				ckd_epi_2021_egfr = ifelse(creatinine_mgdl<=0.7 & sex=="female", (142 * ((creatinine_mgdl/0.7)^-0.241) * (0.9938^age_at_creat) * 1.012),
						ifelse(creatinine_mgdl>0.7 & sex=="female", (142 * ((creatinine_mgdl/0.7)^-1.2) * (0.9938^age_at_creat) * 1.012),
								ifelse(creatinine_mgdl<=0.9 & sex=="male", (142 * ((creatinine_mgdl/0.9)^-0.302) * (0.9938^age_at_creat)),
										ifelse(creatinine_mgdl>0.9 & sex=="male", (142 * ((creatinine_mgdl/0.9)^-1.2) * (0.9938^age_at_creat)), NA))))
		) %>%
		select(-c(creatinine_mgdl, testvalue, sex, age_at_creat)) %>%
		rename(testvalue = ckd_epi_2021_egfr) %>%
		drop_na(testvalue)


## disconnect from database
dbDisconnect(con)


biomarkers <- c("egfr", biomarkers)




###############################################################################

# Combine each biomarkers with start dates of all drug periods (not just first instances; with timetochange, timeaddrem and multi_drug_start added from mm_combo_start_stop table)

## Get drug start dates

load("/home/pcardoso/workspace/SDRN-Cohort-scripts/Interim_Datasets/mm_drug_start_stop.RData")

load("/home/pcardoso/workspace/SDRN-Cohort-scripts/Interim_Datasets/mm_combo_start_stop.RData")

drug_start_dates <- drug_start_stop %>%
		left_join(combo_start_stop, by = c("serialno", "dstartdate" = "dcstartdate"))

## Merge with biomarkers and calculate date difference between biomarker and drug start date
for (i in c(biomarkers, "hba1c")) {
	
	print(i)
	
	clean_tablename <- paste0("clean_", i, "_medcodes")
	drug_merge_tablename <- paste0("full_", i, "_drug_merge")
	
	data <- get(clean_tablename) %>%
			inner_join(drug_start_dates, by = c("serialno")) %>%
			mutate(drugdatediff = difftime(date, dstartdate, units = "days"))
	
	assign(drug_merge_tablename, data)
	
}


###############################################################################

# Find baseline values
## Within period defined above (-2 years to +7 days for all except heigh - also for main HbA1c variable)
## Then use closest date to drug start date
## May be multiple values; use minimum test result, expect for eGFR - use maximum
## Can get duplicates where person has identical results on the same day/days equidistant from the drug start date - choose first row when ordered by drugdatediff

baseline_biomarkers <- drug_start_stop %>%
		select(serialno, dstartdate, drug_class, drug_substance, drug_instance)

## For all except height: between 2 years prior and 7 days after drug start date
biomarkers_no_height <- setdiff(biomarkers, "height")

biomarkers_no_height <- c(biomarkers_no_height, "hba1c")

for (i in biomarkers_no_height) {
	
	print(i)
	
	drug_merge_tablename <- paste0("full_", i, "_drug_merge")
	interim_baseline_biomarker_table <- paste0("baseline_biomarkers_interim_", i)
	pre_biomarker_variable <- paste0("pre", i)
	pre_biomarker_date_variable <- paste0("pre", i, "date")
	pre_biomarker_drugdiff_variable <- paste0("pre", i, "drugdiff")
	
	data <- get(drug_merge_tablename) %>%
			mutate(drugdatediff = as.numeric(drugdatediff)) %>%
			filter(drugdatediff <=7 & drugdatediff >=-730) %>%
			
			group_by(serialno, dstartdate, drug_substance) %>%
			
			mutate(interim_var = ifelse(drugdatediff<0, 0-drugdatediff, drugdatediff)) %>%  # abs() function did not work due to running out of memory
			
			mutate(min_timediff = min(interim_var, na.rm = TRUE)) %>%
			
			filter(interim_var == min_timediff) %>%
			
			mutate(pre_biomarker = ifelse(i=="egfr", max(testvalue, na.rm = TRUE), min(testvalue, na.rm = TRUE))) %>%
			filter(pre_biomarker==testvalue) %>%
			
			arrange(drugdatediff) %>%
			filter(row_number()==1) %>%
			
			ungroup() %>%
			
			relocate(pre_biomarker, .after = serialno) %>%
			relocate(date, .after = pre_biomarker) %>%
			relocate(drugdatediff, .after = date) %>%
			
			rename(
					{{pre_biomarker_variable}}:=pre_biomarker,
					{{pre_biomarker_date_variable}}:=date,
					{{pre_biomarker_drugdiff_variable}}:=drugdatediff
			) %>%
			select(serialno, dstartdate, drug_substance, {{pre_biomarker_variable}}, {{pre_biomarker_date_variable}}, {{pre_biomarker_drugdiff_variable}})
	
	baseline_biomarkers <- baseline_biomarkers %>%
			left_join(data, by = c("serialno", "dstartdate", "drug_substance"))
	
}


## Height - only keep reading at/post drug start date, and find mean
baseline_height <- full_height_drug_merge %>%
		
		filter(drugdatediff >= 0) %>%
		
		group_by(serialno, dstartdate, drug_substance) %>%
		
		summarise(height = mean(testvalue, na.rm = TRUE)) %>%
		
		ungroup()

baseline_biomarkers <- baseline_biomarkers %>%
		left_join(baseline_height, by = c("serialno", "dstartdate", "drug_substance"))

## HbA1c: have 2 years value from above, now add in between 6 months prior and 7 days after drug start date (for prehba1c) or between 12 months prior and 7 days after (for prehba1c12m)
## Exclude if before timeprevcombo_class for 6 month and 12 month value (not 2 year value)

baseline_hba1c <- full_hba1c_drug_merge %>%
		
		filter(drugdatediff<=7 & drugdatediff>=-366) %>%
		
		group_by(serialno, dstartdate, drug_substance) %>%
		
		mutate(interim_var = ifelse(drugdatediff<0, 0-drugdatediff, drugdatediff)) %>%  # abs() function did not work due to running out of memory
		
		mutate(min_timediff = min(interim_var, na.rm = TRUE)) %>%
		
		filter(interim_var == min_timediff) %>%
		
		mutate(prehba1c12m=min(testvalue, na.rm = TRUE)) %>%
		filter(prehba1c12m==testvalue) %>%
		
		arrange(drugdatediff) %>%
		filter(row_number()==1) %>%
		
		ungroup() %>%
		
		rename(
				prehba1c12mdate=date,
				prehba1c12mdrugdiff=drugdatediff
		) %>%
		
		mutate(
				prehba1c = ifelse(prehba1c12mdrugdiff>=-183, prehba1c12m, NA),
				prehba1cdate = ifelse(prehba1c12mdrugdiff>=-183, prehba1c12mdate, NA),
				prehba1cdrugdiff = ifelse(prehba1c12mdrugdiff>=-183, prehba1c12mdrugdiff, NA)
		) %>%
		
		select(serialno, dstartdate, drug_substance, prehba1c12m, prehba1c12mdate, prehba1c12mdrugdiff, prehba1c, prehba1cdate, prehba1cdrugdiff)

### timeprevcombo_class in combo_start_stop table


load("/home/pcardoso/workspace/SDRN-Cohort-scripts/Interim_Datasets/mm_combo_start_stop.RData")

baseline_hba1c <- baseline_hba1c %>%
		left_join((combo_start_stop %>% select(serialno, dcstartdate, timeprevcombo_class)), by = c("serialno", c("dstartdate" = "dcstartdate"))) %>%
		filter(prehba1c12mdrugdiff>=0 | is.na(timeprevcombo_class) | (!is.na(timeprevcombo_class) & abs(prehba1c12mdrugdiff)<=timeprevcombo_class)) %>%
		select(-timeprevcombo_class)

baseline_biomarkers <- baseline_biomarkers %>%
		rename(
				prehba1c2yrs = prehba1c,
				prehba1c2yrsdate = prehba1cdate,
				prehba1c2yrsdrugdiff = prehba1cdrugdiff
		) %>%
		left_join(baseline_hba1c, by = c("serialno", "dstartdate", "drug_substance")) %>%
		relocate(height, .after=prehba1cdrugdiff)

save(baseline_biomarkers, file = "/home/pcardoso/workspace/SDRN-Cohort-scripts/Interim_Datasets/mm_baseline_biomarkers.RData")






