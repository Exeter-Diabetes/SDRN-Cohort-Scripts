# Author: pcardoso
###############################################################################

# Identify those with diabetes at prevalence, collect baseline biomarkers

###############################################################################

rm(list=ls())

# load libraries
library(tidyverse)


###############################################################################

## connection to database
con <- dbConn("NDS_2023")

###############################################################################

# Pull out all raw bimoarkers values

biomarkers <- c("weight", "height", "bmi", "fastingglucose", "hdl", "triglyceride", "creatinine_blood", "ldl", "alt", "ast", "totalcholesterol", "dbp", "sbp", "acr")

concept_observation <- c("body_mass-weight", "body_mass-height", "body_mass-bmi", "biochem-bglu_fasting", "biochem-lipid-hdl", "biochem-lipid-trig", "biochem-creatinine", "biochem-lipid-ldl", "biochem-other-alt", "biochem-other-ast", "biochem-lipid-tchol", "blood_pressure-dbp", "blood_pressure-sbp", "biochem-albumin-ratio")

min_limits <- c(40, 0.6, 15, 2.5, 0.2, 0.1, 20, 0.1, 0, 0, 0.5, 20, 40, 0)

max_limits <- c(350, 2.25, 100, 30, 10, 40, 2500, 20, 200, 300, 20, 200, 270, 122)


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

# Combine each biomarker with index dates

## Get index dates (diagnosis dates)
index_date <- as.Date("2022-11-01", origin = "1970-01-01")

## Merge with biomarkers and calculate date difference between biomarker and index date

for (i in biomarkers) {
	
	print(i)
	
	clean_tablename <- paste0("clean_", i, "_medcodes")
	index_date_merge_tablename <- paste0("full_", i, "_index_date_merge")
	
	data <- get(clean_tablename) %>%
			mutate(datediff=as.numeric(difftime(date, index_date, units = "days")))
	
	assign(index_date_merge_tablename, data)
	
}

# HbA1c

full_hba1c_index_date_merge <- clean_hba1c_medcodes %>%
		mutate(datediff=as.numeric(difftime(date, index_date, units = "days")))


###############################################################################

# Find baseline values
## Within period defined above (-2years to +7 days for all except HbA1c and height)
## Then use closest date to index date
## May be multiple values; use minimum test result, expect for eGFR - use maximum
## Can get duplicates where person has identical results on the same day/days equidistant from the index date - choose first row when ordered by datediff

baseline_biomarkers <- diabetes_cohort %>%
		select(serialno)

## For all except HbA1c and height: between 2 years prior and 7 days after index date

biomarkers_no_height <- setdiff(biomarkers, "height")

biomarkers_no_height <- c(biomarkers_no_height, "hba1c")

for (i in biomarkers_no_height) {
	
	print(i)
	
	index_date_merge_tablename <- paste0("full_", i, "_index_date_merge")
	interim_baseline_biomarker_table <- paste0("baseline_biomarkers_interim_", i)
	pre_biomarker_variable <- paste0("pre", i)
	pre_biomarker_date_variable <- paste0("pre", i, "date")
	pre_biomarker_datediff_variable <- paste0("pre", i, "datediff")
	
	data <- get(index_date_merge_tablename) %>%
			filter(datediff <=7 & datediff >=-730) %>%
			
			group_by(serialno) %>%
			
			mutate(interim_var = ifelse(datediff<0, 0-datediff, datediff)) %>%  # abs() function did not work due to running out of memory
			
			mutate(min_timediff = min(interim_var, na.rm = TRUE)) %>%
			
			filter(interim_var == min_timediff) %>%
			
			mutate(pre_biomarker = ifelse(i=="egfr", max(testvalue, na.rm = TRUE), min(testvalue, na.rm = TRUE))) %>%
			filter(pre_biomarker==testvalue) %>%
			
			arrange(datediff) %>%
			filter(row_number()==1) %>%
			
			ungroup() %>%
			
			relocate(pre_biomarker, .after = serialno) %>%
			relocate(date, .after = pre_biomarker) %>%
			relocate(datediff, .after = date) %>%
			
			rename(
					{{pre_biomarker_variable}}:=pre_biomarker,
					{{pre_biomarker_date_variable}}:=date,
					{{pre_biomarker_datediff_variable}}:=datediff
			) %>%
			select(-c(testvalue, min_timediff, interim_var))
	
	baseline_biomarkers <- baseline_biomarkers %>%
			left_join(data, by = "serialno")
	
	
}


## Height - only keep readings at/post-index date, and find mean
baseline_height <- full_height_index_date_merge %>%
		filter(datediff>=0) %>%
		group_by(serialno) %>%
		summarise(height=mean(testvalue, na.rm = TRUE)) %>%
		ungroup()

baseline_biomarkers <- baseline_biomarkers %>%
		left_join(baseline_height, by = "serialno")


## HbA1c: only between 6 months prior and 7 days after index date
### NB: in treatment response cohort, baseline HbA1c set to missing if occurs before previous treatment change

baseline_hba1c <- full_hba1c_index_date_merge %>%
		
		filter(datediff<=7 & datediff>=-183) %>%
		
		group_by(serialno) %>%
		
		mutate(min_timediff=min(abs(datediff), na.rm = TRUE)) %>%
		filter(abs(datediff) == min_timediff) %>%
		
		mutate(prehba1c = min(testvalue, na.rm = TRUE)) %>%
		filter(prehba1c==testvalue) %>%
		
		arrange(datediff) %>%
		filter(row_number()==1) %>%
		
		ungroup() %>%
		
		relocate(prehba1c, .after=serialno) %>%
		relocate(date, .after=prehba1c) %>%
		relocate(datediff, .after=date) %>%
		
		rename(
				prehba1cdate = date,
				prehba1cdatediff = datediff
		) %>%
		
		select(-c(testvalue, min_timediff))

## Join HbA1c to main table
baseline_biomarkers <- baseline_biomarkers %>%
		rename(
			prehba1c2yrs = prehba1c,
			prehba1cdate2yrs = prehba1cdate,
			prehba1cdatediff2yrs = prehba1cdatediff
		) %>%
		left_join(baseline_hba1c, by = "serialno") %>%
		relocate(height, .after = prehba1cdatediff)


save(baseline_biomarkers, file = "/home/pcardoso/workspace/SDRN-Cohort-scripts/Interim_Datasets/prev_baseline_biomarkers.RData")







