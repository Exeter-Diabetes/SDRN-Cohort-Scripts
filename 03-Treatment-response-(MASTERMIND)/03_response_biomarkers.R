# Author: pcardoso
###############################################################################

# Collect response biomarkers for combos
# Each step checks if it can load the table, otherwise creates table, saves the table to directory, deletes the table
## Save on loaded memory

###############################################################################

rm(list=ls())
gc()

# load libraries
library(tidyverse)


###############################################################################

## connection to database
con <- dbConn("NDS_2023")

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
	
	# check if file exists, otherise run
	file_name <- paste0("/home/pcardoso/workspace/SDRN-Cohort-scripts/Interim_Datasets/mm_", clean_tablename, ".RData")
	if (!file.exists(file_name)) {
		
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
		
		do.call(save, list(clean_tablename, file = file_name))
		
		rm(list = c(clean_tablename, "data"))
		
	}
	
	
}

if (!file.exists("/home/pcardoso/workspace/SDRN-Cohort-scripts/Interim_Datasets/mm_clean_hba1c_medcodes.RData")) {
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
	
	do.call(save, list("clean_hba1c_medcodes", file = "/home/pcardoso/workspace/SDRN-Cohort-scripts/Interim_Datasets/mm_clean_hba1c_medcodes.RData"))
	
	rm(list = "clean_hba1c_medcodes")
}


if (!file.exists("/home/pcardoso/workspace/SDRN-Cohort-scripts/Interim_Datasets/mm_clean_egfr_medcodes.RData")) {
	load("/home/pcardoso/workspace/SDRN-Cohort-scripts/Interim_Datasets/mm_clean_creatinine_blood_medcodes.RData")
	
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
	
	do.call(save, list("clean_egfr_medcodes", file = "/home/pcardoso/workspace/SDRN-Cohort-scripts/Interim_Datasets/mm_clean_egfr_medcodes.RData"))
	
	rm(list = c("clean_egfr_medcodes", "clean_creatinine_blood_medcodes"))
}


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
	
	file_name <- paste0("/home/pcardoso/workspace/SDRN-Cohort-scripts/Interim_Datasets/mm_", drug_merge_tablename, ".RData")
	if (!file.exists(file_name)) {
		
		load(paste0("/home/pcardoso/workspace/SDRN-Cohort-scripts/Interim_Datasets/mm_", clean_tablename, ".RData"))
		
		data <- get(clean_tablename) %>%
				inner_join(drug_start_dates, by = c("serialno")) %>%
				mutate(drugdatediff = difftime(date, dstartdate, units = "days"))
		
		assign(drug_merge_tablename, data)
		
		do.call(save, list(drug_merge_tablename, file = file_name))
		
		rm(list = c(clean_tablename, drug_merge_tablename))
	}
	
}


###############################################################################

# Pull out 6 month and 12 month biomarker values

## Loop through full biomarker drug merge tables

## Just keep first instance

## Define earliest (min) and latest (last) valid date for each response length (6m and 12m)
### Earliet = 3 months for 6m response/9 months for 12m response
### Latest = minimum of timetochange + 91 days, timetoaddrem and 9 months (for 6m response)/15 months (for 12m response)

## Then use closest date to 6/12 months post drug start date
### May be multiple values; use minimum
### Can get duplicates where person has identical results on the same day/days equidistant from 6/12 months post drug start - choose first row when ordered by drugdatediff

# Then combine with baseline values and find response
## Remove HbA1c responses where timeprevcombo<=61 days i.e. where change glucose-lowering meds less than 61 days before current drug initiation


biomarkers <- c("weight", "bmi", "fastingglucose", "hdl", "triglyceride", "creatinine_blood", "ldl", "alt", "ast", "totalcholesterol", "dbp", "sbp", "acr", "hba1c", "egfr", "bilirubin", "haemoglobin", "pcr")


# clear unused memory
gc()


# 6 month response
for (i in biomarkers) {
	
	print(i)
	
	drug_merge_tablename <- paste0("full_", i, "_drug_merge")
	post6m_table_name <- paste0("post6m_", i)
	
	file_name <- paste0("/home/pcardoso/workspace/SDRN-Cohort-scripts/Interim_Datasets/mm_", post6m_table_name, ".RData")
	if (!file.exists(file_name)) {
		
		load(paste0("/home/pcardoso/workspace/SDRN-Cohort-scripts/Interim_Datasets/mm_", drug_merge_tablename, ".RData"))
		
		data <- get(drug_merge_tablename) %>%
				
				filter(drug_instance==1) %>%
				
				mutate(
						
						minvaliddate6m = as.Date(dstartdate, origin = "1970-01-01") + as.difftime(91, unit = "days"),
						minvaliddate6m = as.Date(minvaliddate6m, origin = "1970-01-01"),
						
						maxtime6m = pmin(ifelse(is.na(timetoaddrem_class), 274, timetoaddrem_class),
								ifelse(is.na(timetochange_class), 274, timetochange_class+91), 274, na.rm = TRUE),
						
						lastvaliddate6m = ifelse(maxtime6m<91, NA, as.Date(dstartdate, origin = "1970-01-01") + as.difftime(maxtime6m, units = "days")),
						lastvaliddate6m = as.Date(lastvaliddate6m, origin = "1970-01-01")
				
				) %>%
				
				filter(date>=minvaliddate6m) %>%
				
				filter(date<=lastvaliddate6m)
		
		data <- data %>%
				
				group_by(serialno, dstartdate, drug_substance) %>%
				
				
				mutate(
						interim_var_1 = 183-drugdatediff,
						interim_var_2 = ifelse(interim_var_1<0, 0-interim_var_1, interim_var_1) # abs() function did not work due to running out of memory
				) %>%
				
				mutate(min_timediff = min(interim_var_2, na.rm = TRUE)) %>%
				
				filter(interim_var_2 == min_timediff) %>%
				
				mutate(post_biomarker_6m=min(testvalue, na.rm = TRUE)) %>%
				filter(post_biomarker_6m==testvalue) %>%
				
				rename(
						post_biomarker_6mdate = date,
						post_biomarker_6mdrugdiff = drugdatediff
				) %>%
				
				arrange(post_biomarker_6mdrugdiff) %>%
				filter(row_number()==1) %>%
				
				ungroup() %>%
				
				select(serialno, dstartdate, drug_class, drug_substance, post_biomarker_6m, post_biomarker_6mdate, post_biomarker_6mdrugdiff)
		
		assign(post6m_table_name, data)
		
		do.call(save, list(post6m_table_name, file = file_name))
		
		rm(list = c(drug_merge_tablename, post6m_table_name))
	}
}


# 12 month response
for (i in biomarkers) {
	
	print(i)
	
	drug_merge_tablename <- paste0("full_", i, "_drug_merge")
	post12m_table_name <- paste0("post12m_", i)
	
	file_name <- paste0("/home/pcardoso/workspace/SDRN-Cohort-scripts/Interim_Datasets/mm_", post12m_table_name, ".RData")
	if (!file.exists(file_name)) {
		
		load(paste0("/home/pcardoso/workspace/SDRN-Cohort-scripts/Interim_Datasets/mm_", drug_merge_tablename, ".RData"))
		
		data <- get(drug_merge_tablename) %>%
				
				filter(drug_instance==1) %>%
				
				mutate(
						
						minvaliddate12m = as.Date(dstartdate, origin = "1970-01-01") + as.difftime(274, unit = "days"),
						minvaliddate12m = as.Date(minvaliddate12m, origin = "1970-01-01"),
						
						maxtime12m = pmin(ifelse(is.na(timetoaddrem_class), 457, timetoaddrem_class),
								ifelse(is.na(timetochange_class), 457, timetochange_class+91), 457, na.rm = TRUE),
						
						lastvaliddate12m = ifelse(maxtime12m<274, NA, as.Date(dstartdate, origin = "1970-01-01") + as.difftime(maxtime12m, units = "days")),
						lastvaliddate12m = as.Date(lastvaliddate12m, origin = "1970-01-01")
				
				) %>%
				
				filter(date>=minvaliddate12m) %>%
				filter(date<=lastvaliddate12m)
		
		data <- data %>%
				
				group_by(serialno, dstartdate, drug_substance) %>%
				
				
				mutate(
						interim_var_1 = 365-drugdatediff,
						interim_var_2 = ifelse(interim_var_1<0, 0-interim_var_1, interim_var_1) # abs() function did not work due to running out of memory
				) %>%
				
				mutate(min_timediff = min(interim_var_2, na.rm = TRUE)) %>%
				
				filter(interim_var_2 == min_timediff) %>%
				
				mutate(post_biomarker_12m=min(testvalue, na.rm = TRUE)) %>%
				filter(post_biomarker_12m==testvalue) %>%
				
				rename(
						post_biomarker_12mdate = date,
						post_biomarker_12mdrugdiff = drugdatediff
				) %>%
				
				arrange(post_biomarker_12mdrugdiff) %>%
				filter(row_number()==1) %>%
				
				ungroup() %>%
				
				select(serialno, dstartdate, drug_class, drug_substance, post_biomarker_12m, post_biomarker_12mdate, post_biomarker_12mdrugdiff)
		
		assign(post12m_table_name, data)
		
		do.call(save, list(post12m_table_name, file = file_name))
		
		rm(list = c(drug_merge_tablename, post12m_table_name))
	}
}


# Combine with baseline values and find reponse

load("/home/pcardoso/workspace/SDRN-Cohort-scripts/Interim_Datasets/mm_baseline_biomarkers.RData")

load("/home/pcardoso/workspace/SDRN-Cohort-scripts/Interim_Datasets/mm_combo_start_stop.RData")


response_biomarkers <- baseline_biomarkers %>%
		left_join((combo_start_stop %>%
							select(serialno, dcstartdate, timeprevcombo_class)), by = c("serialno", "dstartdate"="dcstartdate")) %>%
		filter(drug_instance==1)

for (i in biomarkers) {
	
	print(i)
	
	post6m_table <- paste0("post6m_", i)
	post12m_table <- paste0("post12m_", i)
	
	load(paste0("/home/pcardoso/workspace/SDRN-Cohort-scripts/Interim_Datasets/mm_", post6m_table, ".RData"))
	load(paste0("/home/pcardoso/workspace/SDRN-Cohort-scripts/Interim_Datasets/mm_", post12m_table, ".RData"))
	
	pre_biomarker_variable <- paste0("pre", i)
	pre_biomarker_date_variable <- paste0("pre", i, "date")
	pre_biomarker_drugdiff_variable <- paste0("pre", i, "drugdiff")
	
	post_6m_biomarker_variable <- paste0("post", i, "6m")
	post_6m_biomarker_date_variable <- paste0("post", i, "6mdate")
	post_6m_biomarker_drugdiff_variable <- paste0("post", i, "6mdrugdiff")
	biomarker_6m_response_variable <- paste0(i, "resp6m")
	post_12m_biomarker_variable <- paste0("post", i, "12m")
	post_12m_biomarker_date_variable <- paste0("post", i, "12mdate")
	post_12m_biomarker_drugdiff_variable <- paste0("post", i, "12mdrugdiff")
	biomarker_12m_response_variable <- paste0(i, "resp12m")
	
	response_biomarkers <- response_biomarkers %>%
			left_join((get(post6m_table) %>% select(-drug_class)), by = c("serialno", "dstartdate", "drug_substance")) %>%
			left_join((get(post12m_table) %>% select(-drug_class)), by = c("serialno", "dstartdate", "drug_substance"))
	
	if (i=="hba1c") {
		
		response_biomarkers <- response_biomarkers %>%
				mutate(
						post_biomarker_6m = ifelse(!is.na(timeprevcombo_class) & timeprevcombo_class<=61, NA, post_biomarker_6m),
						post_biomarker_6mdate = ifelse(!is.na(timeprevcombo_class) & timeprevcombo_class<=61, as.Date(NA), post_biomarker_6mdate),
						post_biomarker_6mdate = as.Date(post_biomarker_6mdate, origin = "1970-01-01"),
						post_biomarker_6mdrugdiff = ifelse(!is.na(timeprevcombo_class) & timeprevcombo_class<=61, NA, post_biomarker_6mdrugdiff),
						post_biomarker_12m = ifelse(!is.na(timeprevcombo_class) & timeprevcombo_class<=61, NA, post_biomarker_12m),
						post_biomarker_12mdate = ifelse(!is.na(timeprevcombo_class) & timeprevcombo_class<=61, as.Date(NA), post_biomarker_12mdate),
						post_biomarker_12mdate = as.Date(post_biomarker_12mdate, origin = "1970-01-01"),
						post_biomarker_12mdrugdiff = ifelse(!is.na(timeprevcombo_class) & timeprevcombo_class<=61, NA, post_biomarker_12mdrugdiff)
				)
		
	}
	
	response_biomarkers <- response_biomarkers %>%
			relocate(pre_biomarker_variable, .before = post_biomarker_6m) %>%
			relocate(pre_biomarker_date_variable, .before = post_biomarker_6m) %>%
			relocate(pre_biomarker_drugdiff_variable, .before = post_biomarker_6m) %>%
			
			mutate(
					{{biomarker_6m_response_variable}}:=ifelse(!is.na(pre_biomarker_variable) & !is.na(post_biomarker_6m), post_biomarker_6m-!!as.name(pre_biomarker_variable), NA),
					{{biomarker_12m_response_variable}}:=ifelse(!is.na(pre_biomarker_variable) & !is.na(post_biomarker_12m), post_biomarker_12m-!!as.name(pre_biomarker_variable), NA)
			) %>%
			
			rename(
					{{post_6m_biomarker_variable}}:=post_biomarker_6m,
					{{post_6m_biomarker_date_variable}}:=post_biomarker_6mdate,
					{{post_6m_biomarker_drugdiff_variable}}:=post_biomarker_6mdrugdiff,
					{{post_12m_biomarker_variable}}:=post_biomarker_12m,
					{{post_12m_biomarker_date_variable}}:=post_biomarker_12mdate,
					{{post_12m_biomarker_drugdiff_variable}}:=post_biomarker_12mdrugdiff
			)
	
	rm(list = c(post6m_table, post12m_table))
	
}

###############################################################################

# Additional kidney outcomes

## eGFR

### Add in next eGFR measurement
load(paste0("/home/pcardoso/workspace/SDRN-Cohort-scripts/Interim_Datasets/mm_clean_egfr_medcodes.RData"))
egfr_long <- clean_egfr_medcodes

next_egfr <- baseline_biomarkers %>%
		select(serialno, drug_substance, dstartdate, preegfrdate) %>%
		left_join(egfr_long, by = "serialno") %>%
		filter(difftime(date, preegfrdate, units = "days")>0) %>%
		group_by(serialno, drug_substance, dstartdate) %>%
		summarise(next_egfr_date = min(date, na.rm = TRUE)) %>%
		ungroup() %>%
		mutate(next_egfr_date = as.Date(next_egfr_date, origin = "1970-01-01"))


### Add in 40% declince in eGFR outcome
### Join drug start dates with all longitudinal eGFR measurements, and only keep later eGFR measurements which are <=60% of the baseline value
### Checked and those with null eGFR do get dropped
egfr40 <- baseline_biomarkers %>%
		select(serialno, drug_substance, dstartdate, preegfr, preegfrdate) %>%
		left_join(egfr_long, by = c("serialno")) %>%
		filter(difftime(date, preegfrdate, units = "days")>0) %>%
		filter(testvalue<=0.6*preegfr) %>%
		group_by(serialno, drug_substance, dstartdate, preegfr) %>%
		summarise(egfr_40_decline_date = min(date, na.rm = TRUE)) %>%
		ungroup() %>%
		mutate(egfr_40_decline_date = as.Date(egfr_40_decline_date, origin = "1970-01-01")) %>%
		left_join(egfr_long, by = c("serialno")) %>%
		filter(difftime(date, egfr_40_decline_date, units = "days")>=28) %>%
		filter(testvalue<=0.6*preegfr) %>%
		distinct(serialno, drug_substance, dstartdate, egfr_40_decline_date)

### Add in 50% decline in eGFR outcome
### Join drug start dates with all longitudinal eGFR measurements, and only keep later eGFR measurements which are <=50% of the baseline values
### Checked and those with null eGFR do get dropped
egfr50 <- baseline_biomarkers %>%
		select(serialno, drug_substance, dstartdate, preegfr, preegfrdate) %>%
		left_join(egfr_long, by = c("serialno")) %>%
		filter(difftime(date, preegfrdate, units = "days")>0) %>%
		filter(testvalue<=0.5*preegfr) %>%
		group_by(serialno, drug_substance, dstartdate, preegfr) %>%
		summarise(egfr_50_decline_date = min(date, na.rm = TRUE)) %>%
		ungroup() %>%
		mutate(egfr_50_decline_date = as.Date(egfr_50_decline_date, origin = "1970-01-01")) %>%
		left_join(egfr_long, by = c("serialno")) %>%
		filter(difftime(date, egfr_50_decline_date, units = "days")>=28) %>%
		filter(testvalue<=0.5*preegfr) %>%
		distinct(serialno, drug_substance, dstartdate, egfr_50_decline_date)

## ACR

### Add in whether baseline microalbuminuria is confirmed by previous or next measurement
load(paste0("/home/pcardoso/workspace/SDRN-Cohort-scripts/Interim_Datasets/mm_clean_acr_medcodes.RData"))
acr_long <- clean_acr_medcodes %>%
		group_by(serialno, date) %>%
		mutate(count=n()) %>%
		ungroup() %>%
		filter(count==1) %>%
		select(serialno, date, testvalue)

prev_acr <- baseline_biomarkers %>%
		select(serialno, dstartdate, drug_substance, preacrdate) %>%
		left_join(acr_long, by = c("serialno")) %>%
		group_by(serialno, dstartdate, drug_substance, preacrdate) %>%
		arrange(date) %>%
		mutate(
				preacr_previous = ifelse(row_number()==1, NA, lag(testvalue)),
				preacr_previous_date = ifelse(row_number()==1, NA, lag(date)),
				preacr_previous_date = as.Date(preacr_previous_date, origin = "1970-01-01"),
				preacr_next = ifelse(row_number()==1, NA, lead(testvalue)),
				preacr_next_date = ifelse(row_number()==1, NA, lead(date)),
				preacr_next_date = as.Date(preacr_next_date, origin = "1970-01-01")
		) %>%
		ungroup()

prev_acr <- prev_acr %>%
		filter(date==preacrdate) %>%
		mutate(
				preacr_confirmed = ifelse(testvalue >= 3 & (preacr_previous >=3 & difftime(preacr_previous_date, preacrdate, units = "days") <=7 & difftime(preacr_previous_date, preacrdate, units = "days") >=-730 | preacr_next >= 3), TRUE, FALSE)
		) %>%
		select(serialno, dstartdate, drug_substance, preacr_confirmed, preacr_previous, preacr_previous_date, preacr_next, preacr_next_date)

### Add in new macroalbuminuria for those with confirmed microalbuminuria at baseline
new_macroalb <- baseline_biomarkers %>%
		select(serialno, dstartdate, drug_substance, preacrdate) %>%
		left_join(acr_long, by = c("serialno")) %>%
		left_join(prev_acr, by = c("serialno", "dstartdate", "drug_substance")) %>%
		filter(date>preacrdate)

new_macroalb <- new_macroalb %>%
		group_by(serialno, dstartdate, drug_substance) %>%
		arrange(date) %>%
		mutate(nextvalue = lead(testvalue)) %>%
		ungroup()

new_macroalb <- new_macroalb %>%
		filter(
				preacr_confirmed == TRUE & testvalue >= 30 |
						preacr_confirmed == FALSE & testvalue >=30 & nextvalue >=30
		) %>%
		group_by(serialno, drug_substance, dstartdate) %>%
		summarise(macroalb_date = min(date, na.rm = TRUE)) %>%
		ungroup() %>%
		mutate(macroalb_date = as.Date(macroalb_date, origin = "1970-01-01"))


###############################################################################

# Join to rest of response dataset and move where height variable is
response_biomarkers <- response_biomarkers %>%
		left_join(next_egfr, by = c("serialno", "drug_substance", "dstartdate")) %>%
		left_join(egfr40, by = c("serialno", "drug_substance", "dstartdate")) %>%
		left_join(egfr50, by = c("serialno", "drug_substance", "dstartdate")) %>%
		left_join(prev_acr, by = c("serialno", "drug_substance", "dstartdate")) %>%
		left_join(new_macroalb, by = c("serialno", "drug_substance", "dstartdate")) %>%
		relocate(height, .after=timeprevcombo_class) %>%
		relocate(prehba1c12m, .after=hba1cresp12m) %>%
		relocate(prehba1c12mdate, .after=prehba1c12m) %>%
		relocate(prehba1c12mdrugdiff, .after=prehba1c12mdate) %>%
		relocate(prehba1c2yrs, .after=hba1cresp12m) %>%
		relocate(prehba1c2yrsdate, .after=prehba1c2yrs) %>%
		relocate(prehba1c2yrsdrugdiff, .after=prehba1c2yrsdate)

save(response_biomarkers, file = "/home/pcardoso/workspace/SDRN-Cohort-scripts/Interim_Datasets/mm_response_biomarkers.RData")


