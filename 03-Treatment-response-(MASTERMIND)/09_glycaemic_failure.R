# Author: pcardoso
###############################################################################

# Identify those with diabetes type 2, discontinuation at 3-/6-/12-months

###############################################################################

# load libraries
library(tidyverse)


###############################################################################


# Setup dataset with type 2 diabetes

## connection to database
con <- dbConn("NDS_2023")
## select patients with type 2 diabetes
cohort.diabetestype2.raw <- dbGetQueryMap(con, "
				SELECT serialno, date_of_birth, date_of_death, earliest_mention, dm_type, gender, ethnic
				FROM o_person 
				WHERE date_of_birth < '2022-11-01' AND 
				dm_type = 2 AND earliest_mention IS NOT NULL")



###############################################################################

# Adds time to glycaemic failure to all drug class periods

# Glycaemic failure defined as two consecutive HbA1c>threshold or one HbA1c>threshold where HbA1c is last before a drug is added

# Thresholds used:
## 7.5% (58.46 mmol/mol)
## 8.5% (69.39 mmol/mol)
## Baseline HbA1c
## Baseline HbA1c - 0.5% (5.47 mmol/mol)


###############################################################################

load("/home/pcardoso/workspace/SDRN-Cohort-scripts/Interim_Datasets/mm_baseline_biomarkers.RData")

load("/home/pcardoso/workspace/SDRN-Cohort-scripts/Interim_Datasets/mm_drug_start_stop.RData")

load("/home/pcardoso/workspace/SDRN-Cohort-scripts/Interim_Datasets/mm_combo_start_stop.RData")

drug_start_dates <- drug_start_stop %>%
		left_join(combo_start_stop, by = c("serialno", "dstartdate" = "dcstartdate"))

# load full HbA1c drug merge
clean_hba1c_medcodes <- dbGetQueryMap(con, "
						SELECT o_observation.*
						FROM o_observation, o_concept_observation, o_person
						WHERE
						o_person.date_of_birth < '2022-11-01' AND o_person.dm_type = 2 AND o_person.earliest_mention IS NOT NULL AND
						o_observation.serialno = o_person.serialno AND
						o_observation.concept_id = o_concept_observation.uid AND
						o_concept_observation.path = 'biochem-hba1c'") %>%
		select(serialno, date, "testvalue" = num.value) %>%
		drop_na(testvalue) %>%
		mutate(testvalue = ifelse(testvalue<20, ((testvalue-2.152)/0.09148), testvalue)) %>%
		filter(testvalue >= 20 & testvalue <= 195) %>%
		group_by(serialno, date) %>%
		summarise(testvalue = mean(testvalue, na.rm = TRUE)) %>%
		ungroup()


## Merge with biomarkers and calculate date difference between biomarker and drug start date
for (i in c("hba1c")) {
	
	print(i)
	
	clean_tablename <- paste0("clean_", i, "_medcodes")
	drug_merge_tablename <- paste0("full_", i, "_drug_merge")
	
	data <- get(clean_tablename) %>%
			inner_join(drug_start_dates, by = c("serialno")) %>%
			mutate(drugdatediff = difftime(date, dstartdate, units = "days"))
	
	assign(drug_merge_tablename, data)
	
}

# And drug class periods with required variables from drug sorting scripts (drug_sorting_and_combos)
load("/home/pcardoso/workspace/SDRN-Cohort-scripts/Interim_Datasets/mm_drug_class_start_stop.RData")

load("/home/pcardoso/workspace/SDRN-Cohort-scripts/Interim_Datasets/mm_combo_class_start_stop.RData")

drug_periods <- drug_class_start_stop %>%
		left_join((combo_class_start_stop %>% select(serialno, dcstartdate, nextdrugchange, nextdcdate, dcstartdate, timetochange_class, timetoaddrem_class)), by = c("serialno", "dstartdate_class"="dcstartdate")) %>%
		select(serialno, dstartdate=dstartdate_class, dstopdate=dstopdate_class, drug_class, timetochange=timetochange_class, timetoaddrem=timetoaddrem_class, nextdrugchange, nextdcdate)


###############################################################################

# Code up glycaemic failure variables

# Make new table of all drug periods with baseline HbA1c and fail thresholds defined
## Thresholds need to be in same format as HbA1cs to able to compare

glycaemic_failure_thresholds <- drug_periods %>%
		inner_join((baseline_biomarkers %>% select(serialno, dstartdate, drug_class, prehba1c, prehba1cdate)), by = c("serialno", "dstartdate", "drug_class")) %>%
		distinct() %>%
		mutate(
			threshold_7.5 = 58,
			threshold_8.5 = 70,
			threshold_baseline = prehba1c,
			threshold_baseline_0.5 = prehba1c-5.5
		)

# Baseline biomarkers has duplicates for serialno-dstartdate-drug_class for multiple substances - but prehba1c and prehba1cdate will be the same
# Same for full_hba1c_drug_merge below


# Join with HbA1cs during 'failure period' - more than 90 days after drugstart and no later than when diabetes drugs changed (doesn't take into account gaps)

glycaemic_failure_hba1cs <- glycaemic_failure_thresholds %>%
		left_join((full_hba1c_drug_merge %>% filter(drugdatediff>90) %>% distinct(serialno, dstartdate, drug_class, testvalue, date)), by = c("serialno", "dstartdate", "drug_class")) %>%
		filter(date<=nextdcdate) %>%
		group_by(serialno, dstartdate, drug_class) %>%
		mutate(latest_fail_hba1c=max(date, na.rm = TRUE)) %>%
		ungroup()


# Make variables for each threshold:
## Fail date: earliest of nextdcdate, two consecutive HbA1cs over threshold, one HbA1c over threshold followed by drug being added (before next HbA1c) - within 'failure period' defined as above
## Fail reason: which of the above 3 scenarios the fail date represents

thresholds <- c("7.5", "8.5", "baseline", "baseline_0.5")

glycaemic_failure <- glycaemic_failure_hba1cs

for (i in thresholds) {
	
	threshold_value <- paste0("threshold_", i)
	fail_date <- paste0("hba1c_fail_", i, "_date")
	fail_reason <- paste0("hba1c_fail_", i, "_reason")
	
	glycaemic_failure <- glycaemic_failure %>%
			group_by(serialno, dstartdate, drug_class) %>%
			arrange(date) %>%
			
			mutate(
				threshold_double = ifelse(testvalue>!!as.name(threshold_value) & lead(testvalue)>!!as.name(threshold_value), 1, 0),
				threshold_single_and_add = ifelse(testvalue>!!as.name(threshold_value) & date==latest_fail_hba1c & nextdrugchange=="add", 1, 0),
				fail_date = ifelse((!is.na(threshold_double) & threshold_double==1) | (!is.na(threshold_single_and_add) & threshold_single_and_add==1), date,
						ifelse(is.na(!!as.name(threshold_value)), as.Date(NA), nextdcdate)),
				fail_date = as.Date(fail_date, origin = "1970-01-01"),
				threshold_double_period = max(threshold_double, na.rm = TRUE),
				threshold_single_and_add_period = max(threshold_single_and_add, na.rm = TRUE),
				{{fail_date}}:=min(fail_date, na.rm = TRUE),
				{{fail_reason}}:=ifelse(is.na(!!as.name(threshold_value)), NA,
						ifelse(!is.na(threshold_double) & threshold_double_period==1, "Fail - 2 HbA1cs >threshold",
								ifelse(!is.na(threshold_single_and_add) & threshold_single_and_add_period==1, "Fail - 1 HbA1cs >threshold then add drug",
										ifelse(nextdcdate==dstopdate, "End of prescriptions", "Change in diabetes drugs"))))
			) %>%
			
			ungroup() %>%
			
			select(-c(threshold_double, threshold_single_and_add, fail_date, threshold_double_period, threshold_single_and_add_period))
	
}


glycaemic_failure <- glycaemic_failure %>%
		group_by(serialno, dstartdate, drug_class) %>%
		filter(row_number()==1) %>%
		ungroup() %>%
		arrange(serialno, dstartdate, drug_class) %>%
		select(-c(testvalue, date, latest_fail_hba1c))


###############################################################################

# Add in whether threshold was ever reached (obviously not very useful for baseline threshold, but do want for others)

## Join failure dates and thresholds with HbA1cs going right back to and including baseline HbA1c and no later than when diabetes drugs changed (doesn't take into account gaps) so that we can find if they were ever at/below the threshold

glycaemic_failure_threshold_hba1cs <- glycaemic_failure %>%
		inner_join((full_hba1c_drug_merge %>%
							distinct(serialno, dstartdate, drug_class, testvalue, date)), by = c("serialno", "dstartdate", "drug_class")) %>%
		filter(date<=nextdcdate & date>=prehba1cdate) %>%
		select(serialno, dstartdate, drug_class, testvalue, date)

glycaemic_failure_threshold_hba1cs <- glycaemic_failure %>%
		left_join(glycaemic_failure_threshold_hba1cs, by = c("serialno", "dstartdate", "drug_class"))



## Fail threshold reached: whether there is an HbA1c at/below the threshold value prior to failure
glycaemic_failure_thresholds_reached <- glycaemic_failure_threshold_hba1cs

for (i in thresholds) {
	
	threshold_value <- paste0("threshold_", i)
	fail_date <- paste0("hba1c_fail_", i, "_date")
	fail_threshold_reached <- paste0("hba1c_fail_", i, "_reached")
	
	glycaemic_failure_thresholds_reached <- glycaemic_failure_thresholds_reached %>%
			group_by(serialno, dstartdate, drug_class) %>%
			
			mutate(
				threshold_reached = ifelse(!is.na(testvalue) & testvalue<=!!as.name(threshold_value) & date<=!!as.name(fail_date), 1, 0),
				threshold_reached = ifelse(is.na(!!as.name(threshold_value)), NA, threshold_reached),
				{{fail_threshold_reached}}:=max(threshold_reached, na.rm = TRUE)
			) %>%
			
			ungroup() %>%
			
			select(-threshold_reached)
	
}

glycaemic_failure <- glycaemic_failure_thresholds_reached %>%
		group_by(serialno, dstartdate, drug_class) %>%
		filter(row_number() == 1) %>%
		ungroup() %>%
		arrange(serialno, dstartdate, drug_class) %>%
		select(-c(testvalue, date)) %>%
		relocate(hba1c_fail_7.5_reached, .after=hba1c_fail_7.5_reason) %>%
		relocate(hba1c_fail_8.5_reached, .after=hba1c_fail_8.5_reason) %>%
		relocate(hba1c_fail_baseline_reached, .after=hba1c_fail_baseline_reason) %>%
		relocate(hba1c_fail_baseline_0.5_reached, .after=hba1c_fail_baseline_0.5_reason) %>%
		mutate(
			hba1c_fail_baseline_reached = ifelse(is.infinite(hba1c_fail_baseline_reached), NA, hba1c_fail_baseline_reached),
			hba1c_fail_baseline_0.5_reached = ifelse(is.infinite(hba1c_fail_baseline_0.5_reached), NA, hba1c_fail_baseline_0.5_reached)
		)

save(glycaemic_failure, file = "/home/pcardoso/workspace/SDRN-Cohort-scripts/Interim_Datasets/mm_glycaemic_failure.RData")





