# Author: pcardoso
###############################################################################

# Identify those with diabetes type 2, collect comorbidities for combos

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

# Extracts dates for comorbidity code occurrences in GP and HES records

# Merges with drug start and stop dates

# Then finds earliest predrug, latest predrug, and earliest postdrug occurrence for each comorbidity

# Plus binary 'is there a predrug occurrence?' variables

# Also find binary yes/no whether they had hospital admission in previous year to drug start, and when first postdrug hospital admission was for any cause, and what this cause is


###############################################################################

# Define comorbidities
## If you add comorbidity to the end of this list, code should run fine to incorporate new comorbidity
## Missing = fh_diabetes, fh_premature_cvd, frailty_simple, hosp_cause_majoramputation, hosp_cause_minoramputation, osteoporosis, photocoagulation, revasc, solidorgantransplant

comorbids <- c(
		"af", "angina", "anxiety_disorders", "asthma", "benignprostatehyperplasia", "bronchiectasis",
		"ckd5_code", "cld", "copd", "cysticfibrosis", "dementia", "diabeticnephropathy", "dka",
		"falls", "fh_diabetes", "fh_premature_cvd", "frailty_simple", "haem_cancer", 
		"heartfailure", "hosp_cause_majoramputation", "hosp_cause_minoramputation", "hypertension", "ihd",  "incident_mi",
		"incident_stroke", "lowerlimbfracture", "micturition_control", "myocardialinfarction", "neuropathy",
		"osteoporosis", "otherneuroconditions", "pad", "photocoagulation", "pulmonaryfibrosis", "pulmonaryhypertension", "retinopathy",
		"revasc", "rheumatoidarthritis", "solid_cancer", "solidorgantransplant", "stroke", "tia", "unstableangina", "urinary_frequency",
		"vitreoushemorrhage", "volume_depletion"
)

comorbidity_ICD10_table <- readRDS("/home/pcardoso/workspace/SDRN-Cohort-scripts/Codelists/Comorbidities/comorbidity_ICD10_table.rds")
comorbidity_OPCS4_table <- readRDS("/home/pcardoso/workspace/SDRN-Cohort-scripts/Codelists/Comorbidities/comorbidity_OPCS4_table.rds")

###############################################################################

# Pull out all raw code instancas

# Should it count death records too?

for (i in comorbids) {
	
	print(i)
	
	if (length(comorbidity_ICD10_table[[i]]) >0) {
		
		raw_tablename <- paste0("raw_", i, "_icd10")
		
		mysqlquery <- paste0("
						SELECT o_condition.* 
						FROM o_condition, o_concept_condition, o_person 
						WHERE
						o_person.date_of_birth < '2022-11-01' AND o_person.dm_type = 2 AND o_person.earliest_mention IS NOT NULL AND 
						o_condition.serialno = o_person.serialno AND
						o_concept_condition.name = 'icd10' AND o_concept_condition.uid = o_condition.concept_id AND
						o_concept_condition.path LIKE 'smr%' AND o_concept_condition.path NOT LIKE '%causeofdeath%' AND
						(", paste("o_condition.condition_code LIKE ", paste(paste("'", comorbidity_ICD10_table[[i]], "%'", sep = ""), collapse = " OR o_condition.condition_code LIKE ")), ")")
		
		data <- dbGetQueryMap(con, mysqlquery)
		
		print(paste("Number of rows", raw_tablename, nrow(data)))
		
		assign(raw_tablename, data)
		
	}
	
	if (length(comorbidity_OPCS4_table[[i]]) >0) {
		
		raw_tablename <- paste0("raw_", i, "_opcs4")
		
		mysqlquery <- paste0("
						SELECT o_condition.* 
						FROM o_condition, o_concept_condition, o_person 
						WHERE
						o_person.date_of_birth < '2022-11-01' AND o_person.dm_type = 2 AND o_person.earliest_mention IS NOT NULL AND 
						o_condition.serialno = o_person.serialno AND
						o_concept_condition.name = 'opcs4' AND o_concept_condition.uid = o_condition.concept_id AND
						o_concept_condition.path LIKE 'smr%' AND o_concept_condition.path NOT LIKE '%causeofdeath%' AND
						(", paste("o_condition.condition_code LIKE ", paste(paste("'", comorbidity_OPCS4_table[[i]], "%'", sep = ""), collapse = " OR o_condition.condition_code LIKE ")), ")")
		
		data <- dbGetQueryMap(con, mysqlquery)
		
		print(paste("Number of rows", raw_tablename, nrow(data)))
		
		assign(raw_tablename, data)
		
	}
	
}


# Make new primary cause hospitalisation for heart failure, incident MI, and incident stroke comorbidities
# This uses SMR01 (hospital inpatient) diag1 as the main cause for hospitalisation

raw_primary_hhf_icd10 <- dbGetQueryMap(con, paste0("
					SELECT o_condition.* 
						FROM o_condition, o_concept_condition, o_person 
						WHERE
						o_person.date_of_birth < '2022-11-01' AND o_person.dm_type = 2 AND o_person.earliest_mention IS NOT NULL AND 
						o_condition.serialno = o_person.serialno AND
						o_concept_condition.name = 'icd10' AND o_concept_condition.uid = o_condition.concept_id AND
						o_concept_condition.path LIKE 'smr01-diag1%' AND o_concept_condition.path NOT LIKE '%causeofdeath%' AND
						(", paste("o_condition.condition_code LIKE ", paste(paste("'", comorbidity_ICD10_table[["heartfailure"]], "%'", sep = ""), collapse = " OR o_condition.condition_code LIKE ")), ")"))
		
raw_primary_incident_mi_icd10 <- dbGetQueryMap(con, paste0("
					SELECT o_condition.* 
						FROM o_condition, o_concept_condition, o_person 
						WHERE
						o_person.date_of_birth < '2022-11-01' AND o_person.dm_type = 2 AND o_person.earliest_mention IS NOT NULL AND 
						o_condition.serialno = o_person.serialno AND
						o_concept_condition.name = 'icd10' AND o_concept_condition.uid = o_condition.concept_id AND
						o_concept_condition.path LIKE 'smr01-diag1%' AND o_concept_condition.path NOT LIKE '%causeofdeath%' AND
						(", paste("o_condition.condition_code LIKE ", paste(paste("'", comorbidity_ICD10_table[["incident_mi"]], "%'", sep = ""), collapse = " OR o_condition.condition_code LIKE ")), ")"))
		
raw_primary_incident_stroke_icd10 <- dbGetQueryMap(con, paste0("
					SELECT o_condition.* 
						FROM o_condition, o_concept_condition, o_person 
						WHERE
						o_person.date_of_birth < '2022-11-01' AND o_person.dm_type = 2 AND o_person.earliest_mention IS NOT NULL AND 
						o_condition.serialno = o_person.serialno AND
						o_concept_condition.name = 'icd10' AND o_concept_condition.uid = o_condition.concept_id AND
						o_concept_condition.path LIKE 'smr01-diag1%' AND o_concept_condition.path NOT LIKE '%causeofdeath%' AND
						(", paste("o_condition.condition_code LIKE ", paste(paste("'", comorbidity_ICD10_table[["incident_stroke"]], "%'", sep = ""), collapse = " OR o_condition.condition_code LIKE ")), ")"))
		
comorbids <- c("primary_hhf", "primary_incident_mi", "primary_incident_stroke", comorbids)


## Separate frailty by severity into three different categories
### Add to beginning of list
#raw_frailty_mild_medcodes <- raw_frailty_simple_medcodes %>% filter(frailty_simple_cat == "Mild")
#raw_frailty_moderate_medcodes <- raw_frailty_simple_medcodes %>% filter(frailty_simple_cat == "Moderate")
#raw_frailty_severe_medcodes <- raw_frailty_simple_medcodes %>% filter(frailty_simple_cat == "Severe")
#comorbids <- setdiff(comorbids, "frailty_simple")
#comorbids <- c("frailty_mild", "frailty_moderate", "frailty_severe", comorbids)


### Separate family history by whether positive or negative
#### Add to beginning of list
#raw_fh_diabetes_positive_medcodes <- raw_fh_diabetes_medcodes %>% fitler(fh_diabetes_cat!="negative")
#raw_fh_diabetes_negative_medcodes <- raw_fh_diabetes_medcodes %>% fitler(fh_diabetes_cat=="negative")
#comorbids <- setdiff(comorbids, "fh_diabetes")
#comorbids <- c("fh_diabetes_positive", "fh_diabetes_negative", comorbids)



###############################################################################

# Clean and combine medcodes, ICD10 codes and OPCS4 codes, then merge with drug start dates
## Remove medcodes and HES codes before DOB or after Lcd/deregistration

# Get drug start dates
load("/home/pcardoso/workspace/SDRN-Cohort-scripts/Interim_Datasets/mm_drug_start_stop.RData")

# Clean comorbidity date and combine with drug start dates
for (i in comorbids) {
	
	print(paste("merging drug dates with", i, "code occurrences"))
	
	drug_merge_tablename <- paste0("full_", i, "_drug_merge")
	
	icd10_tablename <- paste0("raw_", i, "_icd10")
	opcs4_tablename <- paste0("raw_", i, "_opcs4")
	
	if (exists(icd10_tablename)) {
		
		icd10_codes <- get(icd10_tablename) %>%
				select(serialno, date = startdate, code = condition.code)
		
	}
	
	if (exists(opcs4_tablename)) {
		
		opcs4_codes <- get(opcs4_tablename) %>%
				select(serialno, date = startdate, code = condition.code)
		
	}
	
	
	if (exists("icd10_codes")) {
		
		all_codes <- icd10_codes
		rm(icd10_codes)
		
		if (exists("opcs4_codes")) {
			
			all_codes <- all_codes %>%
					union_all(opcs4_codes)
			rm(opcs4_codes)
		}
		
	} else  if (exists("opcs4_codes")) {
		
		all_codes <- opcs4_codes
		rm(opcs4_codes)
	} else {
		
		next
		
	}
	
	# filter for codes before end of records (not needed for SDRN)	
	all_codes_clean <- all_codes
	rm(all_codes)
	
	data <- all_codes_clean %>%
			inner_join((drug_start_stop %>% select(serialno, dstartdate, drug_class, drug_substance, drug_instance)), by = c("serialno")) %>%
			mutate(drugdatediff = difftime(date, dstartdate, units = "days"))
	
	rm(all_codes_clean)
	
	assign(drug_merge_tablename, data)
	
	rm(data)
	
}


###############################################################################

# Find earliest predrug, lastest predrug and first postdrug dates
## Leave amputation and family history of diabetes for now as need to be processed differently

cmorbids <- setdiff(comorbids, c("hosp_cause_majoramputation", "hosp_cause_minoramputation", "fh_diabetes_positive", "fh_diabetes_negative"))

comorbidities <- drug_start_stop %>%
		select(serialno, dstartdate, drug_class, drug_substance, drug_instance)


for (i in comorbids) {
	
	print(paste("working out predrug and postdrug code occurrences for", i))
	
	drug_merge_tablename <- paste0("full_", i, "_drug_merge")
	interim_comorbidity_table <- paste0("comorbidities_interim_", i)
	predrug_earliest_date_variable <- paste0("predrug_earliest_", i)
	predrug_latest_date_variable <- paste0("predrug_latest_", i)
	predrug_variable <- paste0("predrug_", i)
	postdrug_date_variable <- paste0("postdrug_first_", i)
	
	if (exists(drug_merge_tablename)) {
		
		predrug <- get(drug_merge_tablename) %>%
				filter(date<=dstartdate) %>%
				group_by(serialno, dstartdate, drug_substance) %>%
				summarise(
						{{predrug_earliest_date_variable}}:=min(date, na.rm = TRUE),
						{{predrug_latest_date_variable}}:=max(date, na.rm = TRUE)
				) %>%
				ungroup()
		
		postdrug <- get(drug_merge_tablename) %>%
				filter(date>dstartdate) %>%
				group_by(serialno, dstartdate, drug_substance) %>%
				summarise({{postdrug_date_variable}}:=min(date, na.rm = TRUE)) %>%
				ungroup()
		
		comorbidities <- comorbidities %>%
				left_join(predrug, by = c("serialno", "dstartdate", "drug_substance")) %>%
				left_join(postdrug, by = c("serialno", "dstartdate", "drug_substance")) %>%
				mutate({{predrug_variable}}:=as.numeric(!is.na(!!as.name(predrug_earliest_date_variable))))
		
	} else {
		
		next
		
	}
	
}


###############################################################################

# Make separate tables for amputation and fh_diabetes as need to combine 2 x amputation codes / combine positive and negative fh_diabetes codes first

#
# Not coded
#
#
#


save(comorbidities, file = "/home/pcardoso/workspace/SDRN-Cohort-scripts/Interim_Datasets/mm_comorbidities.RData")

