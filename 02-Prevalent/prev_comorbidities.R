# Author: pcardoso
###############################################################################

# Identify those with diabetes at prevalence, collect comorbidities

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

## connection to database
con <- dbConn("NDS_2023")

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


# Make new primary cause hospitalisation for heart failure, incident MI, and incident stroke comorbidities
# This uses SMR01 (hospital inpatient) diag1 as the main cause for hospitalisation

codelist_primary_hff <- clean_codes(read.delim("/home/pcardoso/workspace/SDRN-Cohort-scripts/Codelists/ICD10/exeter_icd10_heartfailure.txt") %>%
				select(-Term_description) %>%
				unlist())

raw_primary_hhf_icd10 <- dbGetQueryMap(con, paste0("
						SELECT o_condition.* 
						FROM o_condition, o_concept_condition 
						WHERE
						o_concept_condition.name = 'icd10' AND o_concept_condition.uid = o_condition.concept_id AND
						o_concept_condition.path LIKE 'smr01-diag1%' AND o_concept_condition.path NOT LIKE '%causeofdeath%' AND
						(", paste("o_condition.condition_code LIKE ", paste(paste("'", codelist_primary_hff, "%'", sep = ""), collapse = " OR o_condition.condition_code LIKE ")), ")"))

codelist_incident_mi <- clean_codes(read.delim("/home/pcardoso/workspace/SDRN-Cohort-scripts/Codelists/ICD10/exeter_icd10_incident_mi.txt") %>%
				select(-Term_description) %>%
				unlist())

raw_primary_incident_mi_icd10 <- dbGetQueryMap(con, paste0("
						SELECT o_condition.* 
						FROM o_condition, o_concept_condition 
						WHERE
						o_concept_condition.name = 'icd10' AND o_concept_condition.uid = o_condition.concept_id AND
						o_concept_condition.path LIKE 'smr01-diag1%' AND o_concept_condition.path NOT LIKE '%causeofdeath%' AND
						(", paste("o_condition.condition_code LIKE ", paste(paste("'", codelist_incident_mi, "%'", sep = ""), collapse = " OR o_condition.condition_code LIKE ")), ")"))

codelist_incident_stroke <- clean_codes(read.delim("/home/pcardoso/workspace/SDRN-Cohort-scripts/Codelists/ICD10/exeter_icd10_incident_stroke.txt") %>%
				select(-Term_description) %>%
				unlist())

raw_primary_incident_stroke_icd10 <- dbGetQueryMap(con, paste0("
						SELECT o_condition.* 
						FROM o_condition, o_concept_condition 
						WHERE
						o_concept_condition.name = 'icd10' AND o_concept_condition.uid = o_condition.concept_id AND
						o_concept_condition.path LIKE 'smr01-diag1%' AND o_concept_condition.path NOT LIKE '%causeofdeath%' AND
						(", paste("o_condition.condition_code LIKE ", paste(paste("'", codelist_incident_stroke, "%'", sep = ""), collapse = " OR o_condition.condition_code LIKE ")), ")"))

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



dbDisconnect(con)


###############################################################################

# Clean and combine medcodes, ICD10 codes and OPCS4 codes, then merge with index date

# Get diabetes cohort
load("/home/pcardoso/workspace/SDRN-Cohort-scripts/Interim_Datasets/all_diabetes_cohort.RData")

index_date <- as.Date("2022-11-01", origin = "1970-01-01")

# Clean comorbidity data and combine with index
for (i in comorbids) {
	
	print(paste("merging drug dates with", i, "code occurrences"))
	
	index_date_merge_tablename <- paste0("full_", i, "_index_date_merge")
	
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
			mutate(datediff = as.numeric(difftime(date, index_date, units = "days")))
	
	rm(all_codes_clean)
	
	assign(index_date_merge_tablename, data)
	
	rm(data)
	
}


###############################################################################


# Find earliest predrug, lastest predrug and first postdrug dates
## Leave amputation and family history of diabetes for now as need to be processed differently

cmorbids <- setdiff(comorbids, c("hosp_cause_majoramputation", "hosp_cause_minoramputation", "fh_diabetes_positive", "fh_diabetes_negative"))

comorbidities <- diabetes_cohort %>%
		select(serialno)



for (i in comorbids) {
	
	print(paste("working out pre- and post-index date code occurrences for", i))
	
	index_date_merge_tablename <- paste0("full_", i, "_index_date_merge")
	interim_comorbidity_table <- paste0("comorbidities_interim_", i)
	pre_index_date_earliest_date_variable <- paste0("pre_index_date_earliest_", i)
	pre_index_date_latest_date_variable <- paste0("pre_index_date_latest_", i)
	pre_index_date_variable <- paste0("pre_index_date_", i)
	post_index_date_date_variable <- paste0("post_index_date_first_", i)
	
	if (exists(index_date_merge_tablename)) {
		
		pre_index_date <- get(index_date_merge_tablename) %>%
				filter(date<=index_date) %>%
				group_by(serialno) %>%
				summarise(
						{{pre_index_date_earliest_date_variable}}:=min(date, na.rm = TRUE),
						{{pre_index_date_latest_date_variable}}:=max(date, na.rm = TRUE)
				) %>%
				ungroup()
		
		post_index_date <- get(index_date_merge_tablename) %>%
				filter(date>index_date) %>%
				group_by(serialno) %>%
				summarise({{post_index_date_date_variable}}:=min(date, na.rm = TRUE)) %>%
				ungroup()
		
		comorbidities <- comorbidities %>%
				left_join(pre_index_date, by = c("serialno")) %>%
				left_join(post_index_date, by = c("serialno")) %>%
				mutate({{pre_index_date_variable}}:=as.numeric(!is.na(!!as.name(pre_index_date_earliest_date_variable))))
		
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




save(comorbidities, file = "/home/pcardoso/workspace/SDRN-Cohort-scripts/Interim_Datasets/prev_comorbidities.RData")

