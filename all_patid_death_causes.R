# Author: pcardoso
###############################################################################

# Collect death causes

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
con <- dbConn("NDS_2024")

###############################################################################

# Add in CV, HF and KF death outcomes
## Do for all IDs as quicker
# Primary cause 

codelist_cv_death <- clean_codes(read.delim("/home/pcardoso/workspace/SDRN-Cohort-scripts/Codelists/ICD10/exeter_icd10_cv_death.txt") %>%
				select(-contains("description")) %>%
				unlist())

cv_death_primary <- dbGetQueryMap(con, paste("
								SELECT o_condition.serialno
								FROM o_condition, o_concept_condition
								WHERE 
								o_concept_condition.name = 'icd10' AND o_concept_condition.uid = o_condition.concept_id AND
								o_concept_condition.uid = '20003' AND
								(", paste("o_condition.condition_code LIKE ", paste(paste("'", codelist_cv_death, "%'", sep = ""), collapse = " OR o_condition.condition_code LIKE ")), ")")) %>%
		mutate(cv_death_primary_cause = 1)

codelist_heartfailure <- clean_codes(read.delim("/home/pcardoso/workspace/SDRN-Cohort-scripts/Codelists/ICD10/exeter_icd10_heartfailure.txt") %>%
				select(-contains("description")) %>%
				unlist())

hf_death_primary <- dbGetQueryMap(con, paste("
								SELECT o_condition.serialno
								FROM o_condition, o_concept_condition
								WHERE 
								o_concept_condition.name = 'icd10' AND o_concept_condition.uid = o_condition.concept_id AND
								o_concept_condition.uid = '20003' AND
								(", paste("o_condition.condition_code LIKE ", paste(paste("'", codelist_heartfailure, "%'", sep = ""), collapse = " OR o_condition.condition_code LIKE ")), ")")) %>%
		mutate(hf_death_primary_cause = 1)


codelist_kf_death <- clean_codes(read.delim("/home/pcardoso/workspace/SDRN-Cohort-scripts/Codelists/ICD10/exeter_icd10_kf_death.txt") %>%
				select(-contains("description")) %>%
				unlist())

kf_death_primary <- dbGetQueryMap(con, paste("
								SELECT o_condition.serialno
								FROM o_condition, o_concept_condition
								WHERE 
								o_concept_condition.name = 'icd10' AND o_concept_condition.uid = o_condition.concept_id AND
								o_concept_condition.uid = '20003' AND
								(", paste("o_condition.condition_code LIKE ", paste(paste("'", codelist_kf_death, "%'", sep = ""), collapse = " OR o_condition.condition_code LIKE ")), ")")) %>%
		mutate(kf_death_primary_cause = 1)



# Secondary causes
cv_death_secondary <- dbGetQueryMap(con, paste("
						SELECT o_condition.serialno
						FROM o_condition, o_concept_condition
						WHERE 
						o_concept_condition.name = 'icd10' AND o_concept_condition.uid = o_condition.concept_id AND
						o_concept_condition.uid IN ('20012', '20022', '20032', '20042', '20052', '20062', '20072', '20082', '20092', '20102') AND
						(", paste("o_condition.condition_code LIKE ", paste(paste("'", codelist_cv_death, "%'", sep = ""), collapse = " OR o_condition.condition_code LIKE ")), ")"))

hf_death_secondary <- dbGetQueryMap(con, paste("
						SELECT o_condition.serialno
						FROM o_condition, o_concept_condition
						WHERE 
						o_concept_condition.name = 'icd10' AND o_concept_condition.uid = o_condition.concept_id AND
						o_concept_condition.uid IN ('20012', '20022', '20032', '20042', '20052', '20062', '20072', '20082', '20092', '20102') AND
						(", paste("o_condition.condition_code LIKE ", paste(paste("'", codelist_heartfailure, "%'", sep = ""), collapse = " OR o_condition.condition_code LIKE ")), ")"))

kf_death_secondary <- dbGetQueryMap(con, paste("
						SELECT o_condition.serialno
						FROM o_condition, o_concept_condition
						WHERE 
						o_concept_condition.name = 'icd10' AND o_concept_condition.uid = o_condition.concept_id AND
						o_concept_condition.uid IN ('20012', '20022', '20032', '20042', '20052', '20062', '20072', '20082', '20092', '20102') AND
						(", paste("o_condition.condition_code LIKE ", paste(paste("'", codelist_kf_death, "%'", sep = ""), collapse = " OR o_condition.condition_code LIKE ")), ")"))


# Join primary  and secondary for any cause

cv_death_any <- dbGetQueryMap(con, paste("
								SELECT o_condition.serialno
								FROM o_condition, o_concept_condition
								WHERE 
								o_concept_condition.name = 'icd10' AND o_concept_condition.uid = o_condition.concept_id AND
								o_concept_condition.uid IN ('20003', '20012', '20022', '20032', '20042', '20052', '20062', '20072', '20082', '20092', '20102') AND
								(", paste("o_condition.condition_code LIKE ", paste(paste("'", codelist_cv_death, "%'", sep = ""), collapse = " OR o_condition.condition_code LIKE ")), ")")) %>%
		mutate(cv_death_any_cause = 1) %>%
		distinct()

hf_death_any <- dbGetQueryMap(con, paste("
								SELECT o_condition.serialno
								FROM o_condition, o_concept_condition
								WHERE 
								o_concept_condition.name = 'icd10' AND o_concept_condition.uid = o_condition.concept_id AND
								o_concept_condition.uid IN ('20003', '20012', '20022', '20032', '20042', '20052', '20062', '20072', '20082', '20092', '20102') AND
								(", paste("o_condition.condition_code LIKE ", paste(paste("'", codelist_heartfailure, "%'", sep = ""), collapse = " OR o_condition.condition_code LIKE ")), ")")) %>%
		mutate(hf_death_any_cause = 1) %>%
		distinct()


kf_death_any <- dbGetQueryMap(con, paste("
								SELECT o_condition.serialno
								FROM o_condition, o_concept_condition
								WHERE 
								o_concept_condition.name = 'icd10' AND o_concept_condition.uid = o_condition.concept_id AND
								o_concept_condition.uid IN ('20003', '20012', '20022', '20032', '20042', '20052', '20062', '20072', '20082', '20092', '20102') AND
								(", paste("o_condition.condition_code LIKE ", paste(paste("'", codelist_kf_death, "%'", sep = ""), collapse = " OR o_condition.condition_code LIKE ")), ")")) %>%
		mutate(kf_death_any_cause = 1) %>%
		distinct()


## Join together and with all primary and secondary cause
death_causes <- dbGetQueryMap(con, "
						SELECT o_condition.serialno, o_condition.condition_code
						FROM o_condition, o_concept_condition
						WHERE 
						o_concept_condition.name = 'icd10' AND o_concept_condition.uid = o_condition.concept_id AND
						o_concept_condition.uid = '20003'") %>%
		rename(primary_death_cause1 = condition.code) %>%
		left_join(
				dbGetQueryMap(con, "
										SELECT o_condition.serialno, o_condition.condition_code
										FROM o_condition, o_concept_condition
										WHERE 
										o_concept_condition.name = 'icd10' AND o_concept_condition.uid = o_condition.concept_id AND
										o_concept_condition.uid IN ('20012', '20022', '20032', '20042', '20052', '20062', '20072', '20082', '20092', '20102')") %>%
						group_by(serialno) %>%
						mutate(
								row_n = row_number(),
								row_name = paste0("secondary_death_cause", row_n)
						) %>%
						ungroup() %>%
						select(-row_n) %>%
						pivot_wider(id_cols = serialno, names_from = row_name, values_from = condition.code, values_fill = NA), by = c("serialno")
		) %>%
		left_join(cv_death_primary, by = c("serialno")) %>%
		left_join(cv_death_any, by = c("serialno")) %>%
		left_join(hf_death_primary, by = c("serialno")) %>%
		left_join(hf_death_any, by = c("serialno")) %>%
		left_join(kf_death_primary, by = c("serialno")) %>%
		left_join(kf_death_any, by = c("serialno"))


## disconnect from database
dbDisconnect(con)



save(death_causes, file = "/home/pcardoso/workspace/SDRN-Cohort-scripts/Interim_Datasets/all_patid_death_causes.RData")



