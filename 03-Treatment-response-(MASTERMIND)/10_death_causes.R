# Author: pcardoso
###############################################################################

# Identify those with diabetes type 2, death causes

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

# Add in CV, HF and KF death outcomes
## Do for all IDs as quicker

comorbidity_ICD10_table <- readRDS("/home/pcardoso/workspace/SDRN-Cohort-scripts/Codelists/Comorbidities/comorbidity_ICD10_table.rds")

# Primary cause 
cv_death_primary <- dbGetQueryMap(con, paste("
				SELECT o_condition.serialno
				FROM o_condition, o_concept_condition, o_person
				WHERE 
				o_person.date_of_birth < '2022-11-01' AND o_person.dm_type = 2 AND o_person.earliest_mention IS NOT NULL AND 
				o_condition.serialno = o_person.serialno AND
				o_concept_condition.name = 'icd10' AND o_concept_condition.uid = o_condition.concept_id AND
				o_concept_condition.uid = '20003' AND
				(", paste("o_condition.condition_code LIKE ", paste(paste("'", comorbidity_ICD10_table[["cv_death"]], "%'", sep = ""), collapse = " OR o_condition.condition_code LIKE ")), ")")) %>%
	mutate(cv_death_primary_cause = 1)

hf_death_primary <- dbGetQueryMap(con, paste("
				SELECT o_condition.serialno
				FROM o_condition, o_concept_condition, o_person
				WHERE 
				o_person.date_of_birth < '2022-11-01' AND o_person.dm_type = 2 AND o_person.earliest_mention IS NOT NULL AND 
				o_condition.serialno = o_person.serialno AND
				o_concept_condition.name = 'icd10' AND o_concept_condition.uid = o_condition.concept_id AND
				o_concept_condition.uid = '20003' AND
				(", paste("o_condition.condition_code LIKE ", paste(paste("'", comorbidity_ICD10_table[["heartfailure"]], "%'", sep = ""), collapse = " OR o_condition.condition_code LIKE ")), ")")) %>%
	mutate(hf_death_primary_cause = 1)


kf_death_primary <- dbGetQueryMap(con, paste("
				SELECT o_condition.serialno
				FROM o_condition, o_concept_condition, o_person
				WHERE 
				o_person.date_of_birth < '2022-11-01' AND o_person.dm_type = 2 AND o_person.earliest_mention IS NOT NULL AND 
				o_condition.serialno = o_person.serialno AND
				o_concept_condition.name = 'icd10' AND o_concept_condition.uid = o_condition.concept_id AND
				o_concept_condition.uid = '20003' AND
				(", paste("o_condition.condition_code LIKE ", paste(paste("'", comorbidity_ICD10_table[["kf_death"]], "%'", sep = ""), collapse = " OR o_condition.condition_code LIKE ")), ")")) %>%
	mutate(kf_death_primary_cause = 1)


		
# Secondary causes
cv_death_secondary <- dbGetQueryMap(con, paste("
				SELECT o_condition.serialno
				FROM o_condition, o_concept_condition, o_person
				WHERE 
				o_person.date_of_birth < '2022-11-01' AND o_person.dm_type = 2 AND o_person.earliest_mention IS NOT NULL AND 
				o_condition.serialno = o_person.serialno AND
				o_concept_condition.name = 'icd10' AND o_concept_condition.uid = o_condition.concept_id AND
				o_concept_condition.uid IN ('20012', '20022', '20032', '20042', '20052', '20062', '20072', '20082', '20092', '20102') AND
				(", paste("o_condition.condition_code LIKE ", paste(paste("'", comorbidity_ICD10_table[["cv_death"]], "%'", sep = ""), collapse = " OR o_condition.condition_code LIKE ")), ")"))

hf_death_secondary <- dbGetQueryMap(con, paste("
				SELECT o_condition.serialno
				FROM o_condition, o_concept_condition, o_person
				WHERE 
				o_person.date_of_birth < '2022-11-01' AND o_person.dm_type = 2 AND o_person.earliest_mention IS NOT NULL AND 
				o_condition.serialno = o_person.serialno AND
				o_concept_condition.name = 'icd10' AND o_concept_condition.uid = o_condition.concept_id AND
				o_concept_condition.uid IN ('20012', '20022', '20032', '20042', '20052', '20062', '20072', '20082', '20092', '20102') AND
				(", paste("o_condition.condition_code LIKE ", paste(paste("'", comorbidity_ICD10_table[["heartfailure"]], "%'", sep = ""), collapse = " OR o_condition.condition_code LIKE ")), ")"))

kf_death_secondary <- dbGetQueryMap(con, paste("
				SELECT o_condition.serialno
				FROM o_condition, o_concept_condition, o_person
				WHERE 
				o_person.date_of_birth < '2022-11-01' AND o_person.dm_type = 2 AND o_person.earliest_mention IS NOT NULL AND 
				o_condition.serialno = o_person.serialno AND
				o_concept_condition.name = 'icd10' AND o_concept_condition.uid = o_condition.concept_id AND
				o_concept_condition.uid IN ('20012', '20022', '20032', '20042', '20052', '20062', '20072', '20082', '20092', '20102') AND
				(", paste("o_condition.condition_code LIKE ", paste(paste("'", comorbidity_ICD10_table[["kf_death"]], "%'", sep = ""), collapse = " OR o_condition.condition_code LIKE ")), ")"))


# Join primary  and secondary for any cause

cv_death_any <- dbGetQueryMap(con, paste("
								SELECT o_condition.serialno
								FROM o_condition, o_concept_condition, o_person
								WHERE 
								o_person.date_of_birth < '2022-11-01' AND o_person.dm_type = 2 AND o_person.earliest_mention IS NOT NULL AND 
								o_condition.serialno = o_person.serialno AND
								o_concept_condition.name = 'icd10' AND o_concept_condition.uid = o_condition.concept_id AND
								o_concept_condition.uid IN ('20003', '20012', '20022', '20032', '20042', '20052', '20062', '20072', '20082', '20092', '20102') AND
								(", paste("o_condition.condition_code LIKE ", paste(paste("'", comorbidity_ICD10_table[["cv_death"]], "%'", sep = ""), collapse = " OR o_condition.condition_code LIKE ")), ")")) %>%
		mutate(cv_death_any_cause = 1) %>%
		distinct()

hf_death_any <- dbGetQueryMap(con, paste("
								SELECT o_condition.serialno
								FROM o_condition, o_concept_condition, o_person
								WHERE 
								o_person.date_of_birth < '2022-11-01' AND o_person.dm_type = 2 AND o_person.earliest_mention IS NOT NULL AND 
								o_condition.serialno = o_person.serialno AND
								o_concept_condition.name = 'icd10' AND o_concept_condition.uid = o_condition.concept_id AND
								o_concept_condition.uid IN ('20003', '20012', '20022', '20032', '20042', '20052', '20062', '20072', '20082', '20092', '20102') AND
								(", paste("o_condition.condition_code LIKE ", paste(paste("'", comorbidity_ICD10_table[["heartfailure"]], "%'", sep = ""), collapse = " OR o_condition.condition_code LIKE ")), ")")) %>%
		mutate(hf_death_any_cause = 1) %>%
		distinct()


kf_death_any <- dbGetQueryMap(con, paste("
								SELECT o_condition.serialno
								FROM o_condition, o_concept_condition, o_person
								WHERE 
								o_person.date_of_birth < '2022-11-01' AND o_person.dm_type = 2 AND o_person.earliest_mention IS NOT NULL AND 
								o_condition.serialno = o_person.serialno AND
								o_concept_condition.name = 'icd10' AND o_concept_condition.uid = o_condition.concept_id AND
								o_concept_condition.uid IN ('20003', '20012', '20022', '20032', '20042', '20052', '20062', '20072', '20082', '20092', '20102') AND
								(", paste("o_condition.condition_code LIKE ", paste(paste("'", comorbidity_ICD10_table[["kf_death"]], "%'", sep = ""), collapse = " OR o_condition.condition_code LIKE ")), ")")) %>%
		mutate(kf_death_any_cause = 1) %>%
		distinct()


## Join together and with all primary and secondary cause
death_causes <- dbGetQueryMap(con, "
								SELECT o_condition.serialno, o_condition.condition_code
								FROM o_condition, o_concept_condition, o_person
								WHERE 
								o_person.date_of_birth < '2022-11-01' AND o_person.dm_type = 2 AND o_person.earliest_mention IS NOT NULL AND 
								o_condition.serialno = o_person.serialno AND
								o_concept_condition.name = 'icd10' AND o_concept_condition.uid = o_condition.concept_id AND
								o_concept_condition.uid = '20003'") %>%
			rename(primary_death_cause1 = condition.code) %>%
			left_join(
					dbGetQueryMap(con, "
									SELECT o_condition.serialno, o_condition.condition_code
									FROM o_condition, o_concept_condition, o_person
									WHERE 
									o_person.date_of_birth < '2022-11-01' AND o_person.dm_type = 2 AND o_person.earliest_mention IS NOT NULL AND 
									o_condition.serialno = o_person.serialno AND
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
	
save(death_causes, file = "/home/pcardoso/workspace/SDRN-Cohort-scripts/Interim_Datasets/mm_death_causes.RData")
	


