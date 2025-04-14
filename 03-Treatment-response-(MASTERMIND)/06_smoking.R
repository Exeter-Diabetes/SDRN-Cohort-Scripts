# Author: pcardoso
###############################################################################

# Collect smoking status for combos

###############################################################################

rm(list=ls())

# load libraries
library(tidyverse)


###############################################################################

## connection to database
con <- dbConn("NDS_2023")


###############################################################################


#### Table of conditions concept list
#test_concept <- dbGetQueryMap(con, "SELECT * FROM o_concept_observation")
#test_concept_smoking <- test_concept %>%
#		filter(grepl("lifestyle", path))
#
## how many smoking codes and how many unique codes
#test_observation_smoker %>%
#		group_by(serialno) %>%
#		summarise(sum = length(str.value), unique = length(unique(str.value))) %>%
#		select(sum, unique) %>%
#		table()
#
#
## o_concept_observation UID 1401
#test_observation_smoker <- dbGetQueryMap(con, "SELECT * FROM o_observation WHERE concept_id = '1401'")
#test_observation_stopsmok <- dbGetQueryMap(con, "SELECT * FROM o_observation WHERE concept_id = '1404'")



###############################################################################

# Pull out all raw code instances for smoking
#[1] "LRA-CTN-10" "LRA-CTN-11" "LRA-CTN-12" "LRA-CTN-13" "LRA-CTN-23"
#[6] "LRA-CTN-97" "LRA-CTN-99"

smoking_cat_table <- data.frame(
		str.value = c("LRA-CTN-10", "LRA-CTN-11", "LRA-CTN-12", "LRA-CTN-13", "LRA-CTN-23", "LRA-CTN-97","LRA-CTN-99"),
		smoking_cat = c("Non-smoker", "Ex-smoker", "Non-smoker", "Active smoker", "Active smoker", "Non-smoker", "Non-smoker")
	)


raw_smoking_medcodes <- dbGetQueryMap(con, "
				SELECT o_observation.serialno, o_observation.date, o_observation.str_value
				FROM o_concept_observation, o_observation
				WHERE 
					o_observation.concept_id = o_concept_observation.uid AND
					o_concept_observation.path = 'lifestyle-smoker'") %>%
	left_join(
			smoking_cat_table, by = c("str.value")
	) %>%
	select(-str.value)



## disconnect from database
dbDisconnect(con)


# Clean: remove duplicated for serialno, date, categories

clean_smoking_medcodes <- raw_smoking_medcodes %>%
	distinct()



###############################################################################

# Find smoking status according to both algorithms at drug start dates

# Get drug start dates (1 row per drug period)
load("/home/pcardoso/workspace/SDRN-Cohort-scripts/Interim_Datasets/mm_drug_start_stop.RData")

# Join with smoking codes on serialno and retain codes before drug start date or up to 7 days after
predrug_smoking_codes <- drug_start_stop %>%
		select(serialno, dstartdate, drug_class, drug_substance, drug_instance) %>%
		inner_join(clean_smoking_medcodes, by = c("serialno")) %>%
		filter(difftime(date, dstartdate, units = "days")<= 7)

# Find smoking status at drug start date according to our algorithm

## Find if ever previously an active smoker
smoker_ever <- predrug_smoking_codes %>%
		filter(smoking_cat == "Active smoker") %>%
		distinct(serialno, dstartdate, drug_substance) %>%
		mutate(smoked_ever_flag = 1)

## Find most recent code
### If both non- and ex-smoker, use ex-smoker
### If conflicting categories (non- and active- / ex- and active-), treat as missing
most_recent_code <- predrug_smoking_codes %>%
		distinct(serialno, dstartdate, drug_substance, date, smoking_cat) %>%
		group_by(serialno, dstartdate, drug_substance) %>%
		filter(date == max(date, na.rm = TRUE)) %>%
		ungroup() %>%
		select(-date) %>%
		mutate(fill = TRUE) %>%
		pivot_wider(
			id_cols = c(serialno, dstartdate, drug_substance),
			names_from = smoking_cat,
			values_from = fill,
			values_fill = list(fill=FALSE)
		) %>%
		mutate(
			`Active smoker` = as.numeric(`Active smoker`),
			`Ex-smoker` = as.numeric(`Ex-smoker`),
			`Non-smoker` = as.numeric(`Non-smoker`),
			smoking_cat = ifelse(`Active smoker`==1 & `Non-smoker`==0 & `Ex-smoker`==0, "Active smoker",
					ifelse(`Active smoker`==0 & `Ex-smoker`==1, "Ex-smoker",
							ifelse(`Active smoker`==0 & `Ex-smoker`==0 & `Non-smoker`==1, "Non-smoker", NA)))
		) %>%
		select(serialno, dstartdate, drug_substance, most_recent_code=smoking_cat)

## Find next recorded code (to use for those with conflicting categories on most recent date)
next_most_recent_code <- predrug_smoking_codes %>%
		distinct(serialno, dstartdate, drug_substance, date, smoking_cat) %>%
		group_by(serialno, dstartdate, drug_substance) %>%
		filter(date != max(date, na.rm = TRUE)) %>%
		filter(date == max(date, na.rm = TRUE)) %>%
		ungroup() %>%
		select(-date) %>%
		mutate(fill = TRUE) %>%
		pivot_wider(
				id_cols = c(serialno, dstartdate, drug_substance),
				names_from = smoking_cat,
				values_from = fill,
				values_fill = list(fill=FALSE)
		) %>%
		mutate(
				`Active smoker` = as.numeric(`Active smoker`),
				`Ex-smoker` = as.numeric(`Ex-smoker`),
				`Non-smoker` = as.numeric(`Non-smoker`),
				smoking_cat = ifelse(`Active smoker`==1 & `Non-smoker`==0 & `Ex-smoker`==0, "Active smoker",
						ifelse(`Active smoker`==0 & `Ex-smoker`==1, "Ex-smoker",
								ifelse(`Active smoker`==0 & `Ex-smoker`==0 & `Non-smoker`==1, "Non-smoker", NA)))
		) %>%
		select(serialno, dstartdate, drug_substance, next_most_recent_code=smoking_cat)
		
## Pull together
smoking_cat <- drug_start_stop %>%
		select(serialno, dstartdate, drug_substance) %>%
		left_join(smoker_ever, by = c("serialno", "dstartdate", "drug_substance")) %>%
		left_join(most_recent_code, by = c("serialno", "dstartdate", "drug_substance")) %>%
		left_join(next_most_recent_code, by = c("serialno", "dstartdate", "drug_substance")) %>%
		mutate(
			most_recent_code = coalesce(most_recent_code, next_most_recent_code),
			smoking_cat = ifelse(most_recent_code=="Non-smoker" & !is.na(smoked_ever_flag) & smoked_ever_flag==1, "Ex-smoker", most_recent_code)
		) %>%
		select(-c(most_recent_code, next_most_recent_code, smoked_ever_flag))


# Work out smoking status from QRISK2 algorithm

#
# Not coded
#
#



###############################################################################

# Join results of our algorithm

smoking <- drug_start_stop %>%
		select(serialno, dstartdate, drug_class, drug_substance, drug_instance) %>%
		left_join(smoking_cat, by = c("serialno", "dstartdate", "drug_substance"))


save(smoking, file = "/home/pcardoso/workspace/SDRN-Cohort-scripts/Interim_Datasets/mm_smoking.RData")





