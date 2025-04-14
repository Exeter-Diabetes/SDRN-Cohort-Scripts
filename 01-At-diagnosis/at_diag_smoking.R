# Author: pcardoso
###############################################################################

# Identify those with diabetes at diagnosis, smoking

###############################################################################

rm(list=ls())

# load libraries
library(tidyverse)



###############################################################################

## connection to database
con <- dbConn("NDS_2023")

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

# Find alcohol status according to algorithm at index dates

## Get index dates (diagnosis dates)

# Get diabetes cohort
load("/home/pcardoso/workspace/SDRN-Cohort-scripts/Interim_Datasets/all_diabetes_cohort.RData")

index_dates <- diabetes_cohort %>%
		filter(!is.na(dm_diag_date_all)) %>%
		select(serialno, index_date = dm_diag_date_all)

## Join with smoking codes on serialno and retain codes before index date or up to 7 days after

pre_index_date_smoking_codes <- index_dates %>%
		inner_join(clean_smoking_medcodes, by = "serialno") %>%
		filter(difftime(date, index_date, units = "days")<=7)


## Find smoking status at index date according to our algorithm

### Find if ever previously an active smoker
smoker_ever <- pre_index_date_smoking_codes %>%
		filter(smoking_cat == "Active smoker") %>%
		distinct(serialno) %>%
		mutate(smoked_ever_flag = 1)


## Find most recent code
### If both non- and ex-smoker, use ex-smoker
### If conflicting categories (non- and active- / ex- and active-), treat as missing
most_recent_code <- pre_index_date_smoking_codes %>%
		distinct(serialno, date, smoking_cat) %>%
		group_by(serialno) %>%
		filter(date == max(date, na.rm = TRUE)) %>%
		ungroup() %>%
		select(-date) %>%
		mutate(fill = TRUE) %>%
		pivot_wider(
				id_cols = c(serialno),
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
		select(serialno, most_recent_code=smoking_cat)


## Find next recorded code (to use for those with conflicting categories on most recent date)
next_most_recent_code <- pre_index_date_smoking_codes %>%
		distinct(serialno, date, smoking_cat) %>%
		group_by(serialno) %>%
		filter(date != max(date, na.rm = TRUE)) %>%
		filter(date == max(date, na.rm = TRUE)) %>%
		ungroup() %>%
		select(-date) %>%
		mutate(fill = TRUE) %>%
		pivot_wider(
				id_cols = c(serialno),
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
		select(serialno, next_most_recent_code=smoking_cat)


## Pull together
smoking_cat <- diabetes_cohort %>%
		select(serialno) %>%
		left_join(smoker_ever, by = c("serialno")) %>%
		left_join(most_recent_code, by = c("serialno")) %>%
		left_join(next_most_recent_code, by = c("serialno")) %>%
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

smoking <- diabetes_cohort %>%
		select(serialno) %>%
		left_join(smoking_cat, by = c("serialno"))


save(smoking, file = "/home/pcardoso/workspace/SDRN-Cohort-scripts/Interim_Datasets/at_diag_smoking.RData")




