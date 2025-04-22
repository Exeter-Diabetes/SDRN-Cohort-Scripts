# Author: pcardoso
###############################################################################

# Collect alcohol status for combos

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
#test_concept_alcohol <- test_concept %>%
#		filter(grepl("lifestyle", path))
#
#
## o_concept_observation UID 1401
#test_observation_alcohol <- dbGetQueryMap(con, "SELECT * FROM o_observation WHERE concept_id = '1406'")
#
## how many smoking codes and how many unique codes
#test_observation_alcohol %>%
#		group_by(serialno) %>%
#		summarise(sum = length(str.value), unique = length(unique(str.value))) %>%
#		select(sum, unique) %>%
#		table()



###############################################################################

# Pull out all raw code instances for smoking
#[1] "LRA-CADS-00" "LRA-CADS-01" "LRA-CADS-02" "LRA-CADS-03" "LRA-CADS-04"
#[6] "LRA-CADS-05" "LRA-CADS-06" "LRA-CADS-07" "LRA-CADS-08" "LRA-CADS-97"
#[11] "LRA-CADS-99"

alcohol_cat_table <- data.frame(
		str.value = c("LRA-CADS-00", "LRA-CADS-01", "LRA-CADS-02", "LRA-CADS-03", "LRA-CADS-04",
				"LRA-CADS-05", "LRA-CADS-06", "LRA-CADS-07", "LRA-CADS-08", "LRA-CADS-97",
				"LRA-CADS-99"),
		alcohol_cat = c("AlcoholConsumptionLevel0", "AlcoholConsumptionLevel0", "AlcoholConsumptionLevel0", "AlcoholConsumptionLevel1", "AlcoholConsumptionLevel2",
				"AlcoholConsumptionLevel2", "AlcoholConsumptionLevel3", "AlcoholConsumptionLevel1", "AlcoholConsumptionLevel0", "AlcoholConsumptionLevel0",
				"AlcoholConsumptionLevel0")
)


raw_alcohol_medcodes <- dbGetQueryMap(con, "
						SELECT o_observation.serialno, o_observation.date, o_observation.str_value
						FROM o_concept_observation, o_observation
						WHERE 
						o_observation.concept_id = o_concept_observation.uid AND
						o_concept_observation.path = 'lifestyle-alcohol_status'") %>%
		left_join(
				alcohol_cat_table, by = c("str.value")
		) %>%
		select(-str.value)




## disconnect from database
dbDisconnect(con)



# Clean: remove duplicated for serialno, date, categories
clean_alcohol_medcodes <- raw_alcohol_medcodes %>%
		distinct()


###############################################################################

# Find alcohol status according to algorithm at drug start dates

# Get drug start dates (1 row per drug period)
load("/home/pcardoso/workspace/SDRN-Cohort-scripts/Interim_Datasets/mm_drug_start_stop.RData")

# Join with alcohol codes on serialno and retain codes before drug start date or up to 7 days after
predrug_alcohol_codes <- drug_start_stop %>%
		select(serialno, dstartdate, drug_class, drug_substance, drug_instance) %>%
		inner_join(clean_alcohol_medcodes, by = c("serialno")) %>%
		filter(difftime(date, dstartdate, units = "days")<=7)

## Find if ever previously a 'harmful' drinker (category 3)
harmful_drinker_ever <- predrug_alcohol_codes %>%
		filter(alcohol_cat=="AlcoholConsumptionLevel3") %>%
		distinct(serialno, dstartdate, drug_substance) %>%
		mutate(harmful_drinker_ever=1)

## Find most recent code
### If different categories on same day, use highest
most_recent_code <- predrug_alcohol_codes %>%
		distinct(serialno, dstartdate, drug_substance, date, alcohol_cat) %>%
		mutate(
				alcohol_cat_numeric = ifelse(alcohol_cat=="AlcoholConsumptionLevel0", 0,
						ifelse(alcohol_cat=="AlcoholConsumptionLevel1", 1,
								ifelse(alcohol_cat=="AlcoholConsumptionLevel2", 2,
										ifelse(alcohol_cat=="AlcoholConsumptionLevel3", 3, NA))))
		) %>%
		group_by(serialno, dstartdate, drug_substance) %>%
		filter(date==max(date, na.rm = TRUE)) %>%
		filter(alcohol_cat_numeric==max(alcohol_cat_numeric, na.rm = TRUE)) %>%
		ungroup() %>%
		select(-date)

## Pull together
alcohol <- drug_start_stop %>%
		distinct(serialno, dstartdate, drug_class, drug_substance, drug_instance) %>%
		left_join(harmful_drinker_ever, by = c("serialno", "dstartdate", "drug_substance")) %>%
		left_join(most_recent_code, by = c("serialno", "dstartdate", "drug_substance")) %>%
		mutate(
				alcohol_cat_numeric = ifelse(!is.na(harmful_drinker_ever) & harmful_drinker_ever==1, 3, alcohol_cat_numeric),
				
				alcohol_cat = case_when(
						alcohol_cat_numeric==0 ~ "None",
						alcohol_cat_numeric==1 ~ "Within limits",
						alcohol_cat_numeric==2 ~ "Excess",
						alcohol_cat_numeric==3 ~ "Harmful"
				)
		) %>%
		select(-c(alcohol_cat_numeric, harmful_drinker_ever))


save(alcohol, file = "/home/pcardoso/workspace/SDRN-Cohort-scripts/Interim_Datasets/mm_alcohol.RData")



