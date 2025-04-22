# Author: pcardoso
###############################################################################

# Identify those with diabetes at prevalence, alcohol

###############################################################################

rm(list=ls())

# load libraries
library(tidyverse)



###############################################################################

## connection to database
con <- dbConn("NDS_2023")

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

dbDisconnect(con)

# Clean: remove duplicated for serialno, date, categories

clean_alcohol_medcodes <- raw_alcohol_medcodes %>%
		distinct()


###############################################################################

# Find alcohol status according to algorithm at index dates

## Get index dates

index_date <- as.Date("2022-11-01", origin = "1970-01-01")

# Get diabetes cohort
load("/home/pcardoso/workspace/SDRN-Cohort-scripts/Interim_Datasets/all_diabetes_cohort.RData")


## Join with alcohol codes on serialno and retain codes before index date or up to 7 days after

pre_index_date_alcohol_codes <- clean_alcohol_medcodes %>%
		filter(difftime(date, index_date, units = "days")<=7)

## Find if ever previously a 'harmful' drinker (category 3)
harmful_drinker_ever <- pre_index_date_alcohol_codes %>%
		filter(alcohol_cat=="AlcoholConsumptionLevel3") %>%
		distinct(serialno) %>%
		mutate(harmful_drinker_ever=1)

## Find most recent code
### If different categories on same day, use highest
most_recent_code <- pre_index_date_alcohol_codes %>%
		mutate(
				alcohol_cat_numeric = ifelse(alcohol_cat=="AlcoholConsumptionLevel0", 0,
						ifelse(alcohol_cat=="AlcoholConsumptionLevel1", 1,
								ifelse(alcohol_cat=="AlcoholConsumptionLevel2", 2,
										ifelse(alcohol_cat=="AlcoholConsumptionLevel3", 3, NA))))
		) %>%
		group_by(serialno) %>%
		filter(date==max(date, na.rm = TRUE)) %>%
		filter(alcohol_cat_numeric==max(alcohol_cat_numeric, na.rm = TRUE)) %>%
		ungroup()

# Pull together
alcohol_cat <- diabetes_cohort %>%
		select(serialno) %>%
		left_join(harmful_drinker_ever, by = "serialno") %>%
		left_join(most_recent_code, by = "serialno") %>%
		mutate(
				alcohol_cat_numeric=ifelse(!is.na(harmful_drinker_ever) & harmful_drinker_ever==1, 3, alcohol_cat_numeric),
				
				alcohol_cat=case_when(
						alcohol_cat_numeric==0 ~ "None",
						alcohol_cat_numeric==1 ~ "Within limits",
						alcohol_cat_numeric==2 ~ "Excess",
						alcohol_cat_numeric==3 ~ "Harmful"
				)
		) %>%
		select(serialno, alcohol_cat)

alcohol <- alcohol_cat

save(alcohol, file = "/home/pcardoso/workspace/SDRN-Cohort-scripts/Interim_Datasets/prev_alcohol.RData")


