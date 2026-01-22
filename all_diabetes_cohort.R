# Author: pcardoso
###############################################################################

# Identify those with diabetes

###############################################################################

rm(list=ls())

# load libraries
library(tidyverse)

###############################################################################

## connection to database
con <- dbConn("NDS_2024")

# currently only including type 1 and type 2

diabetes_cohort_ids <- dbGetQueryMap(con, "
				SELECT *
				FROM o_person 
				WHERE 
				date_of_birth < '2022-11-01'AND earliest_mention IS NOT NULL AND
				earliest_mention >= date_of_birth AND
				dm_type IN ('1', '2')")

# Diabetes cohort

diabetes_cohort <- dbGetQueryMap(con, "
						SELECT o_person.*, s_demography_summary.simd_2016_decile
						FROM o_person, s_demography_summary
						WHERE 
						o_person.date_of_birth < '2022-11-01'AND o_person.earliest_mention IS NOT NULL AND
						o_person.earliest_mention >= o_person.date_of_birth AND
						o_person.serialno = s_demography_summary.serialno AND
						o_person.dm_type IN ('1', '2')") %>%
		# Diagnosis dates	
		rename("dm_diag_date_all" = "earliest.mention") %>%
		mutate(dm_diag_date = dm_diag_date_all) %>%
		# Date of birth
		rename("dob" = "date.of.birth") %>%
		# Calculate diagnosis age from dob and diagnosis date
		mutate(
				dm_diag_age_all = as.numeric((difftime(dm_diag_date_all, dob, units = "days"))/365.25),
				dm_diag_age = dm_diag_age_all # the same since there are no restrictions
		) %>%
		# Diabetes type (1 or 2)
		mutate(
				diabetes_type = ifelse(dm.type == 1, "type 1", "type 2")
		)

dbDisconnect(con)

## Define ethnicity
# SDRN ethnicity categories:
#	ethnic															eth5						QRISK2
#	1A Scottish														0. White					1. White
#	1E Irish														0. White					1. White
#	1F Welsh														0. White					1. White
#	1G Northen Irish												0. White					1. White
#	1H British														0. White					1. White
#	1J Irish														0. White					1. White
#	1K Gypsy/Traveller												0. White					1. White
#	1L Polish														0. White					1. White
#	1Z Any other white ethnic group									0. White					1. White
#	2A Any mixed or multiple ethnic groups							4. Mixed					9. Other
#	3F Pakistani, Pakistani Scottish or Pakistani British			1. South Asian				3. Pakistani
#	3G Indian, Indian Scottish or Pakistani Bristish				1. South Asian				2. Indian
#	3H Bangladeshi, Bangladeshi Scottish or Bangladeshi British		1. South Asian				4. Bangladeshi
#	3J Chinese, Chinese Scottish or Chinese British					3. Other					8. Chinese
#	3Z Other - Asian, Asian Scottish or Asian British				1. South Asian				9. Other
#	4D African, African Scottish or Asian British					2. Black					7. Black African
#	4E Caribbean, Caribbean Scottish or Caribbean British			2. Black					6. Black Caribbean
#	4F Black, Black Scottish or Black British						2. Black					9. Other
#	4Z Other - African, Caribbean or Black							2. Black					9. Other
#	5B Arab															3. Other					9. Other
#	5Z Other - Other ethnic group									3. Other					9. Other
#	98 Refused														5. Not stated/Unknown		0. Unknown
#	99 Not Known													5. Not stated/Unknown		0. Unknown

ethnicity_explanation <- data.frame(
		ethnic = c("1A", "1E", "1F", "1G", "1H", "1J", "1K", "1L", "1Z", "2A", "3F", "3G", "3H", "3J", "3Z", "4D", "4E", "4F", "4Z", "5B", "5Z", "98", "99"),
		eth5 = c("White", "White", "White", "White", "White", "White", "White", "White", "White", "Mixed", "South Asian", "South Asian", "South Asian", "Other", "South Asian", "Black", "Black", "Black", "Black", "Other", "Other", "Not stated/Unknown", "Not stated/Unknown"),
		qrisk2 = c(1, 1, 1, 1, 1, 1, 1, 1, 1, 9, 3, 2, 4, 8, 9, 7, 6, 9, 9, 9, 9, 0, 0)
)

diabetes_cohort <- diabetes_cohort %>%
		left_join(
				ethnicity_explanation, by = c("ethnic")
		) %>%
		rename("ethnicity_5cat" = "eth5", "ethnicity_qrisk2" = "qrisk2") %>%
		mutate(ethnicity_qrisk2 = ifelse(is.na(ethnicity_qrisk2), 0, ethnicity_qrisk2))


## Define deprivation (SIMD)
diabetes_cohort <- diabetes_cohort %>%
		rename("imd_decile" = "simd.2016.decile") %>%
		mutate(
				simd_decile = imd_decile,
				imd_decile = 11 - imd_decile
		)


# Final dataset
diabetes_cohort <- diabetes_cohort %>%
		rename("death_date" = "date.of.death") %>%
		select(serialno, gender, dob, death_date, diabetes_type, ethnicity_5cat, ethnicity_qrisk2, simd_decile, imd_decile, dm_diag_date_all, dm_diag_age_all, dm_diag_date, dm_diag_age)

save(diabetes_cohort, file = "/home/pcardoso/workspace/SDRN-Cohort-scripts/Interim_Datasets/all_diabetes_cohort.RData")

