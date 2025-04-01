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

# Adds 3 month, 6 month and 12 month discontinuation variables for all drug class periods

# Bring together drug start/stop dates (from drug_start_Stop), nextremdrug (from combo_start_stop) and time to last prescription (timetolastpx) from all_scripts table
## Too slow if join all_scripts on both date and serialno - join with serialno only and then remove rows where dstartdate!=date

load("/home/pcardoso/workspace/SDRN-Cohort-scripts/Interim_Datasets/mm_drug_class_start_stop.RData")
load("/home/pcardoso/workspace/SDRN-Cohort-scripts/Interim_Datasets/mm_combo_class_start_stop.RData")
load("/home/pcardoso/workspace/SDRN-Cohort-scripts/Interim_Datasets/mm_all_scripts.RData")


# Create time on drug variable
drug_class_start_stop <- drug_class_start_stop %>%
		mutate(timeondrug = difftime(dstopdate_class, dstartdate_class, units = "days"))


discontinuation <- drug_class_start_stop %>%
		select(serialno, dstartdate_class, drugline_all, dstopdate_class, drug_class, drug_instance, timeondrug) %>%
		inner_join((combo_class_start_stop %>% select(serialno, dcstartdate, nextremdrug)), by = c("serialno", c("dstartdate_class" = "dcstartdate"))) %>%
		inner_join((all_scripts %>% select(serialno, date, timetolastpx)), by = c("serialno"))

discontinuation <- discontinuation %>%
		filter(dstartdate_class == date) %>%
		select(-date)


###############################################################################

# Add binary variables for whether drug stopped within 3/6/12 months
discontinuation <- discontinuation %>%
		mutate(
			ttc3m = timeondrug<=91,
			ttc3m = as.numeric(ttc3m),
			ttc6m = timeondrug<=183,
			ttc6m = as.numeric(ttc6m),
			ttc12m = timeondrug<=365,
			ttc12m = as.numeric(ttc12m)
		)


# Make variables for whether discontinue or not
## e.g. stopdrug3m_6mFU:

### 1 if they stop drug within 3 month (ttc3m==1)
### # This includes: another diabetes med is stopped or started before the current drug is discontinued: nextremdrug==current drug
### # This includes: instances where discontinuation represents a break in the current drug before restarting: nextremdrug==NA
### 0 if they don't stop drug within 3 months (ttc3m==0)
### Missing (NA) if they stop drug within 3 month (ttc3m==1) BUT there is <= 6 months follow-up (FU) post-discontinuation to confirm discontinuation

discontinuation <- discontinuation %>%
		
		mutate(
			stopdrug_3m_3mFU = ifelse(ttc3m==0, 0,
					ifelse(ttc3m==1 & (timetolastpx-timeondrug)>91, 1, NA)),
			stopdrug_3m_6mFU = ifelse(ttc3m==0, 0,
					ifelse(ttc3m==1 & (timetolastpx-timeondrug)>183, 1, NA)),
			
			stopdrug_6m_3mFU = ifelse(ttc6m==0, 0,
					ifelse(ttc6m==1 & (timetolastpx-timeondrug)>91, 1, NA)),
			stopdrug_6m_6mFU = ifelse(ttc6m==0, 0,
					ifelse(ttc6m==1 & (timetolastpx-timeondrug)>183, 1, NA)),
			
			stopdrug_12m_3mFU = ifelse(ttc12m==0, 0,
					ifelse(ttc12m==1 & (timetolastpx-timeondrug)>91, 1, NA)),
			stopdrug_12m_6mFU = ifelse(ttc12m==0, 0,
					ifelse(ttc12m==1 & (timetolastpx-timeondrug)>183, 1, NA))
		)


###############################################################################

# Add in discontinuation history for each variable:
# For other drugs besides MFN: 1 if ever discontinued MFN, 0 if never discontinued MFN, NA if all discontinuation on MFN missing / never took MFN
# For MFN: NA for all
# Only include MFN periods with dstopdate prior to current dstartdate

discontinuation <- discontinuation %>%
		mutate(
			mfn_date = ifelse(drug_class == "MFN", dstopdate_class, dstartdate_class),
			mfn_date = as.Date(mfn_date, origin = "1970-01-01"),
			stopdrug_3m_3mFU_MFN = ifelse(is.na(stopdrug_3m_3mFU) | drug_class!="MFN", NA,
					ifelse(drug_class=="MFN" & stopdrug_3m_3mFU==0, 0, 1)),
			stopdrug_3m_6mFU_MFN = ifelse(is.na(stopdrug_3m_6mFU) | drug_class!="MFN", NA,
					ifelse(drug_class=="MFN" & stopdrug_3m_6mFU==0, 0, 1)),
			stopdrug_6m_3mFU_MFN = ifelse(is.na(stopdrug_6m_3mFU) | drug_class!="MFN", NA,
					ifelse(drug_class=="MFN" & stopdrug_6m_3mFU==0, 0, 1)),
			stopdrug_6m_6mFU_MFN = ifelse(is.na(stopdrug_6m_6mFU) | drug_class!="MFN", NA,
					ifelse(drug_class=="MFN" & stopdrug_6m_6mFU==0, 0, 1)),
			stopdrug_12m_3mFU_MFN = ifelse(is.na(stopdrug_12m_3mFU) | drug_class!="MFN", NA,
					ifelse(drug_class=="MFN" & stopdrug_12m_3mFU==0, 0, 1)),
			stopdrug_12m_6mFU_MFN = ifelse(is.na(stopdrug_12m_6mFU) | drug_class!="MFN", NA,
					ifelse(drug_class=="MFN" & stopdrug_12m_6mFU==0, 0, 1))
		) %>%
		group_by(serialno) %>%
		arrange(mfn_date) %>%
		mutate(
				stopdrug_3m_3mFU_MFN_hist = ifelse(drug_class=="MFN", NA, cumsum(stopdrug_3m_3mFU_MFN)),
				stopdrug_3m_6mFU_MFN_hist = ifelse(drug_class=="MFN", NA, cumsum(stopdrug_3m_6mFU_MFN)),
				stopdrug_6m_3mFU_MFN_hist = ifelse(drug_class=="MFN", NA, cumsum(stopdrug_6m_3mFU_MFN)),
				stopdrug_6m_6mFU_MFN_hist = ifelse(drug_class=="MFN", NA, cumsum(stopdrug_6m_6mFU_MFN)),
				stopdrug_12m_3mFU_MFN_hist = ifelse(drug_class=="MFN", NA, cumsum(stopdrug_12m_3mFU_MFN)),
				stopdrug_12m_6mFU_MFN_hist = ifelse(drug_class=="MFN", NA, cumsum(stopdrug_12m_6mFU_MFN)),
		) %>%
		ungroup() %>%
		select(-c(stopdrug_3m_3mFU_MFN, stopdrug_3m_6mFU_MFN, stopdrug_6m_3mFU_MFN, stopdrug_6m_6mFU_MFN, stopdrug_12m_3mFU_MFN, stopdrug_12m_6mFU_MFN, mfn_date))


save(discontinuation, file = "/home/pcardoso/workspace/SDRN-Cohort-scripts/Interim_Datasets/mm_discontinuation.RData")



