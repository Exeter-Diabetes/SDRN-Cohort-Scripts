# Author: pcardoso
###############################################################################

# Collect non diabetes medication for combos

###############################################################################

rm(list=ls())

# load libraries
library(tidyverse)

## connection to database
con <- dbConn("NDS_2024")


###############################################################################

meds <- c("ace_inhibitors", "beta_blockers", "ca_channel_blockers", "thiazide_diuretics", "loop_diuretics", "ksparing_diuretics", "statins", "arb", "finerenone")

# Define medications
drug_names <- data.frame(
		meds = c("ace_inhibitors", "beta_blockers", "ca_channel_blockers", "thiazide_diuretics", "loop_diuretics", "ksparing_diuretics", "statins", "arb", "finerenone"),
		blood_pressure_drug_name = c("acei", "beta_blockers", "ca_channel_blockers", NA, NA, NA, NA, "arb", NA),
		diuretic_drug_names = c("ACEi", "bb", "CCB", "thiazide", "loop", "k-sparing", NA, "ARB", NA)
)


###############################################################################

# Pull out raw script instances

## read in drug names
blood_pressure_names <- read.csv("/home/pcardoso/workspace/SDRN-Cohort-scripts/Codelists/Prodcodes/Drug name lists/blood_pressure_drug_names.csv") %>%
		select(-Genericorbrand) %>%
		mutate(Name = str_trim(Name)) # remove white spaces
diuretic_names <- read.csv("/home/pcardoso/workspace/SDRN-Cohort-scripts/Codelists/Prodcodes/Drug name lists/Diuretic_names.csv") %>%
		separate_rows("Generic_name", sep = " / ") %>%
		mutate(Drug_class2 = ifelse(Drug_class2 == "", NA, Drug_class2), Drug_class3 = ifelse(Drug_class3 == "", NA, Drug_class3)) %>%
		pivot_longer(!c("Drug_class1", "Drug_class2", "Drug_class3"), names_to = "drug_type", values_to = "drug_name") %>%
		select(-drug_type) %>%
		distinct()
statins_names <- read.csv("/home/pcardoso/workspace/SDRN-Cohort-scripts/Codelists/Prodcodes/Drug name lists/statin_names.csv")
finerenone_names <- read.delim("/home/pcardoso/workspace/SDRN-Cohort-scripts/Codelists/Prodcodes/Renal medications/exeter_prodcodelist_finerenone.txt") %>%
		select(ProductName) %>%
		mutate(ProductName = sub(" .*", "", ProductName)) %>% # remove anything after the main name
		distinct()

for (i in meds) {
	
	print(i)
	
	clean_tablename <- paste0("clean_", i, "_prodcodes")
	
	if (i == "statins") {
		
		current_names <- statins_names %>%
				select(Name) %>%
				mutate(Name = paste0("'%", toupper(Name), "%'")) %>%
				unlist()
		
		mysql_term <- paste(paste("drugname LIKE", current_names), collapse = " OR ")
		
		data <- dbGetQueryMap(con, paste(
						"SELECT 
								o_drug_era.serialno, o_drug_era.startdate, o_drug_era.enddate
								FROM o_drug_era, o_concept_drugs
								WHERE
								o_drug_era.concept_id = o_concept_drugs.UID AND
								(", mysql_term, ")")
		)
		
	} else if (i == "finerenone") {
		
		current_names <- finerenone_names %>%
				mutate(ProductName = paste0("'%", toupper(ProductName), "%'")) %>%
				unlist()
		
		mysql_term <- paste(paste("drugname LIKE", current_names), collapse = " OR ")
		
		data <- dbGetQueryMap(con, paste(
						"SELECT 
								o_drug_era.serialno, o_drug_era.startdate, o_drug_era.enddate
								FROM o_drug_era, o_concept_drugs
								WHERE
								o_drug_era.concept_id = o_concept_drugs.UID AND
								(", mysql_term, ")")
		)
		
	} else {
		
		current_drug_names <- drug_names %>%
				filter(meds == i)
		
		data <- NULL
		
		if (!is.na(current_drug_names$blood_pressure_drug_name)) {
			
			current_names <- current_drug_names %>%
					select("Drug" = "blood_pressure_drug_name") %>%
					left_join(blood_pressure_names, by = "Drug") %>%
					select(-Drug) %>%
					mutate(Name = paste0("'%", toupper(Name), "%'")) %>%
					unlist()
			
			mysql_term <- paste(paste("drugname LIKE", current_names), collapse = " OR ")
			
			data <- data %>% 
					rbind(
							dbGetQueryMap(con, paste(
											"SELECT 
													o_drug_era.serialno, o_drug_era.startdate, o_drug_era.enddate
													FROM o_drug_era, o_concept_drugs
													WHERE
													o_drug_era.concept_id = o_concept_drugs.UID AND
													(", mysql_term, ")")
							)
					)
			
		}
		
		if (!is.na(current_drug_names$diuretic_drug_names)) {
			
			current_names <- current_drug_names %>%
					select("Drug_class" = "diuretic_drug_names") %>%
					left_join(
							diuretic_names %>%
									pivot_longer(!c("drug_name"), values_to = "Drug_class", names_to = "Drug_class_type") %>%
									select(-Drug_class_type), by = "Drug_class"
					) %>%
					select(-Drug_class) %>%
					mutate(drug_name = paste0("'%", toupper(drug_name), "%'")) %>%
					unlist()
			
			mysql_term <- paste(paste("drugname LIKE", current_names), collapse = " OR ")
			
			data <- data %>% 
					rbind(
							dbGetQueryMap(con, paste(
											"SELECT 
													o_drug_era.serialno, o_drug_era.startdate, o_drug_era.enddate
													FROM o_drug_era, o_concept_drugs
													WHERE
													o_drug_era.concept_id = o_concept_drugs.UID AND
													(", mysql_term, ")")
							)
					)
			
		}
		
	}
	
	assign(clean_tablename, data)
	
}


# Disconnect from database
dbDisconnect(con)

###############################################################################

# Clean scripts, then merge with drug start dates

# Get drug start dates (1 row per drug period)
load("/home/pcardoso/workspace/SDRN-Cohort-scripts/Interim_Datasets/mm_drug_start_stop.RData")


# Clean scripts and combine with index dates

for (i in meds) {
	
	print(i)
	
	clean_tablename <- paste0("clean_", i, "_prodcodes")
	drug_merge_tablename <- paste0("full_", i, "_drug_merge")
	
	data <- get(clean_tablename) %>%
			
			inner_join((drug_start_stop %>% select(serialno, dstartdate, drug_class, drug_substance, drug_instance)), by = "serialno") %>%
			mutate(
					drugdatediff = as.numeric(ifelse(dstartdate < startdate, difftime(startdate, dstartdate, units = "days"),
									ifelse(dstartdate > enddate, difftime(enddate, dstartdate, units = "days"), 0))),
					drugdatediff_earliest = as.numeric(difftime(startdate, dstartdate, units = "days"))
			)
	
	assign(drug_merge_tablename, data)
	
}


###############################################################################

# Find earliest predrug, latest predrug and first postdrug dates

non_diabetes_meds <- drug_start_stop %>%
		select(serialno, dstartdate, drug_class, drug_substance, drug_instance)

for (i in meds) {
	
	print(paste("working out predrug and postdrug code occurrences for", i))
	
	drug_merge_tablename <- paste0("full_", i, "_drug_merge")
	predrug_earliest_date_variable <- paste0("predrug_earliest_", i)
	predrug_latest_date_variable <- paste0("predrug_latest_", i)
	postdrug_date_variable <- paste0("postdrug_first_", i)
	
	predrug <- get(drug_merge_tablename) %>%
			filter(drugdatediff <= 0) %>%
			group_by(serialno, dstartdate, drug_substance) %>%
			summarise(
					{{predrug_earliest_date_variable}}:=min(drugdatediff_earliest, na.rm = TRUE),
					{{predrug_latest_date_variable}}:=max(drugdatediff, na.rm = TRUE)
			) %>%
			ungroup() %>%
			mutate(
					{{predrug_earliest_date_variable}}:=dstartdate + !!as.name(predrug_earliest_date_variable),
					{{predrug_latest_date_variable}}:=dstartdate + !!as.name(predrug_latest_date_variable)
			)
	
	postdrug <- get(drug_merge_tablename) %>%
			filter(drugdatediff >= 0) %>%
			group_by(serialno, dstartdate, drug_substance) %>%
			summarise(
					{{postdrug_date_variable}}:=min(drugdatediff, na.rm = TRUE)
			) %>%
			ungroup() %>%
			mutate(
					{{postdrug_date_variable}}:=dstartdate + !!as.name(postdrug_date_variable)
			)
	
	
	non_diabetes_meds <- non_diabetes_meds %>%
			left_join(predrug, by = c("serialno", "dstartdate", "drug_substance")) %>%
			left_join(postdrug, by = c("serialno", "dstartdate", "drug_substance"))
	
}


# save final version
save(non_diabetes_meds, file = "/home/pcardoso/workspace/SDRN-Cohort-scripts/Interim_Datasets/mm_non_diabetes_meds.RData")

