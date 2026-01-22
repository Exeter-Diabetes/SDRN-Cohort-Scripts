# Author: pcardoso
###############################################################################

# Identify those with diabetes at diagnosis, collect non diabetes medication

###############################################################################

rm(list=ls())

# load libraries
library(tidyverse)

## connection to database
con <- dbConn("NDS_2024")


###############################################################################

meds <- c("ace_inhibitors", "beta_blockers", "ca_channel_blockers", "thiazide_diuretics", "loop_diuretics", "ksparing_diuretics", "statins", "arb")

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


# Clean scripts for oha

## Select Metformin initiations
clean_mfn_prodcodes <- dbGetQueryMap(con, "
				SELECT 
				o_drug_era.serialno, o_drug_era.startdate, o_drug_era.enddate
				FROM o_drug_era, o_concept_drugs
				WHERE
				o_drug_era.concept_id = o_concept_drugs.UID AND
				(drugname LIKE 'METFORMIN%' OR 
				drugname IN ('GLUCOPHAGE', 'GLUCOPHAGE SR', 'BOLAMYN SR', 'JANUMET', 
				'METSOL', 'METABET', 'GLUCIENT SR', 'DIAGEMET XL',
				'KOMBOGLYZE', 'SUKKARTO SR', 'MEIJUMET', 'YALTORMIN SR',
				'METUXTAN', 'GLUCOREX SR') OR
				drugname LIKE 'GLUCAMET%' OR
				readcode LIKE 'f41%')")

## Select SGLT2i initiations
clean_sglt2_prodcodes <- dbGetQueryMap(con, "
				SELECT 
				o_drug_era.serialno, o_drug_era.startdate, o_drug_era.enddate
				FROM o_drug_era, o_concept_drugs
				WHERE
				o_drug_era.concept_id = o_concept_drugs.UID AND
				(drugname IN ('DAPAGLIFLOZIN', 'DAPAGLIFLOZIN AND METFORMIN', 'FORXIGA', 'XIGDUO', 'CANAGLIFLOZIN',
				'CANAGLIFLOZIN AND METFORMIN', 'INVOKANA', 'EMPAGLIFLOZIN', 'EMPAGLIFLOZIN AND LINAGLIPTIN',
				'JARDIANCE', 'VOKANAMET', 'SYNJARDY', 'QTERN', 'ERTUGLIFLOZIN', 'STEGLATRO'))")


## Select TZD initiations
clean_tzd_prodcodes <- dbGetQueryMap(con, "
				SELECT
				o_drug_era.serialno, o_drug_era.startdate, o_drug_era.enddate
				FROM o_drug_era, o_concept_drugs
				WHERE
				o_drug_era.concept_id = o_concept_drugs.UID AND
				(drugname IN ('ROSIGLITAZONE', 'ROSIGLITAZONE MALEATE', 'ROSIGLITAZONE AND METFORMIN', 'ROSIGLITAZONE + METFORMIN',
				'ROSIGLITAZONE WITH METFORMIN', 'ROSIGLITAZONE F-C', 'ROSIGLITAZONE ROSIGLITAZONE', 'ROSIGLITAZONE / METFORMIN',
				'ROSIGLITAZON', 'AVANDIA ROSIGLITAZONE', 'AVANDIA', 'AVANDIA 8', 'AVANDIAMET2 / 1', 'PIOGLITAZONE', 
				'PIOGLITAZONE HYDROCHLORIDE', 'PIOGLITAZONE AND METFORMIN', 'PIOGLITAZONE + METFORMIN',
				'PIOGLITAZONE WITH METFORMIN', 'PIOGLITAZON', 'PIOGLITAZONE (ACTOS)', 'ACTOS PIOGLITAZONE',
				'COMPETACT (PIOGLITAZONE / METFORMIN)', 'ACTOS', 'GLIZOFAR', 'GLIDIPION', 'TROGLITAZONE',
				'BIGUANIDE TROGLITAZONE', 'TROGLITAZONE200 MCG' ,'TROGLITIAZONE', 'ROMOZIN', 'ROMOZIN (TROGLITAZONE)'))")


## Select DPP4 initiations
clean_dpp4_prodcodes <- dbGetQueryMap(con, "
				SELECT
				o_drug_era.serialno, o_drug_era.startdate, o_drug_era.enddate
				FROM o_drug_era, o_concept_drugs
				WHERE
				o_drug_era.concept_id = o_concept_drugs.UID AND
				(drugname IN ('SITAGLIPTIN', 'SITAGLIPTIN + METFORMIN', 'SITAGLIPTON', 'SITAGLIPTAIN', 'SITAGLIPTAN / METFORMIN',
				'SITAGLEPTIN', 'SITAGLIPITIN', 'SITAGLIPTIW', 'JANUVIA', 'VILDAGLIPTIN', 'VILDAGLIPTIN + METFORMIN',
				'VILDAGLIPTIN / METFORMIN', 'VILDAGLIPTIN WITH METFORMIN', 'GALVUS', 'SAXAGLIPTIN',
				'SAXAGLIPTIN AND METFORMIN', 'SAXAGLIPTIN AND DAPAGLIFLOZIN', 'SAXAGLIPTAN', 'ONGLYZA',
				'LINAGLIPTIN', 'TRAJENTA', 'JENTADUETO', 'ALOGLIPTIN', 'ALOGLIPTIN AND METFORMIN', 
				'VIPIDIA', 'VIPDOMET'))")


## Select SU initiations
clean_su_prodcodes <- dbGetQueryMap(con, "
				SELECT
				o_drug_era.serialno, o_drug_era.startdate, o_drug_era.enddate
				FROM o_drug_era, o_concept_drugs
				WHERE
				o_drug_era.concept_id = o_concept_drugs.UID AND
				(drugname IN ('AMARYL', 'CHLORPROPAMIDE' ,'DAONIL' ,'SEMI-DAONIL', 'SEMI DAONIL', 'SEMI / DAONIL',
				'DIABETAMIDE', 'DIAMICRON', 'DIAMICRON SR', 'DIAMICRON DIAMICRON', 'DIAMICRON 80 MGM',
				'DIAMICRONE', 'EUGLUCON', 'GLIBENCLAMIDE', 'GLIBENESE', 'GLIPIZIDE', 'GLIPIZIDE PP',
				'GLIMEPIRIDE', 'GLICLAZIDE', '<<D>>GLICLAZIDE', 'GLICLAZIDE 40', 'GLICLAZIDE 1',
				'GLICLAZIDE SR', 'GLICLAZIDE STUDY PREP', 'GLICLAZIDE XL', 'GLURENORM', 'GLIQUIDONE',
				'MINODIAB', 'TOLBUTAMIDE', 'DIAGLYK' ,'DIAGLYCK', 'DIAGLY K', 'DIAGLYX', 'NAZDOL',
				'DACADIS', 'ZICRON', 'ZICRON PR', 'EDICIL', 'LAAGLYDA', 'VAMJU', 'BILXONA',
				'TOLAZAMIDE', 'DIABINESE', 'DIAMICROM', 'RASTINON'))")

## Select GLP1 initiations
clean_glp1_prodcodes <- dbGetQueryMap(con, "
				SELECT
				o_drug_era.serialno, o_drug_era.startdate, o_drug_era.enddate
				FROM o_drug_era, o_concept_drugs
				WHERE
				o_drug_era.concept_id = o_concept_drugs.UID AND
				(drugname IN ('LIRAGLUTIDE', 'LIRAGLUTIDE INJECTION', 'INSULIN DEGLUDEC 100 UNITS / ML / LIRAGLUTIDE',
				'LIRAGLUTIDE PREFILLED', 'INSULIN DEGLUDEC AND LIRAGLUTIDE', 'LIRAGLUTIDE 1' ,
				'LIRAGLUTIDE PRE FILLED', 'LIRAGLUTIDE PREFILLED PENS', 'VICTOZA', 'VICTOZA INJECTION',
				'VICTOZA PREFILLED', 'VICTOZA PREFILLED PENS', 'LIXISENATIDE',
				'INSULIN GLARGINE 100 UNITS / ML / LIXISENATIDE', 'LYXUMIA', 'XULTOPHY 100 UNITS',
				'XULTOPHY', 'DULAGLUTIDE', 'TRULICITY', 'ALBIGLUTIDE', 'SAXENDA', 'EXENATIDE',
				'EXENATIDE INJECTION', 'EXENATIDE 60 DOSE PREFILLED', 'EXENATIDE POWDER AND SOLVENT FOR',
				'EXENATIDE 60-DOSE PREFILLED', 'EXENATIDE PWDR FOR INJECTIONSUSP', 'EXENATIDE SINGLE PREFILLED',
				'BYETTA (EXENATIDE)', 'EXENATIDE INJECTION PREFILLED', 'EXENATIDE PREFILLED',
				'EXENATIDE PRE FILLED', 'EXENATIDE(BYETTA)', 'EXENATIDE DISPOSABLE PENS',
				'EXENATIDE EXENATIDE', 'EXENATIDE PREFILLED PENS', 'BYETTA EXENATIDE', 'EXENATIDE DISPOSABLE',
				'EXENATIDE SUBCUTANEOUS', 'BYETTA', 'BYETTA INJECTION', 'BYETTA 60 DOSE PREFILLED',
				'BYETTA PREFILLED', 'BYETTA BYETTA 10 UG PREFILLED', 'BYETTA INSULIN', 'BYETTA INSULIN',
				'BYETTA 10', 'BYETTA BYETTA 5 UG PREFILLED', 'BYETTA PENS', 'BYETTA INJECTION 10 UG PENFILLED',
				'BYETTA PREFILLED DISPOSABLE', 'BYETTA PREFILLED INJECTION', 'BYETTA PENS BYETTA',
				'BYDUREON', 'BYDUREON BCISE', 'BYDUREON PWDR FOR INJECTIONSUSP', 'BYDUREON POWDER AND SOLVENT FOR',
				'OZEMPIC', 'RYBELSUS', 'SEMAGLUTIDE'))")

## Select insulin initiations

#clean_insulin_prodcodes <- 



# Disconnect from database
dbDisconnect(con)



# Add OHA classes and insulin to meds (missing insulin

meds <- c(meds, "dpp4", "glp1", "mfn", "sglt2", "su", "tzd")

###############################################################################

# Clean scripts and combine with index date

## Get index dates

index_date <- as.Date("2022-11-01", origin = "1970-01-01")

# Get diabetes cohort
load("/home/pcardoso/workspace/SDRN-Cohort-scripts/Interim_Datasets/all_diabetes_cohort.RData")

for (i in meds) {
	
	clean_tablename <- paste0("clean_", i, "_prodcodes")
	index_date_merge_tablename <- paste0("full_", i, "_index_date_merge")
	
	data <- get(clean_tablename) %>%
			mutate(
					datediff = as.numeric(ifelse(index_date < startdate, difftime(startdate, index_date, units = "days"),
									ifelse(index_date > enddate, difftime(enddate, index_date, units = "days"), 0))),
					datediff_earliest = as.numeric(difftime(startdate, index_date, units = "days"))
			)
	
	assign(index_date_merge_tablename, data)
	
}


###############################################################################

# Find earliest pre-index date, latest pre-index date and first post-index date dates

medications <- diabetes_cohort %>%
		select(serialno)

for (i in meds) {
	
	print(paste("working out per- and post- idex date code occurrences for", i))
	
	index_date_merge_tablename <- paste0("full_", i, "_index_date_merge")
	pre_index_date_earliest_date_variable <- paste0("pre_index_date_earliest_", i)
	pre_index_date_latest_date_variable <- paste0("pre_index_date_latest_", i)
	post_index_date_date_variable <- paste0("post_index_date_first_", i)
	
	pre_index_date <- get(index_date_merge_tablename) %>%
			filter(datediff <= 0) %>%
			group_by(serialno) %>%
			summarise(
					{{pre_index_date_earliest_date_variable}}:=min(datediff_earliest, na.rm = TRUE),
					{{pre_index_date_latest_date_variable}}:=max(datediff, na.rm = TRUE)
			) %>%
			ungroup() %>%
			mutate(
					{{pre_index_date_earliest_date_variable}}:=index_date + !!as.name(pre_index_date_earliest_date_variable),
					{{pre_index_date_latest_date_variable}}:=index_date + !!as.name(pre_index_date_latest_date_variable)
			)
	
	post_index_date <- get(index_date_merge_tablename) %>%
			filter(datediff >= 0) %>%
			group_by(serialno) %>%
			summarise(
					{{post_index_date_date_variable}}:=min(datediff, na.rm = TRUE)
			) %>%
			ungroup() %>%
			mutate(
					{{post_index_date_date_variable}}:=index_date + !!as.name(post_index_date_date_variable)
			)
	
	medications <- medications %>%
			left_join(pre_index_date, by = "serialno") %>%
			left_join(post_index_date, by = "serialno")
	
}

# save final version
save(medications, file = "/home/pcardoso/workspace/SDRN-Cohort-scripts/Interim_Datasets/prev_medications.RData")




