# Author: pcardoso
###############################################################################

# Identify those with diabetes type 2, collect drug combos for each patient

###############################################################################

# load libraries
library(tidyverse)

# load table
drug_class_table <- readRDS("/home/pcardoso/workspace/SDRN-Cohort-scripts/Codelists/Diabetes Medication/data_class_table.rds")


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

## Select Metformin initiations for type 2 diabetes patients
mfn_initiations <- dbGetQueryMap(con, "
	SELECT 
		o_drug_era.serialno, o_drug_era.startdate, o_drug_era.enddate, o_drug_era.dailyexposure,
		o_concept_drugs.drugname, o_concept_drugs.strength
	FROM o_drug_era, o_concept_drugs, o_person
	WHERE
		o_person.date_of_birth < '2022-11-01' AND o_person.dm_type = 2 AND o_person.earliest_mention IS NOT NULL AND
		o_person.serialno = o_drug_era.serialno AND
		o_drug_era.concept_id = o_concept_drugs.UID AND
			(drugname LIKE 'METFORMIN%' OR 
				drugname IN ('GLUCOPHAGE', 'GLUCOPHAGE SR', 'BOLAMYN SR', 'JANUMET', 
							'METSOL', 'METABET', 'GLUCIENT SR', 'DIAGEMET XL',
							'KOMBOGLYZE', 'SUKKARTO SR', 'MEIJUMET', 'YALTORMIN SR',
							'METUXTAN', 'GLUCOREX SR') OR
				drugname LIKE 'GLUCAMET%' OR
			readcode LIKE 'f41%')") %>%
	left_join(
			drug_class_table, by = c("drugname")
	)

# Check if there are any treatments not classified
mfn_initiations %>% filter(is.na(drug_class_1)) %>% head()


## Select SGLT2i initiations for type 2 diabetes patients
sglt2_initiations <- dbGetQueryMap(con, "
	SELECT 
		o_drug_era.serialno, o_drug_era.startdate, o_drug_era.enddate, o_drug_era.dailyexposure,
		o_concept_drugs.drugname, o_concept_drugs.strength
	FROM o_drug_era, o_concept_drugs, o_person
	WHERE
		o_person.date_of_birth < '2022-11-01' AND o_person.dm_type = 2 AND o_person.earliest_mention IS NOT NULL AND
		o_person.serialno = o_drug_era.serialno AND
		o_drug_era.concept_id = o_concept_drugs.UID AND
			(drugname IN ('DAPAGLIFLOZIN', 'DAPAGLIFLOZIN AND METFORMIN', 'FORXIGA', 'XIGDUO', 'CANAGLIFLOZIN',
						'CANAGLIFLOZIN AND METFORMIN', 'INVOKANA', 'EMPAGLIFLOZIN', 'EMPAGLIFLOZIN AND LINAGLIPTIN',
						'JARDIANCE', 'VOKANAMET', 'SYNJARDY', 'QTERN', 'ERTUGLIFLOZIN', 'STEGLATRO'))") %>%
	left_join(
			drug_class_table, by = c("drugname")
	)


# Check if there are any treatments not classified
sglt2_initiations %>% filter(is.na(drug_class_1)) %>% head()


## Select TZD initiations for type 2 diabetes patients
tzd_initiations <- dbGetQueryMap(con, "
	SELECT
		o_drug_era.serialno, o_drug_era.startdate, o_drug_era.enddate, o_drug_era.dailyexposure,
		o_concept_drugs.drugname, o_concept_drugs.strength
	FROM o_drug_era, o_concept_drugs, o_person
	WHERE
		o_person.date_of_birth < '2022-11-01' AND o_person.dm_type = 2 AND o_person.earliest_mention IS NOT NULL AND
		o_person.serialno = o_drug_era.serialno AND
		o_drug_era.concept_id = o_concept_drugs.UID AND
			(drugname IN ('ROSIGLITAZONE', 'ROSIGLITAZONE MALEATE', 'ROSIGLITAZONE AND METFORMIN', 'ROSIGLITAZONE + METFORMIN',
						'ROSIGLITAZONE WITH METFORMIN', 'ROSIGLITAZONE F-C', 'ROSIGLITAZONE ROSIGLITAZONE', 'ROSIGLITAZONE / METFORMIN',
						'ROSIGLITAZON', 'AVANDIA ROSIGLITAZONE', 'AVANDIA', 'AVANDIA 8', 'AVANDIAMET2 / 1', 'PIOGLITAZONE', 
						'PIOGLITAZONE HYDROCHLORIDE', 'PIOGLITAZONE AND METFORMIN', 'PIOGLITAZONE + METFORMIN',
						'PIOGLITAZONE WITH METFORMIN', 'PIOGLITAZON', 'PIOGLITAZONE (ACTOS)', 'ACTOS PIOGLITAZONE',
						'COMPETACT (PIOGLITAZONE / METFORMIN)', 'ACTOS', 'GLIZOFAR', 'GLIDIPION', 'TROGLITAZONE',
						'BIGUANIDE TROGLITAZONE', 'TROGLITAZONE200 MCG' ,'TROGLITIAZONE', 'ROMOZIN', 'ROMOZIN (TROGLITAZONE)'))") %>%
	left_join(
			drug_class_table, by = c("drugname")
	)


# Check if there are any treatments not classified
tzd_initiations %>% filter(is.na(drug_class_1)) %>% head()



## Select DPP4 initiations for type 2 diabetes patients
dpp4_initiations <- dbGetQueryMap(con, "
						SELECT
						o_drug_era.serialno, o_drug_era.startdate, o_drug_era.enddate, o_drug_era.dailyexposure,
						o_concept_drugs.drugname, o_concept_drugs.strength
						FROM o_drug_era, o_concept_drugs, o_person
						WHERE
						o_person.date_of_birth < '2022-11-01' AND o_person.dm_type = 2 AND o_person.earliest_mention IS NOT NULL AND
						o_person.serialno = o_drug_era.serialno AND
						o_drug_era.concept_id = o_concept_drugs.UID AND
						(drugname IN ('SITAGLIPTIN', 'SITAGLIPTIN + METFORMIN', 'SITAGLIPTON', 'SITAGLIPTAIN', 'SITAGLIPTAN / METFORMIN',
									'SITAGLEPTIN', 'SITAGLIPITIN', 'SITAGLIPTIW', 'JANUVIA', 'VILDAGLIPTIN', 'VILDAGLIPTIN + METFORMIN',
									'VILDAGLIPTIN / METFORMIN', 'VILDAGLIPTIN WITH METFORMIN', 'GALVUS', 'SAXAGLIPTIN',
									'SAXAGLIPTIN AND METFORMIN', 'SAXAGLIPTIN AND DAPAGLIFLOZIN', 'SAXAGLIPTAN', 'ONGLYZA',
									'LINAGLIPTIN', 'TRAJENTA', 'JENTADUETO', 'ALOGLIPTIN', 'ALOGLIPTIN AND METFORMIN', 
									'VIPIDIA', 'VIPDOMET'))") %>%
		left_join(
				drug_class_table, by = c("drugname")
		)


# Check if there are any treatments not classified
dpp4_initiations %>% filter(is.na(drug_class_1)) %>% head()


## Select SU initiations for type 2 diabetes patients
su_initiations <- dbGetQueryMap(con, "
						SELECT
						o_drug_era.serialno, o_drug_era.startdate, o_drug_era.enddate, o_drug_era.dailyexposure,
						o_concept_drugs.drugname, o_concept_drugs.strength
						FROM o_drug_era, o_concept_drugs, o_person
						WHERE
						o_person.date_of_birth < '2022-11-01' AND o_person.dm_type = 2 AND o_person.earliest_mention IS NOT NULL AND
						o_person.serialno = o_drug_era.serialno AND
						o_drug_era.concept_id = o_concept_drugs.UID AND
						(drugname IN ('AMARYL', 'CHLORPROPAMIDE' ,'DAONIL' ,'SEMI-DAONIL', 'SEMI DAONIL', 'SEMI / DAONIL',
									'DIABETAMIDE', 'DIAMICRON', 'DIAMICRON SR', 'DIAMICRON DIAMICRON', 'DIAMICRON 80 MGM',
									'DIAMICRONE', 'EUGLUCON', 'GLIBENCLAMIDE', 'GLIBENESE', 'GLIPIZIDE', 'GLIPIZIDE PP',
									'GLIMEPIRIDE', 'GLICLAZIDE', '<<D>>GLICLAZIDE', 'GLICLAZIDE 40', 'GLICLAZIDE 1',
									'GLICLAZIDE SR', 'GLICLAZIDE STUDY PREP', 'GLICLAZIDE XL', 'GLURENORM', 'GLIQUIDONE',
									'MINODIAB', 'TOLBUTAMIDE', 'DIAGLYK' ,'DIAGLYCK', 'DIAGLY K', 'DIAGLYX', 'NAZDOL',
									'DACADIS', 'ZICRON', 'ZICRON PR', 'EDICIL', 'LAAGLYDA', 'VAMJU', 'BILXONA',
									'TOLAZAMIDE', 'DIABINESE', 'DIAMICROM', 'RASTINON'))") %>%
		left_join(
				drug_class_table, by = c("drugname")
		)


# Check if there are any treatments not classified
su_initiations %>% filter(is.na(drug_class_1)) %>% head()

## Select GLP1 initiations for type 2 diabetes patients
glp1_initiations <- dbGetQueryMap(con, "
						SELECT
						o_drug_era.serialno, o_drug_era.startdate, o_drug_era.enddate, o_drug_era.dailyexposure,
						o_concept_drugs.drugname, o_concept_drugs.strength
						FROM o_drug_era, o_concept_drugs, o_person
						WHERE
						o_person.date_of_birth < '2022-11-01' AND o_person.dm_type = 2 AND o_person.earliest_mention IS NOT NULL AND
						o_person.serialno = o_drug_era.serialno AND
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
									'OZEMPIC', 'RYBELSUS', 'SEMAGLUTIDE'))") %>%
		left_join(
				drug_class_table, by = c("drugname")
		) %>%
		mutate(
			###### Oral semaglutide
			drug_class_1 = ifelse(drugname == "SEMAGLUTIDE" & strength %in% c("14 MG", "7 MG", "3 MG"), "GLP1", drug_class_1),
			drug_substance_1 = ifelse(drugname == "SEMAGLUTIDE" & strength %in% c("14 MG", "7 MG", "3 MG"), "Oral semaglutide", drug_substance_1),
			###### Low-dose semaglutide
			drug_class_1 = ifelse(drugname == "SEMAGLUTIDE" & grepl(c("0.25 MG / 0.19 ML"), strength), "GLP1", drug_class_1),
			drug_substance_1 = ifelse(drugname == "SEMAGLUTIDE" & grepl(c("0.25 MG / 0.19 ML"), strength), "Low-dose semaglutide", drug_substance_1),
			drug_class_1 = ifelse(drugname == "SEMAGLUTIDE" & grepl(c("0.5 MG / 0.37 ML"), strength), "GLP1", drug_class_1),
			drug_substance_1 = ifelse(drugname == "SEMAGLUTIDE" & grepl(c("0.5 MG / 0.37 ML"), strength), "Low-dose semaglutide", drug_substance_1),
			drug_class_1 = ifelse(drugname == "SEMAGLUTIDE" & grepl(c("1 MG / 0.74 ML"), strength), "GLP1", drug_class_1),
			drug_substance_1 = ifelse(drugname == "SEMAGLUTIDE" & grepl(c("1 MG / 0.74 ML"), strength), "Low-dose semaglutide", drug_substance_1)
		)


# Check if there are any treatments not classified
glp1_initiations %>% filter(is.na(drug_class_1)) %>% head()




## disconnect from database
dbDisconnect(con)


##########################################################################
# Sort out combination medications

## Raw data has single line for combination meds - expand so there is 1 line per drug class/substance
## Raw data has drug_class_1, drug_class_2, drug_substance_1, drug_substance_2 columns
## Pivotting reshapes to give 1 line per drug class/substance

## Remove rows with NA for drug_class

oha_list <- mfn_initiations %>%
		rbind(
			sglt2_initiations,
			tzd_initiations,
			dpp4_initiations,
			su_initiations,
			glp1_initiations
		) %>%
		select(-dailyexposure, -drugname, -strength) %>%
		pivot_longer(cols = c(starts_with("drug_class"), starts_with("drug_substance")), names_to = c(".value", "row"), names_pattern = "([A-Za-z]+_[A-Za-z]+)_(\\d+)") %>%
		select(-row) %>%
		filter(!is.na(drug_class))

# Remove serialno/startdate/enddate/drug_class/drug_substance duplicates
oha_list <- oha_list %>%
		group_by(serialno, startdate, enddate, drug_class, drug_substance) %>%
		filter(row_number() == 1) %>%
		ungroup()

##################################################################
# Define whether date is start or stop for each drug class/substance

# We are using already set periods, so we just duplicate all rows, one being the start and the other being the stop
## Remove treatment eras that start and finish on the same day
## Set the start and end of treatment eras (specific SDRN data structure) and create a row for each start and end separately
## For initiation:
### If first entry of the class/substance, set as start
### If labelled as start and previous row is not a start and time since lass is >183d, set as start
### Else, set as not important (0)
## For ending:
### If last entry of the class/substance, set as start
### If labelled as a finish and following row is not a finish and time to next is >183d, set as start
### Else, set as not importnat (0)
all_scripts_long <- oha_list %>%
		## remove entries that start and finish the same day
		filter(difftime(enddate, startdate, units = "days") > 0) %>%
		## drug_substance era start and end
		rename("date" = "startdate") %>%
		mutate(
			dstart_substance_orig = 1,
			dstop_substance_orig = 0
		) %>%
		select(-enddate) %>%
		rbind(
			oha_list %>%
				## remove entries that start and finish the same day
				filter(difftime(enddate, startdate, units = "days") > 0) %>%
				## drug_substance era start and end
				rename("date" = "enddate") %>%
				mutate(
					dstart_substance_orig = 0,
					dstop_substance_orig = 1
				) %>%
				select(-startdate)
		) %>%
		group_by(serialno, drug_substance) %>%
		arrange(date) %>%
		mutate(
			n_substance_era_started = cumsum(dstart_substance_orig),
			n_substance_era_stopped = cumsum(dstop_substance_orig),
			n_substance_era = n_substance_era_started - n_substance_era_stopped
		) %>%
		select(-n_substance_era_started, -n_substance_era_stopped) %>%
		mutate(
			dstart_substance_interim = ifelse(dstart_substance_orig == 1 & n_substance_era == 1, 1, 0),
			dstop_substance_interim = ifelse(dstop_substance_orig == 1 & n_substance_era == 0, 1, 0)
		) %>%
		select(-dstart_substance_orig, -dstop_substance_orig, -n_substance_era) %>%
		filter(dstart_substance_interim + dstop_substance_interim > 0) %>%
		mutate(
			dstart_substance = ifelse(dstart_substance_interim == 1 & lag(dstart_substance_interim) %in% c(NA), 1, 
					ifelse(dstart_substance_interim == 1 & lag(dstart_substance_interim) == 0 & difftime(date, lag(date), units = "days") > 183, 1, 0)),
			dstop_substance = ifelse(dstop_substance_interim == 1 & is.na(lead(dstop_substance_interim)), 1,
					ifelse(dstop_substance_interim == 1 & lead(dstop_substance_interim) == 0 & difftime(lead(date), date, units = "days") > 183, 1, 0))
		) %>%
		select(-dstart_substance_interim, -dstop_substance_interim) %>%
		ungroup() %>%
		## keep only entries for start and stop
		mutate(
			interim_start_stop = dstart_substance + dstop_substance
		) %>%
		filter(interim_start_stop > 0) %>%
		select(-interim_start_stop) %>%
		## drug_class start and end
		group_by(serialno, drug_class) %>%
		arrange(date) %>%
		mutate(
			dstart_class = ifelse(dstart_substance == 1 & lag(dstart_substance) %in% c(NA), 1, 
					ifelse(dstart_substance == 1 & lag(dstart_substance) == 0 & difftime(date, lag(date), units = "days") > 183, 1, 0)),
			dstop_class = ifelse(dstop_substance == 1 & is.na(lead(dstop_substance)), 1,
					ifelse(dstop_substance == 1 & lead(dstop_substance) == 0 & difftime(lead(date), date, units = "days") > 183, 1, 0))
		) %>%
		ungroup() %>%
		arrange(serialno, date)



# Define number of drug classes started (numstart) and stopped on each date (numstop)
all_scripts_long <- all_scripts_long %>%
		group_by(serialno, date) %>%
		mutate(
			numstart_class = sum(dstart_class, na.rm = TRUE),
			numstop_class = sum(dstop_class, na.rm = TRUE),
			numstart_substance = sum(dstart_substance, na.rm = TRUE),
			numstop_substance = sum(dstop_substance, na.rm = TRUE)
		) %>%
		ungroup()


save(all_scripts_long, file = "/home/pcardoso/workspace/SDRN-Cohort-scripts/Interim_Datasets/mm_all_scripts_long.RData")


##################################################################
# all_scripts table: 1 line per serialno / date, with drug class and drug substance info in wide format

# Define drug classes and drug substances in dataset

#sort(unique(all_scripts_long$drug_class))
drugclasses <- c("DPP4", "GLP1", "INS", "MFN", "SGLT2", "SU", "TZD")

#sort(unique(all_scripts_long$drug_substance))
#[1] "Albiglutide"                 "Alogliptin"                 
#[3] "Canagliflozin"               "Chlorpropamide"             
#[5] "Dapagliflozin"               "Dulaglutide"                
#[7] "Empagliflozin"               "Ertugliflozin"              
#[9] "Exenatide"                   "Exenatide prolonged-release"
#[11] "Glibenclamide"               "Gliclazide"                 
#[13] "Glimepiride"                 "Glipizide"                  
#[15] "Gliquidone"                  "Insulin"                    
#[17] "Linagliptin"                 "Liraglutide"                
#[19] "Lixisenatide"                "Low-dose semaglutide"       
#[21] "Metformin"                   "Oral semaglutide"           
#[23] "Pioglitazone"                "Rosiglitazone"              
#[25] "Saxagliptin"                 "Sitagliptin"                
#[27] "Tolazamide"                  "Tolbutamide"                
#[29] "Troglitazone"                "Vildagliptin"
drugsubstances <- c("Albiglutide", "Alogliptin", "Canagliflozin", "Chlorpropamide", "Dapagliflozin", "Dulaglutide", "Empagliflozin",
		"Ertugliflozin", "Exenatide", "Exenatide prolonged-release", "Glibenclamide", "Gliclazide", "Glimepiride", "Glipizide", "Gliquidone", "Insulin",
		"Linagliptin", "Liraglutide", "Lixisenatide", "Low-dose semaglutide", "Metformin", "Oral semaglutide", "Pioglitazone", "Rosiglitazone",
		"Saxagliptin", "Sitagliptin", "Tolazamide", "Tolbutamide", "Troglitazone", "Vildagliptin")


# Reshape all_scripts_long wide by drug class and drug substance - 1 row per patid/date
all_scripts_class_wide <- all_scripts_long %>%
		pivot_wider(
			id_cols = c(serialno, date, numstart_class, numstop_class),
			names_from = drug_class,
			values_from = c(dstart_class, dstop_class),
			values_fill = 0,
			values_fn = max
		)

#all_scripts_class_wide %>% nrow()
#2301488


all_scripts_substance_wide <- all_scripts_long %>%
		pivot_wider(
			id_cols = c(serialno, date, numstart_substance, numstop_substance),
			names_from = drug_substance,
			values_from = c(dstart_substance, dstop_substance),
			values_fill = 0,
			values_fn = max
		)

#all_scripts_substance_wide %>% nrow()
#2301488

all_scripts <- all_scripts_class_wide %>%
		inner_join(all_scripts_substance_wide, by = c("serialno", "date"))


#all_scripts %>% nrow()
#2301488

# Use numstart and numstop to work out total number of drug classes patient is on at each date (numdrugs; they can be on a drug even if not prescribed on that date)
## Add numstop to numdrugs so that drug stopped is included in numdrugs count on the date it is stopped
all_scripts <- all_scripts %>%
		group_by(serialno) %>%
		arrange(date) %>%
		mutate(
			cu_numstart_class = cumsum(numstart_class),
			cu_numstop_class = cumsum(numstop_class),
			numdrugs_class = cu_numstart_class - cu_numstop_class + numstop_class,
			cu_numstart_substance = cumsum(numstart_substance),
			cu_numstop_substance = cumsum(numstop_substance),
			numdrugs_substance = cu_numstart_substance - cu_numstop_substance + numstop_substance
		) %>%
		ungroup()

# Make variable for what combination of drugs patients is on at each date
## First make binary variables for each drug for whether patient was on the drug (whether or not prescribed) at each date
all_scripts <- all_scripts %>%
		group_by(serialno) %>%
		arrange(date) %>%
		mutate(
			DPP4 = cumsum(dstart_class_DPP4) > cumsum(dstop_class_DPP4) | dstart_class_DPP4==1 | dstop_class_DPP4 == 1,
			GLP1 = cumsum(dstart_class_GLP1) > cumsum(dstop_class_GLP1) | dstart_class_GLP1==1 | dstop_class_GLP1 == 1,
			INS = cumsum(dstart_class_INS) > cumsum(dstop_class_INS) | dstart_class_INS==1 | dstop_class_INS == 1,
			MFN = cumsum(dstart_class_MFN) > cumsum(dstop_class_MFN) | dstart_class_MFN==1 | dstop_class_MFN == 1,
			SGLT2 = cumsum(dstart_class_SGLT2) > cumsum(dstop_class_SGLT2) | dstart_class_SGLT2==1 | dstop_class_SGLT2 == 1,
			SU = cumsum(dstart_class_SU) > cumsum(dstop_class_SU) | dstart_class_SU==1 | dstop_class_SU == 1,
			TZD = cumsum(dstart_class_TZD) > cumsum(dstop_class_TZD) | dstart_class_TZD==1 | dstop_class_TZD == 1,
			
			
			`Albiglutide` = cumsum(`dstart_substance_Albiglutide`) > cumsum(`dstop_substance_Albiglutide`) | `dstart_substance_Albiglutide`==1 | `dstop_substance_Albiglutide` == 1,
			`Alogliptin` = cumsum(`dstart_substance_Alogliptin`) > cumsum(`dstop_substance_Alogliptin`) | `dstart_substance_Alogliptin`==1 | `dstop_substance_Alogliptin` == 1,
			`Canagliflozin` = cumsum(`dstart_substance_Canagliflozin`) > cumsum(`dstop_substance_Canagliflozin`) | `dstart_substance_Canagliflozin`==1 | `dstop_substance_Canagliflozin` == 1,
			`Chlorpropamide` = cumsum(`dstart_substance_Chlorpropamide`) > cumsum(`dstop_substance_Chlorpropamide`) | `dstart_substance_Chlorpropamide`==1 | `dstop_substance_Chlorpropamide` == 1,
			`Dapagliflozin` = cumsum(`dstart_substance_Dapagliflozin`) > cumsum(`dstop_substance_Dapagliflozin`) | `dstart_substance_Dapagliflozin`==1 | `dstop_substance_Dapagliflozin` == 1,
			`Dulaglutide` = cumsum(`dstart_substance_Dulaglutide`) > cumsum(`dstop_substance_Dulaglutide`) | `dstart_substance_Dulaglutide`==1 | `dstop_substance_Dulaglutide` == 1,
			`Empagliflozin` = cumsum(`dstart_substance_Empagliflozin`) > cumsum(`dstop_substance_Empagliflozin`) | `dstart_substance_Empagliflozin`==1 | `dstop_substance_Empagliflozin` == 1,
			`Ertugliflozin` = cumsum(`dstart_substance_Ertugliflozin`) > cumsum(`dstop_substance_Ertugliflozin`) | `dstart_substance_Ertugliflozin`==1 | `dstop_substance_Ertugliflozin` == 1,
			`Exenatide` = cumsum(`dstart_substance_Exenatide`) > cumsum(`dstop_substance_Exenatide`) | `dstart_substance_Exenatide`==1 | `dstop_substance_Exenatide` == 1,
			`Exenatide prolonged-release` = cumsum(`dstart_substance_Exenatide prolonged-release`) > cumsum(`dstop_substance_Exenatide prolonged-release`) | `dstart_substance_Exenatide prolonged-release`==1 | `dstop_substance_Exenatide prolonged-release` == 1,
			`Glibenclamide` = cumsum(`dstart_substance_Glibenclamide`) > cumsum(`dstop_substance_Glibenclamide`) | `dstart_substance_Glibenclamide`==1 | `dstop_substance_Glibenclamide` == 1,
			`Gliclazide` = cumsum(`dstart_substance_Gliclazide`) > cumsum(`dstop_substance_Gliclazide`) | `dstart_substance_Gliclazide`==1 | `dstop_substance_Gliclazide` == 1,
			`Glimepiride` = cumsum(`dstart_substance_Glimepiride`) > cumsum(`dstop_substance_Glimepiride`) | `dstart_substance_Glimepiride`==1 | `dstop_substance_Glimepiride` == 1,
			`Glipizide` = cumsum(`dstart_substance_Glipizide`) > cumsum(`dstop_substance_Glipizide`) | `dstart_substance_Glipizide`==1 | `dstop_substance_Glipizide` == 1,
			`Gliquidone` = cumsum(`dstart_substance_Gliquidone`) > cumsum(`dstop_substance_Gliquidone`) | `dstart_substance_Gliquidone`==1 | `dstop_substance_Gliquidone` == 1,
			`Insulin` = cumsum(`dstart_substance_Insulin`) > cumsum(`dstop_substance_Insulin`) | `dstart_substance_Insulin`==1 | `dstop_substance_Insulin` == 1,
			`Linagliptin` = cumsum(`dstart_substance_Linagliptin`) > cumsum(`dstop_substance_Linagliptin`) | `dstart_substance_Linagliptin`==1 | `dstop_substance_Linagliptin` == 1,
			`Liraglutide` = cumsum(`dstart_substance_Liraglutide`) > cumsum(`dstop_substance_Liraglutide`) | `dstart_substance_Liraglutide`==1 | `dstop_substance_Liraglutide` == 1,
			`Lixisenatide` = cumsum(`dstart_substance_Lixisenatide`) > cumsum(`dstop_substance_Lixisenatide`) | `dstart_substance_Lixisenatide`==1 | `dstop_substance_Lixisenatide` == 1,
			`Low-dose semaglutide` = cumsum(`dstart_substance_Low-dose semaglutide`) > cumsum(`dstop_substance_Low-dose semaglutide`) | `dstart_substance_Low-dose semaglutide`==1 | `dstop_substance_Low-dose semaglutide` == 1,
			`Metformin` = cumsum(`dstart_substance_Metformin`) > cumsum(`dstop_substance_Metformin`) | `dstart_substance_Metformin`==1 | `dstop_substance_Metformin` == 1,
			`Oral semaglutide` = cumsum(`dstart_substance_Oral semaglutide`) > cumsum(`dstop_substance_Oral semaglutide`) | `dstart_substance_Oral semaglutide`==1 | `dstop_substance_Oral semaglutide` == 1,
			`Pioglitazone` = cumsum(`dstart_substance_Pioglitazone`) > cumsum(`dstop_substance_Pioglitazone`) | `dstart_substance_Pioglitazone`==1 | `dstop_substance_Pioglitazone` == 1,
			`Rosiglitazone` = cumsum(`dstart_substance_Rosiglitazone`) > cumsum(`dstop_substance_Rosiglitazone`) | `dstart_substance_Rosiglitazone`==1 | `dstop_substance_Rosiglitazone` == 1,
			`Saxagliptin` = cumsum(`dstart_substance_Saxagliptin`) > cumsum(`dstop_substance_Saxagliptin`) | `dstart_substance_Saxagliptin`==1 | `dstop_substance_Saxagliptin` == 1,
			`Sitagliptin` = cumsum(`dstart_substance_Sitagliptin`) > cumsum(`dstop_substance_Sitagliptin`) | `dstart_substance_Sitagliptin`==1 | `dstop_substance_Sitagliptin` == 1,
			`Tolazamide` = cumsum(`dstart_substance_Tolazamide`) > cumsum(`dstop_substance_Tolazamide`) | `dstart_substance_Tolazamide`==1 | `dstop_substance_Tolazamide` == 1,
			`Tolbutamide` = cumsum(`dstart_substance_Tolbutamide`) > cumsum(`dstop_substance_Tolbutamide`) | `dstart_substance_Tolbutamide`==1 | `dstop_substance_Tolbutamide` == 1,
			`Troglitazone` = cumsum(`dstart_substance_Troglitazone`) > cumsum(`dstop_substance_Troglitazone`) | `dstart_substance_Troglitazone`==1 | `dstop_substance_Troglitazone` == 1,
			`Vildagliptin` = cumsum(`dstart_substance_Vildagliptin`) > cumsum(`dstop_substance_Vildagliptin`) | `dstart_substance_Vildagliptin`==1 | `dstop_substance_Vildagliptin` == 1
		) %>%
		ungroup()



## Use binary drug class columns to make single 'drugcombo' column with the names of all the drug classes patient is on at each date
all_scripts <- all_scripts %>%
		select(-c(starts_with("dstart"), starts_with("dstop"))) %>%
		mutate(
			drug_class_combo = paste0(ifelse(DPP4==1, "DPP4_", ""),
					ifelse(GLP1==1, "GLP1_", ""),
					ifelse(INS==1, "INS_", ""),
					ifelse(MFN==1, "MFN_", ""),
					ifelse(SGLT2==1, "SGLT2_", ""),
					ifelse(SU==1, "SU_", ""),
					ifelse(TZD==1, "TZD_", "")),
			drug_class_combo = ifelse(str_sub(drug_class_combo, -1, -1)=="_", str_sub(drug_class_combo, 1, -2), drug_class_combo),
			drug_substance_combo = paste0(ifelse(`Albiglutide`==1, "Albiglutide_", ""),
					ifelse(`Alogliptin`==1, "Alogliptin_", ""),
					ifelse(`Canagliflozin`==1, "Canagliflozin_", ""),
					ifelse(`Chlorpropamide`==1, "Chlorpropamide_", ""),
					ifelse(`Dapagliflozin`==1, "Dapagliflozin_", ""),
					ifelse(`Dulaglutide`==1, "Dulaglutide_", ""),
					ifelse(`Empagliflozin`==1, "Empagliflozin_", ""),
					ifelse(`Ertugliflozin`==1, "Ertugliflozin_", ""),
					ifelse('Exenatide'==1, "Exenatide_", ""),
					ifelse(`Exenatide prolonged-release`==1, "Exenatide prolonged-release_", ""),
					ifelse(`Glibenclamide`==1, "Glibenclamide_", ""),
					ifelse(`Gliclazide`==1, "Gliclazide_", ""),
					ifelse(`Glimepiride`==1, "Glimepiride_", ""),
					ifelse(`Glipizide`==1, "Glipizide_", ""),
					ifelse(`Gliquidone`==1, "Gliquidone_", ""),
					ifelse(`Insulin`==1, "Insulin_", ""),
					ifelse(`Linagliptin`==1, "Linagliptin_", ""),
					ifelse(`Liraglutide`==1, "Liraglutide_", ""),
					ifelse(`Lixisenatide`==1, "Lixisenatide_", ""),
					ifelse(`Low-dose semaglutide`==1, "Low-dose semaglutide_", ""),
					ifelse(`Metformin`==1, "Metformin_", ""),
					ifelse(`Oral semaglutide`==1, "Oral semaglutide_", ""),
					ifelse(`Pioglitazone`==1, "Pioglitazone_", ""),
					ifelse(`Rosiglitazone`==1, "Rosiglitazone_", ""),
					ifelse(`Saxagliptin`==1, "Saxagliptin_", ""),
					ifelse(`Sitagliptin`==1, "Sitagliptin_", ""),
					ifelse(`Tolazamide`==1, "Tolazamide_", ""),
					ifelse(`Tolbutamide`==1, "Tolbutamide_", ""),
					ifelse(`Troglitazone`==1, "Troglitazone_", ""),
					ifelse(`Vildagliptin`==1, "Vildagliptin_", "")),
			drug_substance_combo = ifelse(str_sub(drug_substance_combo, -1, -1)=="_", str_sub(drug_substance_combo, 1, -2), drug_substance_combo)
		)

# Recalculate numdrugs (number of different drug classes patients is on at each date) and check it matches earlier calculation
all_scripts <- all_scripts %>%
		mutate(
			numdrugs_class2 = DPP4 + GLP1 + INS + MFN + SGLT2 + SU + TZD,
			numdrugs_substance2 = Albiglutide + Alogliptin + Canagliflozin + Chlorpropamide + Dapagliflozin + Dulaglutide + Empagliflozin +
					Ertugliflozin + Exenatide + `Exenatide prolonged-release` + Glibenclamide + Gliclazide + Glimepiride + Glipizide + Gliquidone + Insulin +
					Linagliptin + Liraglutide + Lixisenatide + `Low-dose semaglutide` + Metformin + `Oral semaglutide` + Pioglitazone + Rosiglitazone +
					Saxagliptin + Sitagliptin + Tolazamide + Tolbutamide + Troglitazone + Vildagliptin
		)

#all_scripts %>% filter(numdrugs_class!=numdrugs_class2 | is.na(numdrugs_class) | is.na(numdrugs_class2)) %>% nrow()
#0 - perfect
#all_scripts %>% filter(numdrugs_substance!=numdrugs_substance2 | is.na(numdrugs_substance) | is.na(numdrugs_substance2)) %>% nrow()
#0 - perfect

# Define whether date is start or stop for each drug combination
## Coded differently to drug classes as patient can only be on one combination at once
## Find time from previous script (dcprevuse) and to next script (dcnextuse) for same person and same drug combo
## If previous script is for a different drug combo or no previous script, define as start date (dcstart = 1)
## If next script is for a different drug combo or no next script, define as stop date (dcstop = 1)
all_scripts <- all_scripts %>%
		group_by(serialno, drug_class_combo) %>%
		arrange(date) %>%
		mutate(
			dcnextuse_class = difftime(lead(date), date, units = "days"),
			dcprevuse_class = difftime(date, lag(date), units = "days")
		) %>%
		ungroup()


all_scripts <- all_scripts %>%
		group_by(serialno, drug_substance_combo) %>%
		arrange(date) %>%
		mutate(
			dcnextuse_substance = difftime(lead(date), date, units = "days"),
			dcprevuse_substance = difftime(date, lag(date), units = "days")
		) %>%
		ungroup()

all_scripts <- all_scripts %>%
		group_by(serialno) %>%
		arrange(date) %>%
		mutate(
			dcstart_class = drug_class_combo!=lag(drug_class_combo) | is.na(dcprevuse_class),
			dcstop_class = drug_class_combo!=lead(drug_class_combo) | is.na(dcnextuse_class),
			dcstart_substance = drug_substance_combo!=lag(drug_substance_combo) | is.na(dcprevuse_substance),
			dcstop_substance = drug_substance_combo!=lead(drug_substance_combo) | is.na(dcnextuse_substance)
		) %>%
		ungroup()

#all_scripts %>% nrow()
#2301488

# Add 'gaps': defined as break of >6 months (183 days) in prescribing of combination
## Update start and stop dates based on these
all_scripts <- all_scripts %>%
		group_by(serialno) %>%
		arrange(date) %>%
		mutate(
			stopgap_class = ifelse(dcnextuse_class>183 & drug_class_combo==lead(drug_class_combo), 1, NA),
			startgap_class = ifelse(dcprevuse_class>183 | is.na(dcprevuse_class), 1, NA),
			dcstop_class = ifelse(!is.na(stopgap_class) & stopgap_class==1 & dcstop_class==0, 1, dcstop_class),
			dcstart_class = ifelse(!is.na(startgap_class) & startgap_class==1 & dcstart_class==0, 1, dcstart_class),
			
			stopgap_substance = ifelse(dcnextuse_substance>183 & drug_substance_combo==lead(drug_substance_combo), 1, NA),
			startgap_substance = ifelse(dcprevuse_substance>183 | is.na(dcprevuse_substance), 1, NA),
			dcstop_substance = ifelse(!is.na(stopgap_substance) & stopgap_substance==1 & dcstop_substance==0, 1, dcstop_substance),
			dcstart_substance = ifelse(!is.na(startgap_substance) & startgap_substance==1 & dcstart_substance==0, 1, dcstart_substance)
		) %>%
		ungroup()

# Add in time to last prescription date for each patient (any drug class)
all_scripts <- all_scripts %>%
		group_by(serialno) %>%
		mutate(
			timetolastpx = difftime(max(date, na.rm = TRUE), date, units = "days")
		) %>%
		ungroup()

#all_scripts %>% nrow()
#2301488

save(all_scripts, file = "/home/pcardoso/workspace/SDRN-Cohort-scripts/Interim_Datasets/mm_all_scripts.RData")


##################################################################

# drug_class_start_stop = 1 line per serialno / drug class instance (continuous period of drug use)
## Below section is identical to this but uses drug substance

# Just keep dates where a drug is started or stopped
## Pull out start and stop dates on rows where dstart==1
## Each row either has both start and stop date, or start on one date and next date for same serialno and drug class will be stop date
drug_class_start_stop <- all_scripts_long %>%
		filter(dstart_class==1 | dstop_class==1) %>%
		group_by(serialno, drug_class) %>%
		arrange(date) %>%
		mutate(
			dstartdate_class = ifelse(dstart_class==1, date, as.Date(NA)),
			dstartdate_class = as.Date(dstartdate_class, origin = "1970-01-01"),
			dstopdate_class = ifelse(dstart_class==1 & dstop_class==1, date,
					ifelse(dstart_class==1 & dstop_class==0, lead(date), as.Date(NA))),
			dstopdate_class = as.Date(dstopdate_class, origin = "1970-01-01")
		) %>%
		ungroup()

# Just keep rows where dstart==1 - 1 row per drug class instance, and only keep variables which apply to whole instance, not those relating to specific scripts within the instance
drug_class_start_stop <- drug_class_start_stop %>%
		filter(dstart_class==1) %>%
		select(serialno, drug_class, dstartdate_class, dstopdate_class)

#drug_class_start_stop %>% nrow()
#1207294

# Add drug order count within each serialno: how many periods of medication have they had
## If multiple meds started on same day, use minimum for both/all drugs
drug_class_start_stop <- drug_class_start_stop %>%
		group_by(serialno) %>%
		arrange(dstartdate_class) %>%
		mutate(drug_order = row_number()) %>%
		ungroup() %>%
		group_by(serialno, dstartdate_class) %>%
		mutate(drug_order = min(drug_order, na.rm = TRUE)) %>%
		ungroup()

# Add drug instance count for each serialno / drug class instance e.g. if several periods of MFN usage, these should be labelled 1, 2 etc. based on start date
drug_class_start_stop <- drug_class_start_stop %>%
		group_by(serialno, drug_class) %>%
		arrange(dstartdate_class) %>%
		mutate(drug_instance=row_number()) %>%
		ungroup()

# Add drug line for each patid / drug class: on first usage of this drug, how many previous distinct rug classes had been used + 1
## If multiple meds started on same day, use minimum for both/all drugs
drug_line <- drug_class_start_stop %>%
		filter(drug_instance == 1) %>%
		group_by(serialno) %>%
		arrange(dstartdate_class) %>%
		mutate(drugline_all = row_number()) %>%
		ungroup() %>%
		
		group_by(serialno, dstartdate_class) %>%
		mutate(drugline_all = min(drugline_all, na.rm=TRUE)) %>%
		ungroup() %>%
		
		select(serialno, drug_class, drugline_all)

drug_class_start_stop <- drug_class_start_stop %>%
		inner_join(drug_line, by = c("serialno", "drug_class"))

#drug_class_start_stop %>% nrow()
#1207294


save(drug_class_start_stop, file = "/home/pcardoso/workspace/SDRN-Cohort-scripts/Interim_Datasets/mm_drug_class_start_stop.RData")


##################################################################

# drug_substance_start_stop = 1 line per patid / drug substance instance (continuous period of drug use)
# Identical to above code - except uses substance rather than class and doesn't calculate drug_order, drug_instance and drug_line

# Just keep dates where a drug is started or stopped
## Pull out start and stop dates on rows where dstart == 1
## Each row either has both start and stop date, or start on one date and next date for same serialno and drug class will be stop date
drug_substance_start_stop <- all_scripts_long %>%
		filter(dstart_substance==1 | dstop_substance==1) %>%
		group_by(serialno, drug_substance) %>%
		arrange(date) %>%
		mutate(
			dstartdate_substance = ifelse(dstart_substance==1, date, as.Date(NA)),
			dstartdate_substance = as.Date(dstartdate_substance, origin = "1970-01-01"),
			dstopdate_substance = ifelse(dstart_substance==1 & dstop_substance == 1, date,
					ifelse(dstart_substance==1 & dstop_substance==0, lead(date), as.Date(NA))),
			dstopdate_substance = as.Date(dstopdate_substance, origin = "1970-01-01")
		) %>%
		ungroup()

# Just keep rows where dstart==1 - 1 row per drug class instance, and only keep variable which apply to the whole instance, not those relating to specific scripts within the instance
drug_substance_start_stop <- drug_substance_start_stop %>%
		filter(dstart_substance==1) %>%
		select(serialno, drug_class, drug_substance, dstartdate_substance, dstopdate_substance)

#drug_substance_start_stop %>% nrow()
#1262658

save(drug_substance_start_stop, file = "/home/pcardoso/workspace/SDRN-Cohort-scripts/Interim_Datasets/mm_drug_substance_start_stop.RData")


##################################################################

# Combine drug_start_stop tables
## Copy down class-specific variables (drug_order, drug_instance, drugline_all) to all drug substance starts within this drug class period
drug_start_stop <- drug_substance_start_stop %>%
		left_join(drug_class_start_stop, by = c("serialno", "drug_class", c("dstartdate_substance" = "dstartdate_class"))) %>%
		mutate(
			drug_class_start = ifelse(!is.na(dstopdate_class), 1, 0)
		) %>%
		group_by(serialno, drug_class) %>%
		arrange(dstartdate_substance) %>%
		mutate(
			drug_class_starts = cumsum(drug_class_start==1)
		) %>%
		arrange() %>%
		ungroup() %>%
		group_by(serialno, drug_class, drug_class_starts) %>%
		mutate(
			drug_order = max(drug_order, na.rm = TRUE),
			drug_instance = max(drug_instance, na.rm = TRUE),
			drugline_all = max(drugline_all, na.rm = TRUE),
			dstopdate_class = max(dstopdate_class, na.rm = TRUE)
		) %>%
		ungroup() %>%
		select(serialno, dstartdate=dstartdate_substance, drug_class_start, drug_class, dstopdate_class, drug_order, drug_instance, drugline_all, drug_substance, dstopdate_substance)


## Define different substance periods within same drug class period (patid - drug_class - drug_order defines unique drug class period)
# 0 - only substance in drug class period (NA: if full drug class period is 1 script only, substance_status will still be 0
# 1 - multiple substances within drug class period, this one covers full drug class period (NA: if full drug class period is 1 script only, substance_status will be 4 not 1)
# 2 - multuple substances within drug class period, this one is started at start of drug class period but stops before end of drug class period
# 3 - multiple substances within drug class period, this one is during drug class period
# 4 - not codded here

drug_start_stop <- drug_start_stop %>%
		group_by(serialno, drug_class, drug_order) %>%
		mutate(total_substance_count = n()) %>%
		ungroup() %>%
		mutate(
			substance_status = ifelse(total_substance_count == 1, 0,
					ifelse(total_substance_count>1 & drug_class_start==1 & dstopdate_substance==dstopdate_class, 1,
							ifelse(total_substance_count>1 & drug_class_start==1 & dstopdate_substance<dstopdate_class, 2,
									ifelse(total_substance_count>1 & drug_class_start==0, 3, NA))))
		) %>%
		
		group_by(serialno, drug_class, drug_order, substance_status) %>%
		arrange(drug_substance) %>%
		mutate(
			substance_status_count=n(),
			substance_order=row_number()
		) %>%
		arrange() %>%
		ungroup() %>%
		
		mutate(
			substance_status = ifelse(substance_status_count==1, substance_status,
					ifelse(substance_status_count>1 & substance_order==1, paste0(substance_status, "a"),
							ifelse(substance_status_count>1 & substance_order==2, paste0(substance_status, "b"),
									ifelse(substance_status_count>1 & substance_order==3, paste0(substance_status, "c"),
											ifelse(substance_status_count>1 & substance_order==4, paste0(substance_status, "d"),
													ifelse(substance_status_count>1 & substance_order==5, paste0(substance_status, "e"),
															ifelse(substance_status_count>1 & substance_order==6, paste0(substance_status, "f"),
																	ifelse(substance_status_count>1 & substance_order==7, paste0(substance_status, "g"),
																			ifelse(substance_status_count>1 & substance_order==8, paste0(substance_status, "h"),
																					ifelse(substance_status_count>1 & substance_order==9, paste0(substance_status, "i"),
																							ifelse(substance_status_count>1 & substance_order==10, paste0(substance_status, "j"), NA)))))))))))
		) %>%
		select(serialno, dstartdate, drug_class_start, drug_class, dstopdate_class, drug_order, drug_instance, drugline_all, drug_substance, substance_status, dstopdate_substance)

#drug_start_stop %>% nrow()
#1262658


save(drug_start_stop, file = "/home/pcardoso/workspace/SDRN-Cohort-scripts/Interim_Datasets/mm_drug_start_stop.RData")



##################################################################

# combo_class_start_stop = 1 line per patid / drug combo instance (continuous period of drug combo use) BASED ON DRUG CLASSES
# Next section of code is similar, but based on drug substances

# Similar process to that for drug classes above:
## Just keep dates where a drug is started or stopped
## Pull out start and stop dates on rows where dcstart==1
## Each row either has both start and stop date, or start on one date and next date for same patid will be stop date (as can only be on one drug combo at once)

combo_class_start_stop <- all_scripts %>%
		filter(dcstart_class==1 | dcstop_class==1) %>%
		group_by(serialno) %>%
		arrange(date) %>%
		mutate(
			dcstartdate = ifelse(dcstart_class==1, date, as.Date(NA)),
			dcstartdate = as.Date(dcstartdate, origin = "1970-01-01"),
			dcstopdate = ifelse(dcstart_class==1 & dcstop_class==1, date,
					ifelse(dcstart_class==1 & dcstop_class==0, lead(date), as.Date(NA))),
			dcstopdate = as.Date(dcstopdate, origin = "1970-01-01")
		) %>%
		ungroup()

# Just leep rows where dcstart==1 - 1 row per drug combo instance, and only keep variables which apply to whole instance

combo_class_start_stop <- combo_class_start_stop %>%
		filter(dcstart_class==1) %>%
		select(serialno, drug_class_combo, numdrugs_class, dcstartdate, dcstopdate, all_of(drugclasses))

#combo_class_start_stop %>% nrow()
#1903557

# Add drugcomboorder count within each serialno: how many periods of medication have they had
## Also add nextdcdate: date next combination started (use stop date if last combination before end of predictions)

combo_class_start_stop <- combo_class_start_stop %>%
		group_by(serialno) %>%
		arrange(dcstartdate) %>%
		mutate(
			drugcomboorder=row_number(),
			nextdcdate = ifelse(is.na(lead(dcstartdate)), dcstopdate, lead(dcstartdate)),
			nextdcdate = as.Date(nextdcdate, origin = "1970-01-01")
		) %>%
		ungroup()

# Define what current and next drug combination represents in terms of adding/removing/swapping

combo_class_start_stop <- combo_class_start_stop %>%
		mutate(
			add = 0,
			adddrug = as.character(NA),
			rem = 0,
			remdrug = as.character(NA),
			nextadd = 0,
			nextadddrug = as.character(NA),
			nextrem = 0,
			nextremdrug = as.character(NA)
		)

for (drug_col in drugclasses) {
	
	combo_class_start_stop <- combo_class_start_stop %>%
			group_by(serialno) %>%
			arrange(drugcomboorder) %>%
			mutate(
					add = ifelse(!!as.name(drug_col)==TRUE & !is.na(lag(!!as.name(drug_col))) & lag(!!as.name(drug_col))==FALSE, add+1L, add),
					adddrug = ifelse(!!as.name(drug_col)==TRUE & !is.na(lag(!!as.name(drug_col))) & lag(!!as.name(drug_col))==FALSE,
							ifelse(is.na(adddrug), drug_col, paste(adddrug, "&", drug_col)), adddrug),
					
					rem = ifelse(!!as.name(drug_col)==FALSE & !is.na(lag(!!as.name(drug_col))) & lag(!!as.name(drug_col))==TRUE, rem+1L, rem),
					remdrug = ifelse(!!as.name(drug_col)==FALSE & !is.na(lag(!!as.name(drug_col))) & lag(!!as.name(drug_col))==TRUE,
							ifelse(is.na(remdrug), drug_col, paste(remdrug, "&", drug_col)), remdrug),
					
					nextrem = ifelse(!!as.name(drug_col)==TRUE & !is.na(lead(!!as.name(drug_col))) & lead(!!as.name(drug_col))==FALSE, nextrem+1L, nextrem),
					nextremdrug = ifelse(!!as.name(drug_col)==TRUE & !is.na(lead(!!as.name(drug_col))) & lead(!!as.name(drug_col))==FALSE,
							ifelse(is.na(nextremdrug), drug_col, paste(nextremdrug, "&", drug_col)), nextremdrug),
					
					nextadd = ifelse(!!as.name(drug_col)==FALSE & !is.na(lead(!!as.name(drug_col))) & lead(!!as.name(drug_col))==TRUE, nextadd+1L, nextadd),
					nextadddrug = ifelse(!!as.name(drug_col)==FALSE & !is.na(lead(!!as.name(drug_col))) & lead(!!as.name(drug_col))==TRUE,
							ifelse(is.na(nextadddrug), drug_col, paste(nextadddrug, "&", drug_col)), nextadddrug)
			) %>%
			ungroup()
	
}


combo_class_start_stop <- combo_class_start_stop %>%
		
		mutate(
			swap = add>=1 & rem >=1,
			nextswap = nextadd>=1 & nextrem>=1,
			
			drugchange = case_when(
				add>=1 & rem==0 ~ "add",
				add==0 & rem>=1 ~ "remove",
				add>=1 & rem>=1 ~ "swap",
				add==0 & rem==0 & drugcomboorder==1 ~ "start of px",
				add==0 & rem==0 & drugcomboorder!=1 ~ "stop - break"
			),
			
			nextdrugchange = case_when(
				nextadd>=1 & nextrem==0 ~ "add",
				nextadd==0 & nextrem>=1 ~ "remove",
				nextadd>=1 & nextrem>=1 ~ "swap",
				nextadd==0 & nextrem==0 & nextdcdate!=dcstopdate ~ "stop - break",
				nextadd==0 & nextrem==0 & nextdcdate==dcstopdate ~ "stop - end of px"
			)
		) %>%
		mutate_if(is.logical, as.numeric)

# Add date of next drug combination (if last combination or before break, use stop date of current combination), date when a different drug class added or removed, and date when previous combination first prescribed

combo_class_start_stop <- combo_class_start_stop %>%
		group_by(serialno) %>%
		arrange(dcstartdate) %>%
		mutate(
			datechange_class = ifelse(is.na(lead(dcstartdate)) | difftime(lead(dcstartdate), dcstopdate, units = "days")>183, dcstopdate, lead(dcstartdate)),
			datechange_class = as.Date(datechange_class, origin = "1970-01-01"),
			dateaddrem_class = ifelse(is.na(lead(dcstartdate)), NA, lead(dcstartdate)),
			dateaddrem_class = as.Date(dateaddrem_class, origin = "1970-01-01"),
			dateprevcombo_class = lag(dcstartdate),
			dateprevcombo_class = as.Date(dateprevcombo_class, origin = "1970-01-01")
		) %>%
		ungroup()

# Add variable to indicate whether multiple drug classes started on the same day, plus timetochange, timetoaddrem and timeprevcombo variables

combo_class_start_stop <- combo_class_start_stop %>%
		mutate(
			multi_drug_start_class = ifelse(add>1 | (drugcomboorder==1 & numdrugs_class>1), 1, 0),
			timetochange_class = difftime(datechange_class, dcstartdate, units = "days"),
			timetoaddrem_class = difftime(dateaddrem_class, dcstartdate, units = "days"),
			timeprevcombo_class = difftime(dcstartdate, dateprevcombo_class)
		)

#combo_class_start_stop %>% nrow()
#1903557

save(combo_class_start_stop, file = "/home/pcardoso/workspace/SDRN-Cohort-scripts/Interim_Datasets/mm_combo_class_start_stop.RData")



##################################################################

# combo_substance_start_stop = 1 line per patid / drug combo instance (continuous period of drug combo use) BASED ON DRUG SUBSTANCES
# previous section of code is similar, but based on drug substances

# Similar process to that for drug classes above:
## Just keep dates where a drug is started or stopped
## Pull out start and stop dates on rows where dcstart==1
## Each row either has both start and stop date, or start on one date and next date for same patid will be stop date (as can only be on one drug combo at once)

combo_substance_start_stop <- all_scripts %>%
		filter(dcstart_substance==1 | dcstop_substance==1) %>%
		group_by(serialno) %>%
		arrange(date) %>%
		mutate(
				dcstartdate = ifelse(dcstart_substance==1, date, as.Date(NA)),
				dcstartdate = as.Date(dcstartdate, origin = "1970-01-01"),
				dcstopdate = ifelse(dcstart_substance==1 & dcstop_substance==1, date,
						ifelse(dcstart_substance==1 & dcstop_substance==0, lead(date), as.Date(NA))),
				dcstopdate = as.Date(dcstopdate, origin = "1970-01-01")
		) %>%
		ungroup()

# Just leep rows where dcstart==1 - 1 row per drug combo instance, and only keep variables which apply to whole instance

combo_substance_start_stop <- combo_substance_start_stop %>%
		filter(dcstart_substance==1) %>%
		select(serialno, drug_substance_combo, numdrugs_substance, dcstartdate, dcstopdate, all_of(drugsubstances))

#combo_substance_start_stop %>% nrow()
#1930972

# Add drugcomboorder count within each serialno: how many periods of medication have they had
## Also add nextdcdate: date next combination started (use stop date if last combination before end of predictions)

combo_substance_start_stop <- combo_substance_start_stop %>%
		group_by(serialno) %>%
		arrange(dcstartdate) %>%
		mutate(
				drugcomboorder=row_number(),
				nextdcdate = ifelse(is.na(lead(dcstartdate)), dcstopdate, lead(dcstartdate)),
				nextdcdate = as.Date(nextdcdate, origin = "1970-01-01")
		) %>%
		ungroup()

# Define what current and next drug combination represents in terms of adding/removing/swapping

combo_substance_start_stop <- combo_substance_start_stop %>%
		mutate(
				add = 0,
				adddrug = as.character(NA),
				rem = 0,
				remdrug = as.character(NA),
				nextadd = 0,
				nextadddrug = as.character(NA),
				nextrem = 0,
				nextremdrug = as.character(NA)
		)

for (drug_col in drugsubstances) {
	
	combo_substance_start_stop <- combo_substance_start_stop %>%
			group_by(serialno) %>%
			arrange(drugcomboorder) %>%
			mutate(
					add = ifelse(!!as.name(drug_col)==TRUE & !is.na(lag(!!as.name(drug_col))) & lag(!!as.name(drug_col))==FALSE, add+1L, add),
					adddrug = ifelse(!!as.name(drug_col)==TRUE & !is.na(lag(!!as.name(drug_col))) & lag(!!as.name(drug_col))==FALSE,
							ifelse(is.na(adddrug), drug_col, paste(adddrug, "&", drug_col)), adddrug),
					
					rem = ifelse(!!as.name(drug_col)==FALSE & !is.na(lag(!!as.name(drug_col))) & lag(!!as.name(drug_col))==TRUE, rem+1L, rem),
					remdrug = ifelse(!!as.name(drug_col)==FALSE & !is.na(lag(!!as.name(drug_col))) & lag(!!as.name(drug_col))==TRUE,
							ifelse(is.na(remdrug), drug_col, paste(remdrug, "&", drug_col)), remdrug),
					
					nextrem = ifelse(!!as.name(drug_col)==TRUE & !is.na(lead(!!as.name(drug_col))) & lead(!!as.name(drug_col))==FALSE, nextrem+1L, nextrem),
					nextremdrug = ifelse(!!as.name(drug_col)==TRUE & !is.na(lead(!!as.name(drug_col))) & lead(!!as.name(drug_col))==FALSE,
							ifelse(is.na(nextremdrug), drug_col, paste(nextremdrug, "&", drug_col)), nextremdrug),
					
					nextadd = ifelse(!!as.name(drug_col)==FALSE & !is.na(lead(!!as.name(drug_col))) & lead(!!as.name(drug_col))==TRUE, nextadd+1L, nextadd),
					nextadddrug = ifelse(!!as.name(drug_col)==FALSE & !is.na(lead(!!as.name(drug_col))) & lead(!!as.name(drug_col))==TRUE,
							ifelse(is.na(nextadddrug), drug_col, paste(nextadddrug, "&", drug_col)), nextadddrug)
			) %>%
			ungroup()
	
}
	

combo_substance_start_stop <- combo_substance_start_stop %>%
		
		mutate(
				swap = add>=1 & rem >=1,
				nextswap = nextadd>=1 & nextrem>=1,
				
				drugchange = case_when(
						add>=1 & rem==0 ~ "add",
						add==0 & rem>=1 ~ "remove",
						add>=1 & rem>=1 ~ "swap",
						add==0 & rem==0 & drugcomboorder==1 ~ "start of px",
						add==0 & rem==0 & drugcomboorder!=1 ~ "stop - break"
				),
				
				nextdrugchange = case_when(
						nextadd>=1 & nextrem==0 ~ "add",
						nextadd==0 & nextrem>=1 ~ "remove",
						nextadd>=1 & nextrem>=1 ~ "swap",
						nextadd==0 & nextrem==0 & nextdcdate!=dcstopdate ~ "stop - break",
						nextadd==0 & nextrem==0 & nextdcdate==dcstopdate ~ "stop - end of px"
				)
		) %>%
		mutate_if(is.logical, as.numeric)

# Add date of next drug combination (if last combination or before break, use stop date of current combination), date when a different drug substance added or removed, and date when previous combination first prescribed

combo_substance_start_stop <- combo_substance_start_stop %>%
		group_by(serialno) %>%
		arrange(dcstartdate) %>%
		mutate(
				datechange_substance = ifelse(is.na(lead(dcstartdate)) | difftime(lead(dcstartdate), dcstopdate, units = "days")>183, dcstopdate, lead(dcstartdate)),
				datechange_substance = as.Date(datechange_substance, origin = "1970-01-01"),
				dateaddrem_substance = ifelse(is.na(lead(dcstartdate)), NA, lead(dcstartdate)),
				dateaddrem_substance = as.Date(dateaddrem_substance, origin = "1970-01-01"),
				dateprevcombo_substance = lag(dcstartdate),
				dateprevcombo_substance = as.Date(dateprevcombo_substance, origin = "1970-01-01")
		) %>%
		ungroup()

# Add variable to indicate whether multiple drug substances started on the same day, plus timetochange, timetoaddrem and timeprevcombo variables

combo_substance_start_stop <- combo_substance_start_stop %>%
		mutate(
				multi_drug_start_substance = ifelse(add>1 | (drugcomboorder==1 & numdrugs_substance>1), 1, 0),
				timetochange_substance = difftime(datechange_substance, dcstartdate, units = "days"),
				timetoaddrem_substance = difftime(dateaddrem_substance, dcstartdate, units = "days"),
				timeprevcombo_substance = difftime(dcstartdate, dateprevcombo_substance)
		)

#combo_substance_start_stop %>% nrow()
#1930972

save(combo_substance_start_stop, file = "/home/pcardoso/workspace/SDRN-Cohort-scripts/Interim_Datasets/mm_combo_substance_start_stop.RData")



##################################################################

# Combine combo_start_stop tables
## Only include variables which we are currently using and binary drug class / drug substnance variables (Acarbose columsn from two tables are identical: remove from drug class table)
## Add ncurrtx variable: number of distinct major drug CLASSES (DPP4, GLP1, INS, MFN, SGLT2, SU, TZD), not including one being initiated (i.e. DPP4+GLP1+INS+MFN+SGLT2+SU+TZD
## If rows are only present when considering drug substance changes, ncurrtx is missing (NA)

## Copy down and recalculate timetochange_class, timetoaddrem_class and timeprevcombo_class for all rows

combo_start_stop <- combo_substance_start_stop %>%
		select(serialno, dcstartdate, dcstopdate_substance=dcstopdate, drug_substance_combo, datechange_substance, dateaddrem_substance, dateprevcombo_substance, multi_drug_start_substance, all_of(drugsubstances), timetochange_substance, timetoaddrem_substance, timeprevcombo_substance) %>%
		left_join(
			combo_class_start_stop %>%
					select(serialno, dcstartdate, dcstopdate_class=dcstopdate, drug_class_combo, datechange_class, dateaddrem_class, dateprevcombo_class, multi_drug_start_class, all_of(drugclasses), timetochange_class, timetoaddrem_class, timeprevcombo_class),
			by = c("serialno", "dcstartdate")
		) %>%
		mutate(
			drug_class_combo_change = ifelse(!is.na(dcstopdate_class), 1, 0)
		) %>%
		
		group_by(serialno) %>%
		arrange(dcstartdate) %>%
		mutate(
			drug_class_changes = cumsum(drug_class_combo_change==1)
		) %>%
		arrange() %>%
		ungroup()

combo_start_stop <- combo_start_stop %>%
		group_by(serialno, drug_class_changes) %>%
		mutate(
			datechange_class = as.Date(datechange_class, origin = "1970-01-01"),
			dateaddrem_class = as.Date(dateaddrem_class, origin = "1970-01-01"),
			dateprevcombo_class = as.Date(dateprevcombo_class, origin = "1970-01-01"),
			
			DPP4 = max(DPP4, na.rm = TRUE),
			GLP1 = max(GLP1, na.rm = TRUE),
			INS = max(INS, na.rm = TRUE),
			MFN = max(MFN, na.rm = TRUE),
			SGLT2 = max(SGLT2, na.rm = TRUE),
			SU = max(SU, na.rm = TRUE),
			TZD = max(TZD, na.rm = TRUE)
		) %>%
		
		ungroup() %>%
		
		mutate(
			timetochange_class = difftime(datechange_class, dcstartdate, units = "days"),
			timetoaddrem_class = difftime(dateaddrem_class, dcstartdate, units = "days"),
			timeprevcombo_class = difftime(dcstartdate, dateprevcombo_class, units = "days")
		) %>%
		
		select(serialno, dcstartdate, dcstopdate_class, drug_class_combo, timetochange_class, timetoaddrem_class, timeprevcombo_class, multi_drug_start_class, all_of(drugclasses), dcstopdate_substance, drug_substance_combo, timetochange_substance, timetoaddrem_substance, timeprevcombo_substance, multi_drug_start_substance, all_of(drugsubstances)) %>%
		mutate(ncurrtx = DPP4+GLP1+INS+MFN+SGLT2+SU+TZD-1)

#combo_start_stop %>% nrow()
#1930972

save(combo_start_stop, file = "/home/pcardoso/workspace/SDRN-Cohort-scripts/Interim_Datasets/mm_combo_start_stop.RData")




