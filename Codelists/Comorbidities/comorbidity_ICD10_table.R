# TODO: Add comment
# 
# Author: pcardoso
###############################################################################


### connection to database
#con <- dbConn("NDS_2023")
### Check conditions
#### Table of conditions concept list
#test_concept <- dbGetQueryMap(con, "SELECT * FROM o_concept_condition")
##### Example for angina
#comorbidity_code_example = "I21"
#test <- dbGetQueryMap(con, paste0("SELECT * FROM o_condition WHERE o_condition.condition_code = '", comorbidity_code_example, "'"))
#
## specify the type of code being asked
#test <- dbGetQueryMap(con, paste0("SELECT o_condition.*, o_concept_condition.name FROM o_condition, o_concept_condition WHERE 
#			o_condition.condition_code = 'R29.6' AND o_concept_condition.name = 'icd10' AND o_condition.concept_id = o_concept_condition.uid"))


## load librarires
library(tidyverse)

comorbidity_ICD10_table <- list(
		"acutepancreatitis" = c("K85", "B26.3", "B25.2"),
		"af" = c("I48"),
		"angina" = c("I20.0", "I20.1", "I20.8", "I20.9"),
		"anxiety_disorders" = c("F40.0", "F40.1", "F40.8", "F40.9", "F41"),
		"asthma" = c("J45", "J46"),
		"benignprostatehyperplasia" = c("N40"),
		"bronchiectasis" = c("J47", "Q33.4"),
		"chronicpancreatitis" = c("K86.0", "K86.1", "k86.3"),
		"ckd5_code" = c("N16.5", "N18.0", "N18.5", "T86.1", "Z49.0", "Z49.2", "Z94.0"),
		"cld" = c("B15.0", "B16.0", "B19.0", "B18", "K70", "K71.1", "K71.7", "K72", "K74", "K75.4", "K75.8", "K76.0", "K76.2", "K76.3"),
		"copd" = c("J41", "J42", "J43", "J44"),
		"cv_death" = c("I"),
		"cysticfibrosis" = c("E84"),
		"dementia" = c("F00", "F01", "F03", "F05.1", "G30", "F02"),
		"diabeticnephropathy" = c("E10.2", "E11.2", "E12.2", "E13.2", "E14.2", "R80.9"),
		"dka" = c("E10.1", "E11.1", "E12.1", "E13.1", "E14.1"),
		"falls" = c("W00", "W01", "W03", "W04", "W05", "W06", "W07", "W08", "W10", "W17", "W18", "W19", "R29.6"),
		"genital_infection" = c("A18.1", "A54.0", "A56.0", "A59.0", "A60.0", "B37.3", "B37.4", "N51", "N76.0", "N76.1", "N76.2", "N76.3", "N77.1", "O23.5", "O86.1"),
		"genital_infection_nonspec" = c("B37.9"),
#		"fh_diabetes" = c(""),
#		"fh_premature_cvd" = c(""),
#		"frailty_simple" = c(),
		"haem_cancer" = c("C81", "C82", "C83", "C84", "C85", "C88", "C90", "C91", "C92", "C93", "C94", "C95", "C96"),
		"haemochromatosis" = c("E83.1"),
		"haemorrhagicstroke" = c("I60", "I61", "I62.9", "I69.0", "I69.1", "I69.2", "S06.6"),
		"heartfailure" = c("I11.0", "I13.0", "I13.2", "I50"),
#		"hosp_cause_majoramputation" = c(""),
#		"hosp_cause_minoramputation" = c(""),
		"hypertension" = c("I10", "I11", "I12", "I13", "I15", "I67.4"),
		"hypoglycaemia" = c("I16.0", "I16.1", "I16.2"),
		"ihd" = c("I24.0", "I24.8", "I24.9", "I25.0", "I25.1", "I25.3", "I25.4", "I25.5", "I25.8", "I25.9"), #ischaemic heart disease
		"incident_mi" = c("I21"),
		"incident_stroke" = c("I63", "I64"),
		"ischaemicstroke" = c("I63", "I64", "G46", "I69.3", "I69.4", "I69.8", "O22.5", "O87.3"),
		"kf_death" = c("N18.0", "N18.5", "N18.8", "N18.9", "N19", "I12.0", "I13.1", "I13.2"),
		"lowerlimbfracture" = c("T12", "T02.5", "T02.6", "S72", "S72.0", "S72.1", "S72.2", "S72.3", "S72.4", "S72.7", "S72.8", "S72.9", "S82", "S82.0", "S82.1", "S82.2", "S82.3", "S82.4", "S82.5", "S82.6", "S82.7", "S82.8", "S82.9", "S92.1"),
		"micturition_control" = c("N39.3", "N39.4", "R32", "F98.0"),
		"myocardialinfarction" = c("I21", "I22", "I23", "I24.1", "I25.2", "I25.6"),
		"neuropathy" = c("E10.4", "E11.4", "E12.4", "E13.4", "E14.4", "G58.9", "G59.0", "G62.9", "G63.2"),
#		"osteoporosis" = c(""),
		"otherneuroconditions" = c("A81.0", "A81.1", "A81.2", "F02.3", "G10", "G12.2", "G20", "G35", "G70.0", "G71.0", "G80", "G81", "G82"),
		"pad" = c("I70.2", "I70.3", "I72.4", "I73", "I74.3", "I74.4", "I74.5"), #peripheral arterial disease
		"pancreaticcancer" = c("C25"),
#		"photocoagulation" = c(""),
		"pulmonaryfibrosis" = c("J84.1"),
		"pulmonaryhypertension" = c("I27.0", "I27.2"),
		"respiratoryinfection" = c("J00", "J01", "J02", "J03", "J04", "J05", "J06", "J09", "J10", "J11", "J12", "J13", "J14", "J15", "J16", "J17", "J18", "J20", "J21", "J22"),
		"retinopathy" = c("E10.3", "E11.3", "E12.3", "E13.3", "E14.3", "H35.0", "H35.1", "H35.2", "H35.3", "H35.4", "H35.5", "H35.6", "H35.7", "H35.8", "H35.9", "H36.0"),
#		"revasc" = c(""), #revascularisation procedure
		"rheumatoidarthritis" = c("M05.0", "M05.1", "M05.2", "M05.3", "M05.8", "M05.9", "M06.0", "M06.8", "M06.9"),
		"solid_cancer" = c("C00", "C01", "C02", "C03", "C04", "C05", "C06", "C07", "C08", "C09",
				"C10", "C11", "C12", "C13", "C14", "C15", "C16", "C17", "C18", "C19",
				"C20", "C21", "C22", "C23", "C24", "C25", "C26",
				"C30", "C31", "C32", "C33", "C34", "C37", "C38", "C39",
				"C40", "C41", "C42", "C43", "C45", "C46", "C47", "C48", "C49",
				"C50", "C51", "C52", "C53", "C54", "C55", "C56", "C57", "C58",
				"C60", "C61", "C62", "C63", "C64", "C65", "C66", "C67", "C68", "C69",
				"C70", "C71", "C72", "C73", "C74", "C75"),
#		"solidorgantransplant" = c(""),
		"stroke" = c("G46.3", "G46.4", "G46.5", "G46.6", "G46.7", "G46.8", "I61", "I63", "I64", "I69.1", "I69.3", "I69.4"),
		"tia" = c("G45.0", "G45.1", "G45.2", "G45.3", "G45.8", "G45.9", "G46.0", "G46.1", "G46.2", "I65", "I66"), #transient ischaemic attack
		"unstableangina" = c("I20.0"),
		"urinary_frequency" = c("R35"),
		"vitreoushemorrhage" = c("H43.1"),
		"volume_depletion" = c("E86")
	)

	
saveRDS(comorbidity_ICD10_table, "/home/pcardoso/workspace/SDRN-Cohort-scripts/Codelists/Comorbidities/comorbidity_ICD10_table.rds")
	
