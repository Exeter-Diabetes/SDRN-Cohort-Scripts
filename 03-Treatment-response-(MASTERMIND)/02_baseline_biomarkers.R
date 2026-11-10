# Author: pcardoso
###############################################################################

# Collect baseline biomarkers for combos

###############################################################################

rm(list=ls())

# load libraries
library(tidyverse)
library(data.table)


###############################################################################

## connection to database: only opened if a cleaned biomarker file is missing
## (03_response_biomarkers.R saves the same cleaned tables as mm_clean_{biomarker}_medcodes.RData; if they exist they are used,
## so this script can run without database access. Delete them to read the database again, e.g. after a data refresh)
con <- NULL

###############################################################################

# Define biomarkers
## Keep HbA1c separate as processed differently
## Missing: haematocrit, albumin_urine, creatinine_urine, acr_from_separate, albumin_blood

## Data has been cleaned by SDRN, but still has weird values sometimes


biomarkers <- c("weight", "height", "bmi", "fastingglucose", "hdl", "triglyceride", "creatinine_blood", "ldl", "alt", "ast", "totalcholesterol", "dbp", "sbp", "acr", "bilirubin", "haemoglobin", "pcr")

concept_observation <- c("body_mass-weight", "body_mass-height", "body_mass-bmi", "biochem-bglu_fasting", "biochem-lipid-hdl", "biochem-lipid-trig", "biochem-creatinine", "biochem-lipid-ldl", "biochem-other-alt", "biochem-other-ast", "biochem-lipid-tchol", "blood_pressure-dbp", "blood_pressure-sbp", "biochem-albumin-ratio", "biochem-other-bilirubin", "biochem-other-haemoglobin", "biochem-urine_prot-pcr")

min_limits <- c(40, 0.6, 15, 2.5, 0.2, 0.1, 20, 0.1, 0, 0, 0.5, 20, 40, 0, 1, 30, 0)

max_limits <- c(350, 2.25, 100, 30, 10, 40, 2500, 20, 200, 300, 20, 200, 270, 122, 250, 250, 682)

for (i in 1:length(biomarkers)) {
	
	print(biomarkers[i])
	
	clean_tablename <- paste0("clean_", biomarkers[i], "_medcodes")

	file_name <- paste0("/home/pcardoso/workspace/SDRN-Cohort-scripts/Interim_Datasets/mm_", clean_tablename, ".RData")
	if (file.exists(file_name)) {

		load(file_name)

		## older saved files were made before same-day averaging was added: average again (no change if already averaged)
		data <- get(clean_tablename) %>%
				group_by(serialno, date) %>%
				summarise(testvalue = mean(testvalue, na.rm = TRUE), .groups = "drop")

		assign(clean_tablename, data)
		rm(data)
		gc()

		next
	}

	if (is.null(con)) con <- dbConn("NDS_2024")

	mysqlquery <- paste0("
					SELECT o_observation.* 
					FROM o_observation, o_concept_observation 
					WHERE
					o_observation.concept_id = o_concept_observation.uid AND 
					o_concept_observation.path = '", concept_observation[i], "'")
	
	data <- dbGetQueryMap(con, mysqlquery) %>%
			select(serialno, date, "testvalue" = num.value) %>%
			drop_na(testvalue) %>%
			filter(testvalue >= min_limits[i] & testvalue <= max_limits[i]) %>%
			## as CPRD: average multiple readings on the same day
			group_by(serialno, date) %>%
			summarise(testvalue = mean(testvalue, na.rm = TRUE), .groups = "drop")

	assign(clean_tablename, data)
	
}


if (file.exists("/home/pcardoso/workspace/SDRN-Cohort-scripts/Interim_Datasets/mm_clean_hba1c_medcodes.RData")) {
	## same cleaning as below (saved by 03_response_biomarkers.R)
	load("/home/pcardoso/workspace/SDRN-Cohort-scripts/Interim_Datasets/mm_clean_hba1c_medcodes.RData")
} else {

if (is.null(con)) con <- dbConn("NDS_2024")

clean_hba1c_medcodes <- dbGetQueryMap(con, "
						SELECT o_observation.*
						FROM o_observation, o_concept_observation
						WHERE
						o_observation.concept_id = o_concept_observation.uid AND
						o_concept_observation.path = 'biochem-hba1c'") %>%
		select(serialno, date, "testvalue" = num.value) %>%
		drop_na(testvalue) %>%
		mutate(testvalue = ifelse(testvalue<20, ((testvalue-2.152)/0.09148), testvalue)) %>%
		filter(testvalue >= 20 & testvalue <= 195) %>%
		group_by(serialno, date) %>%
		summarise(testvalue = mean(testvalue, na.rm = TRUE)) %>%
		ungroup()

}


# Get diabetes cohort
load("/home/pcardoso/workspace/SDRN-Cohort-scripts/Interim_Datasets/all_diabetes_cohort.RData")


clean_egfr_medcodes <- clean_creatinine_blood_medcodes %>%
		inner_join(diabetes_cohort %>%
						select(serialno, dob, gender), by = c("serialno")) %>%
		mutate(
				age_at_creat = as.numeric(difftime(date, dob, units = "days"))/365.25,
				sex = ifelse(gender==1, "male", ifelse(gender==2, "female", NA))
		) %>%
		select(-c(dob, gender)) %>%
		## CKD epi 2021 egfr
		mutate(
				creatinine_mgdl = testvalue*0.0113,
				ckd_epi_2021_egfr = ifelse(creatinine_mgdl<=0.7 & sex=="female", (142 * ((creatinine_mgdl/0.7)^-0.241) * (0.9938^age_at_creat) * 1.012),
						ifelse(creatinine_mgdl>0.7 & sex=="female", (142 * ((creatinine_mgdl/0.7)^-1.2) * (0.9938^age_at_creat) * 1.012),
								ifelse(creatinine_mgdl<=0.9 & sex=="male", (142 * ((creatinine_mgdl/0.9)^-0.302) * (0.9938^age_at_creat)),
										ifelse(creatinine_mgdl>0.9 & sex=="male", (142 * ((creatinine_mgdl/0.9)^-1.2) * (0.9938^age_at_creat)), NA))))
		) %>%
		select(-c(creatinine_mgdl, testvalue, sex, age_at_creat)) %>%
		rename(testvalue = ckd_epi_2021_egfr) %>%
		drop_na(testvalue)


## disconnect from database
if (!is.null(con)) dbDisconnect(con)


biomarkers <- c("egfr", biomarkers)




###############################################################################

# Combine each biomarkers with start dates of all drug periods (not just first instances; with timetochange, timeaddrem and multi_drug_start added from mm_combo_start_stop table)

## Get drug start dates

load("/home/pcardoso/workspace/SDRN-Cohort-scripts/Interim_Datasets/mm_drug_start_stop.RData")

load("/home/pcardoso/workspace/SDRN-Cohort-scripts/Interim_Datasets/mm_combo_start_stop.RData")

drug_start_dates <- drug_start_stop %>%
		left_join(combo_start_stop, by = c("serialno", "dstartdate" = "dcstartdate"))

# ## Merge with biomarkers and calculate date difference between biomarker and drug start date
# for (i in c(biomarkers, "hba1c")) {
	
# 	print(i)
	
# 	clean_tablename <- paste0("clean_", i, "_medcodes")
# 	drug_merge_tablename <- paste0("full_", i, "_drug_merge")
	
# 	data <- get(clean_tablename) %>%
# 			inner_join(drug_start_dates, by = c("serialno")) %>%
# 			mutate(drugdatediff = difftime(date, dstartdate, units = "days"))
	
# 	assign(drug_merge_tablename, data)
	
# }


## Memory-safe join: limit cartesian explosion by only joining rows within +/- 2 years and +7 days of drug start
non_equi_join_biomarkers <- function(clean_df, drug_dates, lower_days = -730L, upper_days = 7L) {

	clean_dt <- as.data.table(clean_df)
	## keep a copy of the real biomarker date: in a data.table non-equi join the join column 'date'
	## is returned with the values of window_start (and 'date.1' with window_end), not the biomarker date
	clean_dt[, obs_date := date]
	drug_dt <- as.data.table(drug_dates)

	drug_dt[, `:=`(
			window_start = dstartdate + lower_days,
			window_end = dstartdate + upper_days
	)]

	out <- clean_dt[drug_dt, on = .(serialno, date >= window_start, date <= window_end), allow.cartesian = TRUE, nomatch = NULL]

	out[, date := obs_date]

	out[, intersect(c("obs_date", "date.1", "window_start", "window_end"), names(out)) := NULL]

	out[, drugdatediff := as.numeric(date - dstartdate)]

	return(out[])
}

for (i in c(biomarkers, "hba1c")) {
	
	print(i)
	
	clean_tablename <- paste0("clean_", i, "_medcodes")
	drug_merge_tablename <- paste0("full_", i, "_drug_merge")
	
	data <- non_equi_join_biomarkers(get(clean_tablename), drug_start_dates)
	
	assign(drug_merge_tablename, data)
	
}


###############################################################################

# Find baseline values
## Within period defined above (-2 years to +7 days for all except heigh - also for main HbA1c variable)
## Then use closest date to drug start date
## May be multiple values; use minimum test result, expect for eGFR - use maximum
## Can get duplicates where person has identical results on the same day/days equidistant from the drug start date - choose first row when ordered by drugdatediff

baseline_biomarkers <- drug_start_stop %>%
		select(serialno, dstartdate, drug_class, drug_substance, drug_instance)

## For all except height: between 2 years prior and 7 days after drug start date
biomarkers_no_height <- setdiff(biomarkers, "height")

biomarkers_no_height <- c(biomarkers_no_height, "hba1c")

for (i in biomarkers_no_height) {
	
	print(i)
	
	drug_merge_tablename <- paste0("full_", i, "_drug_merge")
	interim_baseline_biomarker_table <- paste0("baseline_biomarkers_interim_", i)
	pre_biomarker_variable <- paste0("pre", i)
	pre_biomarker_date_variable <- paste0("pre", i, "date")
	pre_biomarker_drugdiff_variable <- paste0("pre", i, "drugdiff")
	
	data <- get(drug_merge_tablename) %>%
			mutate(drugdatediff = as.numeric(drugdatediff)) %>%
			filter(drugdatediff <=7 & drugdatediff >=-730) %>%
			
			group_by(serialno, dstartdate, drug_substance) %>%
			
			mutate(interim_var = ifelse(drugdatediff<0, 0-drugdatediff, drugdatediff)) %>%  # abs() function did not work due to running out of memory
			
			mutate(min_timediff = min(interim_var, na.rm = TRUE)) %>%
			
			filter(interim_var == min_timediff) %>%
			
			mutate(pre_biomarker = ifelse(i=="egfr", max(testvalue, na.rm = TRUE), min(testvalue, na.rm = TRUE))) %>%
			filter(pre_biomarker==testvalue) %>%
			
			arrange(drugdatediff) %>%
			filter(row_number()==1) %>%
			
			ungroup() %>%
			
			relocate(pre_biomarker, .after = serialno) %>%
			relocate(date, .after = pre_biomarker) %>%
			relocate(drugdatediff, .after = date) %>%
			
			rename(
					{{pre_biomarker_variable}}:=pre_biomarker,
					{{pre_biomarker_date_variable}}:=date,
					{{pre_biomarker_drugdiff_variable}}:=drugdatediff
			) %>%
			select(serialno, dstartdate, drug_substance, {{pre_biomarker_variable}}, {{pre_biomarker_date_variable}}, {{pre_biomarker_drugdiff_variable}})
	
	baseline_biomarkers <- baseline_biomarkers %>%
			left_join(data, by = c("serialno", "dstartdate", "drug_substance"))
	
}


## Height - only keep reading at/post drug start date, and find mean
baseline_height <- full_height_drug_merge %>%
		
		filter(drugdatediff >= 0) %>%
		
		group_by(serialno, dstartdate, drug_substance) %>%
		
		summarise(height = mean(testvalue, na.rm = TRUE)) %>%
		
		ungroup()

baseline_biomarkers <- baseline_biomarkers %>%
		left_join(baseline_height, by = c("serialno", "dstartdate", "drug_substance"))

## HbA1c: have 2 years value from above, now add in between 6 months prior and 7 days after drug start date (for prehba1c) or between 12 months prior and 7 days after (for prehba1c12m)
## Exclude if before timeprevcombo_class for 6 month and 12 month value (not 2 year value)

baseline_hba1c <- full_hba1c_drug_merge %>%
		
		filter(drugdatediff<=7 & drugdatediff>=-366) %>%
		
		group_by(serialno, dstartdate, drug_substance) %>%
		
		mutate(interim_var = ifelse(drugdatediff<0, 0-drugdatediff, drugdatediff)) %>%  # abs() function did not work due to running out of memory
		
		mutate(min_timediff = min(interim_var, na.rm = TRUE)) %>%
		
		filter(interim_var == min_timediff) %>%
		
		mutate(prehba1c12m=min(testvalue, na.rm = TRUE)) %>%
		filter(prehba1c12m==testvalue) %>%
		
		arrange(drugdatediff) %>%
		filter(row_number()==1) %>%
		
		ungroup() %>%
		
		rename(
				prehba1c12mdate=date,
				prehba1c12mdrugdiff=drugdatediff
		) %>%
		
		mutate(
				prehba1c = ifelse(prehba1c12mdrugdiff>=-183, prehba1c12m, NA),
				prehba1cdate = ifelse(prehba1c12mdrugdiff>=-183, prehba1c12mdate, NA),
				prehba1cdrugdiff = ifelse(prehba1c12mdrugdiff>=-183, prehba1c12mdrugdiff, NA)
		) %>%
		
		select(serialno, dstartdate, drug_substance, prehba1c12m, prehba1c12mdate, prehba1c12mdrugdiff, prehba1c, prehba1cdate, prehba1cdrugdiff)

### timeprevcombo_class in combo_start_stop table


load("/home/pcardoso/workspace/SDRN-Cohort-scripts/Interim_Datasets/mm_combo_start_stop.RData")

baseline_hba1c <- baseline_hba1c %>%
		left_join((combo_start_stop %>% select(serialno, dcstartdate, timeprevcombo_class)), by = c("serialno", c("dstartdate" = "dcstartdate"))) %>%
		filter(prehba1c12mdrugdiff>=0 | is.na(timeprevcombo_class) | (!is.na(timeprevcombo_class) & abs(prehba1c12mdrugdiff)<=timeprevcombo_class)) %>%
		select(-timeprevcombo_class)

baseline_biomarkers <- baseline_biomarkers %>%
		rename(
				prehba1c2yrs = prehba1c,
				prehba1c2yrsdate = prehba1cdate,
				prehba1c2yrsdrugdiff = prehba1cdrugdiff
		) %>%
		left_join(baseline_hba1c, by = c("serialno", "dstartdate", "drug_substance")) %>%
		relocate(height, .after=prehba1cdrugdiff)

save(baseline_biomarkers, file = "/home/pcardoso/workspace/SDRN-Cohort-scripts/Interim_Datasets/mm_baseline_biomarkers.RData")


###############################################################################

# GLP1 group-censored variables (_cens): GLP1 only, used for the _cens response variables in 03_response_biomarkers.R
## As CPRD 02_mm_baseline_biomarkers.R (GLP1 _cens section). No database access needed: uses drug_start_stop, combo_start_stop
## and full_hba1c_drug_merge already in memory
## Variables above only see changes of drug CLASS, so a switch between two GLP1s (e.g. liraglutide -> semaglutide) is not seen
## For the _cens variables, each group of similar GLP1s is treated as if it were its own drug class:
### Liraglutide/exenatide/lixisenatide: Liraglutide, Exenatide, Exenatide prolonged-release, Lixisenatide, Albiglutide
### Dulaglutide
### Oral semaglutide
### Injectable semaglutide: Low-dose semaglutide, Semaglutide, dose unclear, High-dose semaglutide (dose labels merged, so titration is not a change)
### (SDRN 01 only has Low-dose semaglutide for injectable semaglutide; the other two names are kept to match CPRD)
## Only first instance GLP1 class periods (drug_instance==1), as for the response variables
## SDRN: dstopdate_substance is the era end (last prescription + its supply), CPRD uses the last prescription date,
## so the 'last script of the group + 91 days' limit in 03 falls later in SDRN

# Within the GLP1 class period, consecutive substance periods (ordered by start date) of the same GLP1 group form a run (glp1_group_run_cens)
## glp1_group_start_cens: 1 for the first substance period of the first run of each GLP1 group; 0 for the others (e.g. a return to a GLP1 group used earlier in the class period)
## timetonextgroup_cens: days until the next run (a different GLP1 group) starts - response windows close at this date
## timetogroupstop_cens: days until the end of this run (dstopdate_substance) - response windows close 91 days after this date
## timeprevcombo_cens: days since the previous change in drug class combination or GLP1 group (as timeprevcombo_class, but also counting GLP1 group changes)
## NB: two GLP1 groups started on the same day give timetonextgroup_cens=0 / timeprevcombo_cens=0, so neither gets a response

## free memory: only full_hba1c_drug_merge is needed from here
rm(list = setdiff(ls(pattern = "^full_|^clean_|^baseline_"), "full_hba1c_drug_merge"))
gc()

glp1_cens_periods <- drug_start_stop %>%
		filter(drug_class=="GLP1" & drug_instance==1) %>%
		mutate(
				glp1_group_cens = case_when(
						drug_substance %in% c("Liraglutide", "Exenatide", "Exenatide prolonged-release", "Lixisenatide", "Albiglutide") ~ "Liraglutide/exenatide/lixisenatide",
						drug_substance=="Dulaglutide" ~ "Dulaglutide",
						drug_substance=="Oral semaglutide" ~ "Oral semaglutide",
						drug_substance %in% c("Low-dose semaglutide", "Semaglutide, dose unclear", "High-dose semaglutide") ~ "Injectable semaglutide"
				)
		)

## check: every GLP1 substance should have a group (otherwise add it to the list above)
cat("G1: GLP1 first instance substance periods =", nrow(glp1_cens_periods), "| without a GLP1 group =", sum(is.na(glp1_cens_periods$glp1_group_cens)), "\n")

glp1_cens_periods <- glp1_cens_periods %>%
		group_by(serialno) %>%
		arrange(dstartdate, drug_substance, .by_group = TRUE) %>%
		mutate(
				new_group_run = ifelse(is.na(lag(glp1_group_cens)) | lag(glp1_group_cens)!=glp1_group_cens, 1L, 0L),
				glp1_group_run_cens = cumsum(new_group_run)
		) %>%
		ungroup() %>%
		group_by(serialno, glp1_group_run_cens) %>%
		mutate(
				group_run_start = min(dstartdate, na.rm = TRUE),
				group_run_stop = max(dstopdate_substance, na.rm = TRUE)
		) %>%
		ungroup() %>%
		group_by(serialno, glp1_group_cens) %>%
		mutate(first_group_run = min(glp1_group_run_cens, na.rm = TRUE)) %>%
		ungroup()


# Start of the previous and next run of a different GLP1 group

glp1_group_runs <- glp1_cens_periods %>%
		distinct(serialno, glp1_group_run_cens, group_run_start) %>%
		group_by(serialno) %>%
		arrange(glp1_group_run_cens, .by_group = TRUE) %>%
		mutate(
				prev_group_run_start = lag(group_run_start),
				next_group_run_start = lead(group_run_start)
		) %>%
		ungroup()


# Latest change in drug class combination before each substance start: rows of combo_start_stop with dcstopdate_class are where the drug class combination changes
## For substance periods starting mid class period this is the start of the current drug class combination (timeprevcombo_class counts back to the start of the combination before that)

prev_class_change <- glp1_cens_periods %>%
		distinct(serialno, dstartdate) %>%
		inner_join((combo_start_stop %>% filter(!is.na(dcstopdate_class)) %>% select(serialno, class_change_date = dcstartdate)), by = "serialno") %>%
		filter(class_change_date < dstartdate) %>%
		group_by(serialno, dstartdate) %>%
		summarise(prev_class_change_date = max(class_change_date, na.rm = TRUE), .groups = "drop")


glp1_cens_periods <- glp1_cens_periods %>%
		inner_join((glp1_group_runs %>% select(-group_run_start)), by = c("serialno", "glp1_group_run_cens")) %>%
		left_join(prev_class_change, by = c("serialno", "dstartdate")) %>%
		mutate(
				glp1_group_start_cens = ifelse(dstartdate==group_run_start & glp1_group_run_cens==first_group_run, 1L, 0L),
				timetonextgroup_cens = as.numeric(difftime(next_group_run_start, dstartdate, units = "days")),
				timetogroupstop_cens = as.numeric(difftime(group_run_stop, dstartdate, units = "days")),
				dateprevcombo_cens = case_when(
						is.na(prev_group_run_start) ~ prev_class_change_date,
						is.na(prev_class_change_date) | prev_group_run_start > prev_class_change_date ~ prev_group_run_start,
						TRUE ~ prev_class_change_date
				),
				timeprevcombo_cens = as.numeric(difftime(dstartdate, dateprevcombo_cens, units = "days"))
		) %>%
		select(serialno, dstartdate, drug_substance, glp1_group_cens, glp1_group_run_cens, glp1_group_start_cens, timetonextgroup_cens, timetogroupstop_cens, timeprevcombo_cens)

cat("G2: GLP1 group starts (glp1_group_start_cens==1) =", sum(glp1_cens_periods$glp1_group_start_cens==1),
		"| in run >1 (switched GLP1 group) =", sum(glp1_cens_periods$glp1_group_start_cens==1 & glp1_cens_periods$glp1_group_run_cens>1), "\n")

save(glp1_cens_periods, file = "/home/pcardoso/workspace/SDRN-Cohort-scripts/Interim_Datasets/mm_glp1_cens_periods.RData")

rm(glp1_group_runs, prev_class_change)


# Baseline HbA1c for GLP1 group starts (glp1_group_start_cens==1): as prehba1c/prehba1c12m above, but excluding values before timeprevcombo_cens rather than timeprevcombo_class

baseline_biomarkers_cens <- full_hba1c_drug_merge %>%

		inner_join((glp1_cens_periods %>% filter(glp1_group_start_cens==1) %>% select(serialno, dstartdate, drug_substance, timeprevcombo_cens)), by = c("serialno", "dstartdate", "drug_substance")) %>%

		mutate(drugdatediff = as.numeric(drugdatediff)) %>%

		filter(drugdatediff<=7 & drugdatediff>=-366) %>%

		group_by(serialno, dstartdate, drug_substance) %>%

		mutate(min_timediff = min(abs(drugdatediff), na.rm = TRUE)) %>%
		filter(abs(drugdatediff)==min_timediff) %>%

		mutate(prehba1c12m_cens = min(testvalue, na.rm = TRUE)) %>%
		filter(prehba1c12m_cens==testvalue) %>%

		arrange(drugdatediff) %>%
		filter(row_number()==1) %>%

		ungroup() %>%

		rename(
				prehba1c12mdate_cens = date,
				prehba1c12mdrugdiff_cens = drugdatediff
		) %>%

		filter(prehba1c12mdrugdiff_cens>=0 | is.na(timeprevcombo_cens) | (!is.na(timeprevcombo_cens) & abs(prehba1c12mdrugdiff_cens)<=timeprevcombo_cens)) %>%

		mutate(
				prehba1c_cens = ifelse(prehba1c12mdrugdiff_cens>=-183, prehba1c12m_cens, NA),
				prehba1cdate_cens = if_else(prehba1c12mdrugdiff_cens>=-183, prehba1c12mdate_cens, as.Date(NA)),
				prehba1cdrugdiff_cens = ifelse(prehba1c12mdrugdiff_cens>=-183, prehba1c12mdrugdiff_cens, NA)
		) %>%

		select(serialno, dstartdate, drug_substance, prehba1c_cens, prehba1cdate_cens, prehba1cdrugdiff_cens, prehba1c12m_cens, prehba1c12mdate_cens, prehba1c12mdrugdiff_cens)

cat("G3: GLP1 group starts with prehba1c_cens =", sum(!is.na(baseline_biomarkers_cens$prehba1c_cens)), "\n")

save(baseline_biomarkers_cens, file = "/home/pcardoso/workspace/SDRN-Cohort-scripts/Interim_Datasets/mm_baseline_biomarkers_cens.RData")






