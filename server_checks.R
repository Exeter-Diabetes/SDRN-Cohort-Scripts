# Author: pcardoso
###############################################################################

# Server checks for the MASTERMIND (treatment response) cohort
# Prints only short counts/summaries (no patient-level data), so the console can be photographed
# or copied by hand. Each line starts with a code (A1, A2, ...) so results can be matched back.
#
# PART A: done 2026-11 on the old outputs (those files are now in Interim_Datasets/old_mm/); skip
# PART B: run on the outputs of the offline run (01 -> 11); no database needed
# PART C: C1o and C6 work without the database; C1-C5 need database access
#
# Run one part at a time (select the lines and run) and photograph the console after each part.

rm(list = ls())
library(tidyverse)

dir <- "/home/pcardoso/workspace/SDRN-Cohort-scripts/Interim_Datasets/"

## small helpers: print one labelled line
p <- function(code, ...) cat(code, ": ", paste(..., collapse = " "), "\n", sep = "")
q5 <- function(x) paste(round(quantile(as.numeric(x), c(0, 0.25, 0.5, 0.75, 1), na.rm = TRUE)), collapse = " / ")
pct <- function(x) paste0(round(100 * mean(x, na.rm = TRUE), 1), "%")
## counts of people/periods below 5 are shown as "<5" (PHS statistical disclosure control)
n5 <- function(x) ifelse(x > 0 & x < 5, "<5", as.character(x))


###############################################################################
# PART A: existing results

load(paste0(dir, "mm_baseline_biomarkers.RData"))
load(paste0(dir, "mm_drug_start_stop.RData"))

b <- baseline_biomarkers %>% filter(drug_instance == 1)
p("A1 first-instance periods", n5(nrow(b)))
p("A2 prehba1c missing", pct(is.na(b$prehba1c)))
p("A3 prehba1cdrugdiff min/q1/med/q3/max", q5(b$prehba1cdrugdiff))
p("A4 prebmidrugdiff min/q1/med/q3/max", q5(b$prebmidrugdiff))
## A3/A4: if every value is -730 (or A2 is ~100%), the baseline-date bug in 02 affected these results

load(paste0(dir, "mm_glycaemic_failure.RData"))
g <- glycaemic_failure %>% filter(drug_class %in% c("DPP4", "GLP1", "SGLT2", "SU", "TZD"))
for (cl in c("DPP4", "GLP1", "SGLT2", "SU", "TZD")) {
	r <- g %>% filter(drug_class == cl) %>% count(hba1c_fail_7.5_reason) %>% mutate(pc = round(100 * n / sum(n)))
	p(paste0("A5 ", cl, " fail7.5 % (2HbA1c / 1+add / end px / change / NA)"),
			paste(sapply(c("Fail - 2 HbA1cs >threshold", "Fail - 1 HbA1cs >threshold then add drug", "End of prescriptions", "Change in diabetes drugs"),
					function(x) sum(r$pc[r$hba1c_fail_7.5_reason %in% x])), collapse = " / "),
			"/", sum(r$pc[is.na(r$hba1c_fail_7.5_reason)]))
}

load(paste0(dir, "mm_combo_class_start_stop.RData"))
p("A6 drugchange counts (add/remove/swap/start/break)",
		paste(sapply(c("add", "remove", "swap", "start of px", "stop - break"), function(x) n5(sum(combo_class_start_stop$drugchange == x, na.rm = TRUE))), collapse = " / "))

rm(list = setdiff(ls(), c("dir", "p", "q5", "pct", "n5")))
gc()


###############################################################################
# PART B: after re-running 01 -> 11 with the fixed scripts

## B0: every drug start should have a combination row on the same date (11_final_merge drops starts without one)
load(paste0(dir, "mm_drug_start_stop.RData"))
load(paste0(dir, "mm_combo_start_stop.RData"))
nm <- drug_start_stop %>% anti_join(combo_start_stop, by = c("serialno", "dstartdate" = "dcstartdate"))
p("B0 drug starts with no combination row (should be 0) / all starts", n5(nrow(nm)), "/", n5(nrow(drug_start_stop)))
p("B0b ...of which by substance (top 3)", paste(head(names(sort(table(nm$drug_substance), decreasing = TRUE)), 3), collapse = " | "))
p("B0c first-instance Exenatide starts (old 5-drug table: 6415)", n5(sum(drug_start_stop$drug_substance == "Exenatide" & drug_start_stop$drug_instance == 1)))
rm(nm, drug_start_stop, combo_start_stop)

load(paste0(dir, "mm_baseline_biomarkers.RData"))
b <- baseline_biomarkers %>% filter(drug_instance == 1)
p("B1 prehba1c missing", pct(is.na(b$prehba1c)))
p("B2 prehba1cdrugdiff min/q1/med/q3/max", q5(b$prehba1cdrugdiff))

load(paste0(dir, "mm_response_biomarkers.RData"))
r <- response_biomarkers
for (cl in c("DPP4", "GLP1", "SGLT2", "SU", "TZD")) {
	x <- r %>% filter(drug_class == cl)
	p(paste0("B3 ", cl, " n / % with hba1cresp12m / mean resp12m / % with hba1cresp6m"),
			n5(nrow(x)), "/", pct(!is.na(x$hba1cresp12m)), "/", round(mean(x$hba1cresp12m, na.rm = TRUE), 1), "/", pct(!is.na(x$hba1cresp6m)))
}

load(paste0(dir, "mm_glycaemic_failure.RData"))
g <- glycaemic_failure
for (cl in c("DPP4", "GLP1", "SGLT2", "SU", "TZD")) {
	r2 <- g %>% filter(drug_class == cl) %>% count(hba1c_fail_7.5_reason) %>% mutate(pc = round(100 * n / sum(n)))
	p(paste0("B4 ", cl, " fail7.5 % (2HbA1c / 1+add / end px / change / NA)"),
			paste(sapply(c("Fail - 2 HbA1cs >threshold", "Fail - 1 HbA1cs >threshold then add drug", "End of prescriptions", "Change in diabetes drugs"),
					function(x) sum(r2$pc[r2$hba1c_fail_7.5_reason %in% x])), collapse = " / "),
			"/", sum(r2$pc[is.na(r2$hba1c_fail_7.5_reason)]))
}

load(paste0(dir, "mm_combo_class_start_stop.RData"))
p("B5 drugchange counts (add/remove/swap/start/break)",
		paste(sapply(c("add", "remove", "swap", "start of px", "stop - break"), function(x) n5(sum(combo_class_start_stop$drugchange == x, na.rm = TRUE))), collapse = " / "))

## B6: 'switches' hidden by overlapping supply. For each first-instance class start, is another class REMOVED soon after?
## (in SDRN the old drug's era runs to the end of its supply, so a switch looks like add-then-remove)
## <=1 day = old drug's era ends on the day the new drug starts (a 1-day combination of both drugs)
x <- combo_class_start_stop %>%
		filter(drugchange %in% c("add", "swap")) %>%
		mutate(ttar = as.numeric(timetoaddrem_class))
p("B6 add/swap combos where next change is a REMOVE within <=1 / 2-30 / 31-90 / 91-183 days",
		n5(sum(x$nextdrugchange == "remove" & x$ttar <= 1, na.rm = TRUE)), "/",
		n5(sum(x$nextdrugchange == "remove" & x$ttar > 1 & x$ttar <= 30, na.rm = TRUE)), "/",
		n5(sum(x$nextdrugchange == "remove" & x$ttar > 30 & x$ttar <= 90, na.rm = TRUE)), "/",
		n5(sum(x$nextdrugchange == "remove" & x$ttar > 90 & x$ttar <= 183, na.rm = TRUE)),
		"of", n5(nrow(x)))

rm(list = setdiff(ls(), c("dir", "p", "q5", "pct", "n5")))
gc()


###############################################################################
# PART C: information for the next update

## C1o (no database): first diabetes drug start per person, by year, from the newest final t2d_all_drug_periods
## (diabetes drugs only; a jump in counts shows when prescribing data begin)
fd <- "/home/pcardoso/workspace/SDRN-Cohort-scripts/Final_Datasets/"
load(tail(sort(list.files(fd, pattern = "^mm_[0-9]{8}_t2d_all_drug_periods\\.RData$", full.names = TRUE)), 1))
yrs <- t2d_all_drug_periods %>% group_by(serialno) %>% summarise(first_start = min(dstartdate)) %>% count(yr = format(first_start, "%Y"))
p("C1o first diabetes drug start per person, year:n", paste0(yrs$yr, ":", n5(yrs$n), collapse = "  "))
rm(t2d_all_drug_periods, yrs)
gc()

## C6 (codelist files) at the end also runs without the database

con <- dbConn("NDS_2024")

## C1: when does prescribing data start? (first-ever drug era of any drug per person, by year) -
## a jump in counts shows when prescribing data begin; starts before that are not true first starts
yrs <- dbGetQueryMap(con, "
				SELECT YEAR(first_start) AS yr, COUNT(*) AS n FROM (
				SELECT serialno, MIN(startdate) AS first_start FROM o_drug_era GROUP BY serialno) t
				GROUP BY YEAR(first_start) ORDER BY yr")
p("C1 first era per person, year:n", paste0(yrs$yr, ":", n5(yrs$n), collapse = "  "))

## C2: observation concept paths that may hold eosinophils (CPRD added these) - list names only
paths <- dbGetQueryMap(con, "SELECT DISTINCT path FROM o_concept_observation")$path
p("C2 paths matching eosin", paste(grep("eosin", paths, value = TRUE, ignore.case = TRUE), collapse = " | "))
p("C3 number of observation paths", length(paths))

## C4: drug names that may be oestrogens (CPRD added oestrogens as a non-diabetes medication) - count only
oes <- dbGetQueryMap(con, "SELECT COUNT(DISTINCT drugname) AS n FROM o_concept_drugs WHERE drugname LIKE '%ESTR%' OR drugname LIKE '%OESTR%'")
p("C4 distinct drugnames containing ESTR/OESTR", oes$n)

## C5: hospital inpatient (SMR01) information - CPRD has hospital admission in previous year and emergency admission variables
## (needs admission dates and admission type); list the kinds of SMR01 paths and the o_condition column names
smr <- dbGetQueryMap(con, "SELECT DISTINCT path FROM o_concept_condition WHERE path LIKE 'smr01%'")$path
p("C5 smr01 paths n / kinds", length(smr), "/", paste(unique(sub("^(smr01-[a-z_]*).*", "\\1", smr)), collapse = " "))
p("C5b o_condition columns", paste(names(dbGetQueryMap(con, "SELECT * FROM o_condition LIMIT 1")), collapse = " "))

dbDisconnect(con)

## C6: codelist files used by the updated 04_comorbidities (1 = file exists)
cl <- "/home/pcardoso/workspace/SDRN-Cohort-scripts/Codelists/"
f <- c("bph", "benignprostatehyperplasia", "ukpds_photocoagulation", "photocoagulation", "hypoglycaemia", "genital_infection", "genital_infection_nonspec")
p("C6 ICD10 files", paste0(f, "=", as.integer(file.exists(paste0(cl, "ICD10/exeter_icd10_", f, ".txt"))), collapse = " "))
p("C6b OPCS4 files", paste0(f, "=", as.integer(file.exists(paste0(cl, "OPCS4/exeter_opcs4_", f, ".txt"))), collapse = " "))
