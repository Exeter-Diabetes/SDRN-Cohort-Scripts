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
#comorbidity_code_example = "L74.6"
#test <- dbGetQueryMap(con, paste0("SELECT * FROM o_condition WHERE o_condition.condition_code = '", comorbidity_code_example, "'"))
#
## specify the type of code being asked
#test <- dbGetQueryMap(con, paste0("SELECT o_condition.*, o_concept_condition.name FROM o_condition, o_concept_condition WHERE 
#			o_condition.condition_code = 'L74.6' AND o_concept_condition.name = 'opcs4' AND o_condition.concept_id = o_concept_condition.uid"))


## load librarires
library(tidyverse)

comorbidity_OPCS4_table <- list(
		"ckd5_code" = c("L74.6", "M01.1", "M01.2", "M01.3", "M01.4", "M01.5", "M01.8", "M01.9", "M02.6", "M02.7", "M08.4", "M17.2", "M17.4", "M17.8", "M17.9", "X40.1", "X40.2", "X40.3", "X40.5", "X40.6", "X41.1", "X41.2", "X42.1"),
		"pad" = c("L50", "L50.1", "L50.2", "L50.3", "L50.4", "L50.5", "L50.6", "L50.8", "L50.9", "L51", "L51.1", "L51.2", "L51.3", "L51.4", "L51.5", "L51.6", "L51.8", "L51.9", "L52", "L52.1", "L52.2", "L52.8", "L52.9", "L53", "L53.1", "L53.2", "L54.1", "L54.2", "L54.4", "L54.8", "L54.9", "L58", "L58.1", "L58.2", "L58.3", "L58.4", "L58.5", "L58.6", "L58.7", "L58.8", "L58.9", "L59", "L59.1", "L59.2", "L59.3", "L59.4", "L59.5", "L59.6", "L59.7", "L59.8", "L59.9", "L60", "L60.1", "L60.2", "L60.3", "L60.4", "L60.8", "L60.9", "L62", "L62.1", "L62.2", "L62.8", "L62.9", "L63.1", "L63.2", "L63.2", "L63.5", "L65", "L65.1", "L65.2", "L65.3"),
		"photocoagulation" = c("C82.5", "C82", "C79.4"),
		"revasc" = c("K40", "K40.1", "K40.2", "K40.3", "K40.4", "K40.8", "K40.9", "K41", "K41.1", "K41.2", "K41.3", "K41.4", "K41.8", "K41.9", "K42", "K42.1", "K42.2", "K42.3", "K42.4", "K42.8", "K42.9", "K43", "K43.1", "K43.2", "K43.3", "K43.4", "K43.8", "K43.9", "K44", "K44.1", "K44.2", "K44.8", "K44.9", "K45", "K45.1", "K45.2", "K45.3", "K45.4", "K45.5", "K45.6", "K45.8", "K45.9", "K46", "K46.1", "K46.2", "K46.3", "K46.4", "K46.5", "K46.8", "K46.9", "K47.1", "K49", "K49.1", "K49.2", "K49.3", "K49.4", "K49.8", "K49.9", "K50", "K50.1", "K50.2", "K50.3", "K50.4", "K50.8", "K50.9", "K75", "K75.1", "K75.2", "K75.3", "K75.4", "K75.8", "K75.9"),
		"solidorgantransplant" = c("B17", "E53", "G26", "G68", "G78.8", "J01", "J54", "J72.1", "K01", "K02", "M01", "M17", "Y01.2", "Y01.4", "Y01.5", "Y01.6", "Y01.8", "Y01.9"),
		"surgicalpancreaticresection" = c("J55", "J55.1", "J55.2", "J55.3", "J55.8", "J55.9", "J56", "J56.1", "J56.2", "J56.3", "J56.4", "J56.8", "J56.9", "J57", "J57.1", "J57.2", "J57.3", "J57.4", "J57.5", "J57.6", "J57.8", "J57.9")
)


saveRDS(comorbidity_OPCS4_table, "/home/pcardoso/workspace/SDRN-Cohort-scripts/Codelists/Comorbidities/comorbidity_OPCS4_table.rds")
