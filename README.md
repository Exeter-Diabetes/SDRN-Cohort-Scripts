# CPRD Aurum Cohort scripts

## Introduction

This repository contains the R scripts used by the Exeter Diabetes team to produce three cohorts and their associated biomarker/comorbidity/sociodemographic data from a SDRN dataset (NDS 2023): 

-   An **'at-diagnosis'** cohort 
-   A **prevalent** cohort (registered at 01/11/2022) 
-   A **treatment response** (MASTERMIND) cohort (those initiating diabetes medications)

A description of the SRDN cohort can be found at: [DOI 10.1136/bmjopen-2022-063046](https://www.doi.org/10.1136/bmjopen-2022-063046)

The below diagram outlines the data processing steps involved in creating these cohorts.

``` mermaid
graph TD;
    A["<b>NDS 2023 release</b>"] --> |"Unique patients with a diabetes diagnosis"| B["<b>Diabetes cohort*</b>: n=558,892"]
    B --> C["<b>01 At-diagnosis cohort</b>: <br> n=558,892 <br> Index date=diagnosis date"]
    B --> D["<b>02 Prevalent cohort</b>: <br> n=558,892 <br> Actively registered on 01/11/2022 <br> Index date=diagnosis date"]
    B --> E["<b>03 Treatment response (MASTERMIND) cohort</b>: n=401,998 with 926,897 unique drug periods <br> With script for diabetes medication <br> Index date=drug start date"]
```

\* Earliest mention of diabetes is required to after on or after date of birth.



## Extract details

All cohorts creates use the latest version of NDS-2023 (National Diabetes Dataset).



## Script overview

The below diagram shows the R scripts (in grey boxes) used to create the final cohorts (at-diagnosis, prevalent, and treatment response).

``` mermaid
graph TD;
    A["<b>Our extract</b>"] --> |"all_diabetes_cohort"| B["<b>Diabetes cohort</b> with static <br> patient data including <br> ethnicity and SIMD*"]
    A --> |"baseline_biomarkers <br> (requires index date)"| C["<b>Biomarkers</b> <br> at index date"]
    A --> |"comorbidities <br> (requires index date)"| D["<b>Comorbidities</b> <br> at index date"]
    A --> |"smoking <br> (requires index date)"| E["<b>Smoking status</b> <br> at index date"]
    A --> |"alcohol <br> (requires index date)"| F["<b>Alcohol status</b> <br> at index date"]
    A --> |"ckd_stages <br> (requires index date)"| G["<b>CKD stage</b <br> at index date"]
    A --> |"all_death_causes"| H["<b>Death causes</b> <br> for all patients"]
    
    B --> |"final_merge"| I["<b>Final cohort dataset</b>"]
    C --> |"final_merge"| I
    D --> |"final_merge"| I
    E --> |"final_merge"| I
    F --> |"final_merge"| I
    G --> |"final_merge"| I
    H --> |"final_merge"| I
```
\*SIMD=Scottish Index of Multiple Deprivation; 'static' using the 2016 data. SIMD is coded as 1=most deprived, 10=least deprived. This differs from England deprivation score, where 1=least deprived, 10=most deprived. Two variables have been created: simd_decile (scottish version), imd_decile (translation of scottish to english).

&nbsp;

Each of the three final cohorts (at-diagnosis, prevalent, and treatment response) contains static patient data e.g. ethnicity, IMD and diabetes type from the diabetes cohort dataset, plus biomarker, comorbidity, and sociodemographic (smoking/alcohol) data at the (cohort-specific) index date.


This directory contains the scripts which are common to all three cohorts: 'all_diabetes_cohort'. These pull out static patient characteristics or features based on longitudinal data which may go beyond the index date of the cohorts.


The exact 'tailored' and additional scripts used to create each cohort dataset can be found in the relevant subdirectory: [01-At-diagnosis](https://github.com/Exeter-Diabetes/SDRN-Cohort-Scripts/tree/main/01-At-diagnosis), [02-Prevalent](https://github.com/Exeter-Diabetes/SDRN-Cohort-Scripts/tree/main/02-Prevalent), [03-Treatment-response-(MASTERMIND)](https://github.com/Exeter-Diabetes/SDRN-Cohort-Scripts/tree/main/03-Treatment-response-(MASTERMIND)), along with a data dictionary of all variables in the final cohort dataset.

&nbsp;


| Script description | &nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;Outputs&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp; |
| ---- | ---- |
| **all_patid_death_causes**: uses ONS death data to define whether primary or any cause of death was from CVD/heart failure/kidney failure (all primary and secondary death causes also included) |  **all_patid_death_causes**:  1 row per patid, with all death causes and binary variables for whether CVD/HF/KF was primary or any cause of death  |
| **all_diabetes_cohort**: table of patids meeting the criteria for our mixed Type 1/Type 2/'other' diabetes cohort plus additional patient variables | **all_diabetes_cohort**: 1 row per patid of those in the diabetes cohort, with diabetes diagnosis dates, DOB, gender, ethnicity etc. |
 **{cohort_prefix}\_final_merge**: 1 row per patid-index date combination with relevant biomarker/comorbidity/smoking/alcohol variables |

&nbsp;


## Codelists

Codelists used for this cohort can be found at: <https://github.com/Exeter-Diabetes/CPRD-Codelists>

## Comorbidities

Comorbidities have been coded using ICD10/OPCS4 codes.

## Things not codded yet:

-   EFI: electronic frailty index (<https://pubmed.ncbi.nlm.nih.gov/26944937/>)
-   QRISK2


## Data dictionary of variables in 'final_merge' table

Biomarkers included: HbA1c (mmol/mol), weight (kg), height (m), BMI (kg/m2), HDL (mmol/L), triglycerides (mmol/L), blood creatinine (umol/L), LDL (mmol/L), ALT (U/L), AST (U/L), total cholesterol (mmol/L), DBP (mmHg), SBP (mmHg), ACR (mg/mmol / g/mol).

Comorbidities included: atrial fibrillation, angina, asthma, bronchiectasis, CKD stage 5/ESRD, CLD, COPD, cystic fibrosis, dementia, diabetic nephropathy, haematological cancers, heart failure, hypertension, IHD, myocardial infarction, neuropathy, other neurological conditions, PAD, pulmonary fibrosis, pulmonary hypertension, retinopathy, (coronary artery) revascularisation, rhematoid arthritis, solid cancer, solid organ transplant, stroke, TIA.

| Variable name | Description | Notes on derivation |
| --- | --- | --- |
| patid | unique patient identifier | |
| index_date | index date (e.g. diagnosis date for 'at-diagnosis' cohort, 01/02/2020 for prevalent cohort, drug start date for treatment response cohort) | |
| gender | gender (1=male, 2=female) | |
| dob | date of birth | if month and date missing, 1st July used, if date but not month missing, 15th of month used, or earliest medcode in year of birth if this is earlier |
| ethnicity_5cat | 5-category ethnicity: (0=White, 1=South Asian, 2=Black, 3=Other, 4=Mixed) | |
| simd_decile | 2016 Scottish Index of Multiple Deprivation (SIMD) decile (1=most deprived, 10=least deprived) || 
| imd_decile | Swap of SIMD to English Index of Multiple Deprivation (IMD) decile (1=least deprived, 10=most deprived) | |
| dm_diag_age_all | age at diabetes diagnosis | dm_diag_date_all - dob<br />NB: as at-diagnosis cohort excludes those with diagnosis dates before registration start, this variable is missing and only dm_diag_age (below) is present<br />See above note next to dm_diag_date_all variable on young diagnosis in T2Ds |
| diabetes_type | diabetes type | (1=Type 1, 2=Type 2) |
| pre{biomarker} | biomarker value at baseline | For all biomarkers except HbA1c: pre{biomarker} is closest biomarker to index date within window of -730 days (2 years before index date) and +7 days (a week after index date)<br /><br />For HbA1c: prehba1c is closest HbA1c to index date within window of -183 days (6 months before index date) and +7 days (a week after index date) |
| pre{biomarker}date | date of baseline biomarker | |
| pre{biomarker}datediff | days between index date and baseline biomarker (negative: biomarker measured before index date) | |
| height | height in cm | Mean of all values on/post-index date |
| preckdstage | CKD stage at baseline | CKD stages calculated as per [our algorithm](https://github.com/Exeter-Diabetes/CPRD-Codelists#ckd-chronic-kidney-disease-stage)<br />eGFR calculated from creatinine using CKD-EPI creatinine 2021 equation<br />Start date = earliest test for CKD stage, only including those confirmed by another test at least 91 days later, without a test for a different stage in the intervening period<br />Baseline stage = maximum stage with start date < index date or up to 7 days afterwards<br />CKD5 supplemented by medcodes/ICD10/OPCS4 codes for CKD5 / ESRD |
| preckdstagedate | date of onset of baseline CKD stage (earliest test for this stage) | |
| preckdstagedatediff | days between index date and preckdstagedate | |
| pre_index_date_earliest_{comorbidity} | earliest occurrence of comorbidity before/at index date | |
| pre_index_date_latest_{comorbidity} | latest occurrence of comorbidity before/at index date | |
| pre_index_date_{comorbidity} | binary 0/1 if any instance of comorbidity before/at index date | |
| post_index_date_first_{comorbidity} | earliest occurrence of comorbidity after (not at) index date | |
| smoking_cat | Smoking category at index date: Non-smoker, Ex-smoker or Active smoker | |
| alcohol_cat | Alcohol consumption category at index date: None, Within limits, Excess or Heavy |  |
| primary_death_cause1 | primary death cause from National Records of Scotland Death data (ICD10) |  | 
| secondary_death_cause1-17 | secondary death cases from National Records of Scotland Death data (ICD10) | | 
| cv_death_primary_cause | 1 if primary cause of death is CV | | 
| cv_death_any_cause | 1 if any (primary or secondary) cause of death is CV | | 
| hf_death_primary_cause | 1 if primary cause of death is heart failure | |
| hf_death_any_cause | 1 if any (primary or secondary) cause of death is heart failure | | 
| kf_death_primary_cause | 1 if primary cause of death is kidney failure | |
| kf_death_any_cause | 1 if any (primary or secondary) cause of death is kidney failure | |
