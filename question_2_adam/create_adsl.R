library(pharmaversesdtm)
library(admiral)
library(dplyr)
library(metatools)


# Load SDTM source domains from {pharmaversesdtm} and standardize missingness:
# convert any blank character values ("") to NA to simplify downstream logic.
dm <- pharmaversesdtm::dm
ds <- pharmaversesdtm::ds
ex <- pharmaversesdtm::ex
ae <- pharmaversesdtm::ae
vs <- pharmaversesdtm::vs

dm <- convert_blanks_to_na(dm)
ds <- convert_blanks_to_na(ds)
ex <- convert_blanks_to_na(ex)
ae <- convert_blanks_to_na(ae)
vs <- convert_blanks_to_na(vs)

# Initialize ADSL from SDTM DM (one record per subject). Remove DOMAIN because
# it is not to be carried into ADaM which are subject-level datasets.
adsl <- dm %>%
  select(-DOMAIN)

#### Derive AGEGR9 & AGEGR9N ####
# Define a lookup table for categorizing analysis age (AGE) into AGEGR9 (text)
# and AGEGR9N (numeric). Conditions are evaluated top-to-bottom by
# derive_vars_cat(); the first TRUE condition assigns the corresponding values.
agegr9.lookup <- exprs(
  ~condition,            ~AGEGR9, ~AGEGR9N,
  is.na(AGE),          "Missing",        4,
  AGE < 18,                "<18",        1,
  between(AGE, 18, 50),  "18-50",        2,
  !is.na(AGE),             ">50",        3
)

# Apply the lookup rules to derive AGEGR9 and AGEGR9N on the ADSL dataset.
adsl <- derive_vars_cat(
  dataset = adsl,
  definition = agegr9.lookup
)


#### Derive ITTFL ####
# Derive the Intent-to-Treat population flag. NOTE: the assessment spec defines
# ITTFL as "Y" when ARM is populated and "N" otherwise. Here, we additionally
# treat ARM == "Screen Failure" as not randomized (study-specific assumption).
any(is.na(adsl$ARM)) # No missing values
ARM.groups <- unique(adsl$ARM) 

# Set the ITTFL variable as "Y", except when screening failed. In this case, we
# assume that the subject is not participating in the study and was not randomized
adsl <- adsl %>%
  dplyr::mutate(
    ITTFL = case_when(
      ARM == "Screen Failure"  ~ "N",
      TRUE ~ "Y"
    )
  )


#### Derive TRTSDTM/TRTSTMF ####
# Prepare EX records eligible for treatment start/end derivations:
# - Keep only "valid dose" records per spec (EXDOSE > 0 OR placebo with EXDOSE == 0).
# - Convert EXSTDTC to a datetime (TRTDTM) and a time imputation flag (TRTTMF).
# - Do not impute missing date components (highest_imputation = "h" enforces complete date);
#   impute missing/partial time components to 00:00:00 (time_imputation = "first").
# - Do not set the time imputation flag when only seconds are missing.
ex.valid <- ex %>%
  filter(EXDOSE > 0 | (EXDOSE == 0 & stringr::str_detect(toupper(EXTRT), "PLACEBO"))) %>%
  derive_vars_dtm(
    new_vars_prefix = "TRT",
    dtc = EXSTDTC,
    # We do not allow imputation higher than the level of hour
    highest_imputation = "h",
    # Imputes the earliest missing time
    time_imputation = "first",
    # Will create an imputation flag in a TRTSTMF variable
    flag_imputation = "time",
    # No flag in TRTTMF if seconds are missing
    ignore_seconds_flag = TRUE
  ) %>%
  # Retain only records with complete date part (TRTDTM non-missing).
  filter(!is.na(TRTDTM)) 

# Merge first and last qualifying exposure datetimes into ADSL:
# - TRTSDTM/TRTSTMF: earliest TRTDTM per subject (mode = "first")
# - TRTEDTM: latest TRTDTM per subject (mode = "last")
# Secondary ordering by EXSEQ provides deterministic tie-breaking when datetimes match.
adsl <- adsl %>%
  derive_vars_merged(
    dataset_add = ex.valid,
    by_vars = exprs(STUDYID, USUBJID),
    new_vars = exprs(
      TRTSDTM = TRTDTM,
      TRTSTMF = TRTTMF
    ),
    order = exprs(TRTDTM, EXSEQ),
    mode = "first"
  ) %>%
  derive_vars_merged(
    dataset_add = ex.valid,
    by_vars = exprs(STUDYID, USUBJID),
    # TRTEDTM is derived from the same exposure-start datetime (EXSTDTC-derived TRTDTM),
    # selecting the latest qualifying record per subject.
    new_vars = exprs(TRTEDTM = TRTDTM),
    order = exprs(TRTDTM, EXSEQ),
    mode = "last"
  ) %>%
  mutate(
    # Datepart of TRTEDTM (used later for LSTAVLDT derivation).
    TRTEDT = as.Date(TRTEDTM)
  )


#### Derive LSTAVLDT ####
# LSTAVLDT is the last known alive date per subject, defined as the maximum of:
# (1) last complete VSDTC date with a valid VS result,
# (2) last complete AE onset date (AESTDTC),
# (3) last complete disposition date (DSSTDTC),
# (4) datepart of TRTEDTM (last qualifying exposure datetime).

# (1) Vitals: keep records with a non-missing result (VSSTRESN/VSSTRESC not both missing),
# derive the date part of VSDTC (VSDT), keep complete dates only, then select the latest VSDT per subject.
vs.dt <- vs %>%
  filter(!(is.na(VSSTRESN) & is.na(VSSTRESC))) %>%
  derive_vars_dt(
    dtc = VSDTC,
    new_vars_prefix = "VS",
    highest_imputation = "D",
    flag_imputation = "none"
  ) %>%
  filter(!is.na(VSDT)) 

# Start a temporary working dataset by adding the three “last observed date” candidates
# (VS, AE, DS) to ADSL. Only these temporary variables will be used to compute LSTAVLDT.
adsl.temp <- adsl %>%
  derive_vars_merged(
    dataset_add = vs.dt,
    by_vars = exprs(STUDYID, USUBJID),
    new_vars = exprs(last.VSDTC = VSDT),
    order = exprs(VSDT, VSSEQ),
    mode = "last"
  )

# (2) Adverse events: derive the date part of AESTDTC (AEDT), keep complete dates only,
# then select the latest AEDT per subject.
ae.dt <- ae %>%
  derive_vars_dt(
    dtc = AESTDTC,
    new_vars_prefix = "AE",
    highest_imputation = "D",
    flag_imputation = "none"
  ) %>%
  filter(!is.na(AEDT))

adsl.temp <- adsl.temp %>%
  derive_vars_merged(
    dataset_add = ae.dt,
    by_vars = exprs(STUDYID, USUBJID),
    new_vars = exprs(last.AESTDTC = AEDT),
    order = exprs(AEDT, AESEQ),
    mode = "last"
  )


# (3) Disposition: derive the date part of DSSTDTC (DSDT), keep complete dates only,
# then select the latest DSDT per subject.
ds.dt <- ds %>%
  derive_vars_dt(
    dtc = DSSTDTC,
    new_vars_prefix = "DS",
    highest_imputation = "D",
    flag_imputation = "none"
  )%>%
  filter(!is.na(DSDT))

adsl.temp <- adsl.temp %>%
  derive_vars_merged(
    dataset_add = ds.dt,
    by_vars = exprs(STUDYID, USUBJID),
    new_vars = exprs(last.DSDTC = DSDT),
    order = exprs(DSDT, DSSEQ),
    mode = "last"
  )

# Compute LSTAVLDT as the maximum across the four candidate dates for each subject.
# (TRTEDT, last.VSDTC, last.AESTDTC, last.DSDTC)
LSTAVLDT_temp <- adsl.temp %>%
  mutate(TRTEDT = as.Date(TRTEDTM)) %>%
  mutate(
    LSTAVLDT = pmax(TRTEDT, last.VSDTC, last.AESTDTC, last.DSDTC, na.rm = TRUE)
  ) %>%
  select(STUDYID, USUBJID, LSTAVLDT)

# Merge LSTAVLDT into the final ADSL (retain all existing ADSL variables and add LSTAVLDT).
adsl <- adsl %>%
  derive_vars_merged(
    dataset_add = LSTAVLDT_temp,
    by_vars = exprs(STUDYID, USUBJID),
    new_vars = exprs(LSTAVLDT = LSTAVLDT)
  )


# Save derived ADSL
write.csv(adsl, "question_2_adam/adsl.csv")
