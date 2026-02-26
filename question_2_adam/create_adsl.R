library(metacore)
library(metatools)
library(pharmaversesdtm)
library(admiral)
library(xportr)
library(dplyr)
library(tidyr)
library(lubridate)
library(stringr)


# Load SDTM data and convert blanks to NA
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

# Set DM as ADSL object and remove DOMAIN variable
adsl <- dm %>%
  select(-DOMAIN)

#### Derive AGEGR9 & AGEGR9N ####
# we use the exprs function from tidyr to build the definition later used by
# derive_vars_cat.
agegr9.lookup <- exprs(
  ~condition,            ~AGEGR9, ~AGEGR9N,
  is.na(AGE),          "Missing",        4,
  AGE < 18,                "<18",        1,
  between(AGE, 18, 50),  "18-50",        2,
  !is.na(AGE),             ">50",        3
)

# Create variables AGEGR9 & AGEGR9N from ADSL according to the conditions set in
# agegr9.lookup
adsl <- derive_vars_cat(
  dataset = adsl,
  definition = agegr9.lookup
)


#### Derive ITTFL ####
# To know whether the subject has been randomized, we first have to check if
# there are any missing values and/or what are the type of values in ARM
any(is.na(adsl$ARM)) # No missing values
ARM.groups <- unique(adsl$ARM) 

# Set the ITTFL variable as "Y", except when screening failed. In this case, we
# assume that the subject is not participating in the study and was not randomized
adsl <- adsl %>%
  dplyr::mutate(
    ITTFL = case_when(
      # ARM.groups[4] = "Screen Failure" 
      ARM == ARM.groups[4] ~ "N",
      TRUE ~ "Y"
    )
  )


#### Derive TRTSDTM/TRTSTMF ####
# Get the subset of EX dataset having valid cases. Detect the valid dates
ex.valid <- ex %>%
  # Valid dosis if EXDOSE > 0 or if EXDOSE == 0 and EXTRT is "PLACEBO"
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
    # No flag in TRTSTMF if seconds are missing
    ignore_seconds_flag = TRUE
  ) %>%
  # Only the valid dates are taken
  filter(!is.na(TRTDTM)) 

# Merge back into adsl
adsl <- adsl %>%
  derive_vars_merged(
    dataset_add = ex.valid,
    # Use subject variables to merge back to adsl
    by_vars = exprs(STUDYID, USUBJID),
    new_vars = exprs(
      TRTSDTM = TRTDTM,
      TRTSTMF = TRTTMF
    ),
    # We use 2 expressions because it prevents ties in the case a subject has more
    # than 1 exposure record
    order = exprs(TRTDTM, EXSEQ),
    # Takes the first observation
    mode = "first"
  ) %>%
  # We also creat a TRTEDTM variable which is the Datetime of Last Exposure to Treatment
  # of the valid doses. We will need it later to create the LSTAVLDT variable
  derive_vars_merged(
    dataset_add = ex.valid,
    # Use subject variables to merge back to adsl
    by_vars = exprs(STUDYID, USUBJID),
    # TRTEDTM is taken from the same exposure datetime variable (EXSTDTC-derived)
    new_vars = exprs(TRTEDTM = TRTDTM),
    # We use 2 expressions because it prevents ties in the case a subject has more
    # than 1 exposure record
    order = exprs(TRTDTM, EXSEQ),
    # Takes the last observation after ordering
    mode = "last"
  ) %>%
  mutate(
    # We need this variable to be a date for later sorting (creation of LSTAVLDT)
    TRTEDT = as.Date(TRTEDTM)
  )


#### Derive LSTAVLDT ####

# 1) Get the valid observations for the first condition
vs.dt <- vs %>%
  # Either VSSTRESN or VSSTRESC can be missing but not both
  filter(!(is.na(VSSTRESN) & is.na(VSSTRESC))) %>%
  # Valid date if YYYY-MM-DD, else NA
  # Get valid date parts of AESTDTC from the VS sdtm dataset
  derive_vars_dt(
    dtc = VSDTC,
    # A variable VSDT will be created with valid dates and NA elsewhere
    new_vars_prefix = "VS",
    # If the day is missing, no imputation beyond that so it will return NA
    highest_imputation = "D",
    # Set to none because we don't want any -TMF variable
    flag_imputation = "none"
  ) %>%
  # Only the valid dates are taken
  filter(!is.na(VSDT)) 

# Merge the last complete date of vital assessment into a temporary adsl
# dataframe
adsl.temp <- adsl %>%
  derive_vars_merged(
    dataset_add = vs.dt,
    # Use subject variables to merge back to adsl.temp
    by_vars = exprs(STUDYID, USUBJID),
    new_vars = exprs(last.VSDTC = VSDT),
    # We use 2 expressions for ordering because it prevents ties in the case a 
    # subject has more than 1 record
    order = exprs(VSDT, VSSEQ),
    # Takes the first observation
    mode = "last"
  )

# 2) Get the valid observations for the second condition
# Get valid date parts of AESTDTC from the AE sdtm dataset
ae.dt <- ae %>%
  derive_vars_dt(
    dtc = AESTDTC,
    # A variable AEDT will be created with valid dates and NA elsewhere
    new_vars_prefix = "AE",
    # If the day is missing, no imputation beyond that so it will return NA
    highest_imputation = "D",
    # Set to none because we don't want any -TMF variable
    flag_imputation = "none"
  ) %>%
  filter(!is.na(AEDT))

# Merge the last date from AEDTC
adsl.temp <- adsl.temp %>%
  derive_vars_merged(
    dataset_add = ae.dt,
    # Use subject variables to merge back to adsl.temp
    by_vars = exprs(STUDYID, USUBJID),
    new_vars = exprs(last.AESTDTC = AEDT),
    # We use 2 expressions because it prevents ties in the case a subject has more
    # than 1 exposure record
    order = exprs(AEDT, AESEQ),
    # Takes the first observation
    mode = "last"
  )


# 3) Get the valid observations for the third condition
# Get valid date parts of DSSTDTC from the DS sdtm dataset
ds.dt <- ds %>%
  derive_vars_dt(
    dtc = DSSTDTC,
    # A variable DSDT will be created with valid dates and NA elsewhere
    new_vars_prefix = "DS",
    # If the day is missing, no imputation beyond that so it will return NA
    highest_imputation = "D",
    # Set to none because we don't want any -TMF variable
    flag_imputation = "none"
  )%>%
  filter(!is.na(DSDT))

# Merge the last date from DSDTC
adsl.temp <- adsl.temp %>%
  derive_vars_merged(
    dataset_add = ds.dt,
    # Use subject variables to merge back to adsl.temp
    by_vars = exprs(STUDYID, USUBJID),
    new_vars = exprs(last.DSDTC = DSDT),
    # We use 2 expressions because it prevents ties in the case a subject has more
    # than 1 exposure record
    order = exprs(DSDT, DSSEQ),
    # Takes the last observation
    mode = "last"
  )

# Get the latest date out of "TRTEDT", "last.VSDTC", "last.AESTDTC", "last.DSDTC"
LSTAVLDT_temp <- adsl.temp %>%
  mutate(TRTEDT = as.Date(TRTEDTM)) %>%
  mutate(
    LSTAVLDT = pmax(TRTEDT, last.VSDTC, last.AESTDTC, last.DSDTC, na.rm = TRUE)
  ) %>%
  select(STUDYID, USUBJID, LSTAVLDT)
# Merge the values into adsl
adsl <- adsl %>%
  derive_vars_merged(
    dataset_add = LSTAVLDT_temp,
    by_vars = exprs(STUDYID, USUBJID),
    new_vars = exprs(LSTAVLDT = LSTAVLDT)
  )


# Save ADSL
write.csv(ds, "question_2_adam/adsl.csv")
