library(pharmaverseraw)
library(sdtm.oak)
library(dplyr)

# Read raw dataset and load study CT
ds.raw <- pharmaverseraw::ds_raw
study.ct <- read.csv("question_1_sdtm/sdtm_ct.csv")

# Prep the mapping of variables
# Create temporary df to enable direct mapping of DSTERM, DSDECOD and DSCAT
ds.temp <- ds.raw
ds.temp <- ds.temp %>%
  # Creates a temp DSDECOD variable with the value of IT.DSDECOD if OTHERSP is
  # null, and with the value of OTHERSP if any
  dplyr::mutate(
    TEMP.DSDECOD = case_when(
      is.na(OTHERSP) ~ IT.DSDECOD,
      TRUE ~ OTHERSP
    )
  ) %>%
  # Creates a temp DSTERM variable which is equal to value in OTHERSP if provided
  # TODO: Maybe we don't need that and we map directly
  dplyr::mutate(
    TEMP.DSTERM = case_when(
      is.na(OTHERSP) ~ IT.DSTERM,
      TRUE ~ OTHERSP
    )
  ) %>%
  # Creates a temp DSCAT variable to be "PROTOCOL MILESTONE", "OTHER EVENT" or
  # "DISPOSITION EVENT"
  dplyr::mutate(
    TEMP.DSCAT = case_when(
      IT.DSDECOD == "Randomized" & is.na(OTHERSP)  ~ "PROTOCOL MILESTONE",
      !is.na(OTHERSP) ~ "OTHER EVENT",
      TRUE ~ "DISPOSITION EVENT"
    )
  )

# Create oak IDs to enable topic variable mapping
ds.temp <- ds.temp %>%
  generate_oak_id_vars(
    pat_var = "PATNUM",
    raw_src = "ds.raw"
  )

# Map the topic variable
# Reflects the collected observation, not the standardized term
# No ct because this is the verbatim
ds <-
  assign_no_ct(
    raw_dat = ds.temp,
    raw_var = "TEMP.DSTERM",
    tgt_var = "DSTERM",
    id_vars = oak_id_vars()
  )

# Map other variables
ds <- ds %>% 
  # Map date in ISO 8601 format
  assign_datetime(
    raw_dat = ds.raw,
    raw_var = "IT.DSSTDAT",
    tgt_var = "DSSTDTC",
    raw_fmt = c("m-d-y"),
    id_vars = oak_id_vars()
  ) %>%
  assign_datetime(
    raw_dat = ds.raw,
    raw_var = c("DSDTCOL", "DSTMCOL"),
    tgt_var = "DSDTC",
    raw_fmt = c("m-d-y", "H:M"),
    id_vars = oak_id_vars()
  ) %>%
  # Map the value of "TEMP.DSDECOD" in "DSDECOD"
  assign_ct(
    raw_dat = ds.temp,
    raw_var = "TEMP.DSDECOD",
    tgt_var = "DSDECOD",
    ct_spec = study.ct,
    ct_clst = "C66727",
    id_vars = oak_id_vars()
  ) %>%
  # Map the values if "TEMP.DSCAT" in "DSCAT"
  assign_ct(
    raw_dat = ds.temp,
    raw_var = "TEMP.DSCAT",
    tgt_var = "DSCAT",
    ct_spec = study.ct,
    ct_clst = "C74558",
    id_vars = oak_id_vars()
  )

# Map the SDTM derived variables
ds <- ds %>%
  dplyr::mutate(
    STUDYID = ds.raw$STUDY,
    DOMAIN = "DS",
    USUBJID = paste0("01-", ds.raw$PATNUM),
  ) %>%
  # DSSEQ ensures each DS record for a subject is uniquely identifiable
  # Therefore, we use USUBJID and DSTERM as variables for constructing DSSEQ
  derive_seq(
    tgt_var = "DSSEQ",
    rec_vars = c("USUBJID", "DSTERM")
  ) %>%
  derive_study_day(
    sdtm_in = .,
    dm_domain = dm,
    tgdt = "AESTDTC",
    refdt = "RFXSTDTC",
    study_day_var = "AESTDY"
  ) %>%
  derive_study_day(
    sdtm_in = .,
    dm_domain = dm,
    tgdt = "AEENDTC",
    refdt = "RFXENDTC",
    study_day_var = "AEENDY"
  ) %>%
  select(
    "STUDYID", "DOMAIN", "USUBJID", "AESEQ", "AETERM", "AELLT", "AELLTCD", "AEDECOD", "AEPTCD", "AEHLT", "AEHLTCD", "AEHLGT",
    "AEHLGTCD", "AEBODSYS", "AEBDSYCD", "AESOC", "AESOCCD", "AESEV", "AESER", "AEACN", "AEREL", "AEOUT", "AESCAN", "AESCONG",
    "AESDISAB", "AESDTH", "AESHOSP", "AESLIFE", "AESOD", "AEDTC", "AESTDTC", "AEENDTC", "AESTDY", "AEENDY"
  )









