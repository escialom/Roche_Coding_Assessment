library(pharmaverseraw)
library(sdtm.oak)
library(dplyr)


# Read raw dataset and load study CT
ds.raw <- pharmaverseraw::ds_raw
study.ct <- read.csv("question_1_sdtm/sdtm_ct.csv")


# Prep the mapping of variables
# Create temporary df to enable direct mapping of DSTERM, DSDECOD and DSCAT
ds.prep <- ds.raw %>%
  mutate(
    TEMP.DSDECOD = if_else(is.na(OTHERSP), IT.DSDECOD, OTHERSP),
    TEMP.DSTERM  = if_else(is.na(OTHERSP), IT.DSTERM,  OTHERSP),
    # Creates a temp DSCAT variable to be "PROTOCOL MILESTONE", "OTHER EVENT" or
    # "DISPOSITION EVENT"
    TEMP.DSCAT = case_when(
      IT.DSDECOD == "Randomized" & is.na(OTHERSP) ~ "PROTOCOL MILESTONE",
      !is.na(OTHERSP)                             ~ "OTHER EVENT",
      TRUE                                        ~ "DISPOSITION EVENT"
    )
  ) %>%
  # Create oak IDs to enable topic variable mapping
  generate_oak_id_vars(
    pat_var = "PATNUM",
    raw_src = "ds.raw"
  )


# Map the topic variable
# Reflects the collected observation, not the standardized term
# No ct because this is the verbatim
ds <- assign_no_ct(
  raw_dat = ds.prep,
  raw_var = "TEMP.DSTERM",
  tgt_var = "DSTERM",
  id_vars = oak_id_vars()
  )

# Map other variables
ds <- ds %>% 
  # DSSTDTC: map date in ISO 8601 format
  assign_datetime(
    raw_dat = ds.prep,
    raw_var = "IT.DSSTDAT",
    tgt_var = "DSSTDTC",
    raw_fmt = "m-d-y",
    id_vars = oak_id_vars()
  ) %>%
  # DSDTC (datetime) from date + time
  assign_datetime(
    raw_dat = ds.prep,
    raw_var = c("DSDTCOL", "DSTMCOL"),
    tgt_var = "DSDTC",
    raw_fmt = c("m-d-y", "H:M"),
    id_vars = oak_id_vars()
  ) %>%
  # Map the value of "TEMP.DSDECOD" in "DSDECOD"
  assign_ct(
    raw_dat = ds.prep,
    raw_var = "TEMP.DSDECOD",
    tgt_var = "DSDECOD",
    ct_spec = study.ct,
    ct_clst = "C66727",
    id_vars = oak_id_vars()
  ) %>%
  # Map the values if "TEMP.DSCAT" in "DSCAT"
  assign_ct(
    raw_dat = ds.prep,
    raw_var = "TEMP.DSCAT",
    tgt_var = "DSCAT",
    ct_spec = study.ct,
    ct_clst = "C74558",
    id_vars = oak_id_vars()
  ) %>%
  # Map the variable INSTANCE to VISIT
  assign_no_ct(
    raw_dat = ds.prep,
    raw_var = "INSTANCE",
    tgt_var = "VISIT",
    id_vars = oak_id_vars()
  )

# Derive a VISITNUM variable from the created VISIT
visit.map <- ds %>%
  distinct(VISIT) %>%
  mutate(VISITNUM = row_number())
ds <- ds %>%
  left_join(visit.map, by = "VISIT")

# Infer the DSSTDY variable from the DM raw data
# Assumption: we use the date of the data collection COL_DT as the
# sponsor-defined RFSTDTC
dm.raw <- pharmaverseraw::dm_raw 
# Create oak IDs to enable merging with ds
dm.raw <- dm.raw %>%
  generate_oak_id_vars(
    pat_var = "PATNUM",
    raw_src = "dm.raw"
  )
# Convert COL_DT to ISO format to enable the use of derive_study_date
dm.temp <- 
  assign_datetime(
    raw_dat = dm.raw,
    raw_var = "COL_DT",
    tgt_var = "COL_DT_ISO",
    raw_fmt = "m/d/y",
    id_vars = oak_id_vars()
  )
# Get the DSSTDY variable out of the COL_DT_ISO
ds <- ds %>%
  derive_study_day(
    dm_domain = dm.temp,
    tgdt = "DSSTDTC",
    refdt = "COL_DT_ISO",
    study_day_var = "DSSTDY",
    merge_key = "patient_number"
  )

# Map the rest of SDTM derived variables
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
  select(
    "STUDYID", "DOMAIN", "USUBJID", "DSSEQ", "DSTERM", "DSDECOD", "DSCAT", 
    "VISIT", "VISITNUM", "DSDTC", "DSSTDTC", "DSSTDY"
  )

# Save ds domain
write.csv(ds, "question_1_sdtm/ds.csv")






