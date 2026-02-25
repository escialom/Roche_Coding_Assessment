library(pharmaverseraw)
library(sdtm.oak)
library(dplyr)

# Read raw dataset and load study CT
ds.raw <- pharmaverseraw::ds_raw
study.ct <- read.csv("question_1_sdtm/sdtm_ct.csv")

# Create oak IDs to enable topic variable mapping
ds.raw <- ds.raw %>%
  generate_oak_id_vars(
    pat_var = "PATNUM",
    raw_src = "ds.raw"
  )

# Map the topic variable
# Reflects the collected observation, not the standardized term
# No ct because this is the verbatim
ds <-
  assign_no_ct(
    raw_dat = ds.raw,
    raw_var = "IT.DSTERM",
    tgt_var = "DSTERM",
    id_vars = oak_id_vars()
  )

# Prep the mapping of other variables
# TODO: Later, write a function to extract the codes
# Create temporary df to enable direct mapping
ds.temp <- ds.raw
# Create a temporary DSCAT variable which should be later mapped in ds
ds.temp <- ds.temp %>%
  mutate(
    TEMP.DSCAT = case_when(
      IT.DSDECOD == "Randomized" ~ "PROTOCOL MILESTONE",
      TRUE ~ "DISPOSITION EVENT"
    )
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
  # Map the value of "IT.DSDECOD" in "DSDECOD" when OTHERSP is null
  assign_ct(
    raw_dat = ds.raw,
    raw_var = "IT.DSDECOD",
    tgt_var = "DSDECOD",
    ct_spec = study.ct,
    ct_clst = "C66727",
    id_vars = oak_id_vars()
  ) %>%
  # Map the values in "DSCAT"
  assign_ct(
    raw_dat = ds.temp,
    raw_var = "TEMP.DSCAT",
    tgt_var = "DSCAT",
    ct_spec = study.ct,
    ct_clst = "C74558",
    id_vars = oak_id_vars()
  )









# Map the variables involving controlled terminology
# Only "DSDECOD","DSCAT" involves CT
ds <- ds %>%
  assign_ct(
    raw_dat = ds.raw,
    raw_var = "IT.DSDECOD",
    tgt_var = "DSDECOD",
    ct_spec = study.ct,
    ct_clst = "C66727",
    id_vars = oak_id_vars()
  ) 


