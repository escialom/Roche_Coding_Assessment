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



