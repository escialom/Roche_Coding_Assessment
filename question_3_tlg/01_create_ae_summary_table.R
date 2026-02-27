library(gtsummary)
library(pharmaverseadam)
library(dplyr)
library(gt)


# Load ADaM datasets from {pharmaverseadam} 
adae <- pharmaverseadam::adae
adsl <- pharmaverseadam::adsl

# Get the records with treatment emergent adversive events
adae.teae <- adae %>%
  filter(TRTEMFL == "Y")

# Get the summary table of sorted TEAEs
tbl <- adae.teae %>%
  tbl_hierarchical(
    variables = c(AESOC, AEDECOD),
    by = ACTARM,
    id = USUBJID,
    denominator = adsl,
    overall_row = TRUE,
    label = "..ard_hierarchical_overall.." ~ "Treatment Emergent AEs"
  ) %>%
  sort_hierarchical(sort = "descending")

# Save table 
as_gt(tbl) %>% 
  gt::gtsave("ae_summary_table.html")
