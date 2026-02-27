# Analytical Data Science Programmer Coding Assessment

This repository contains my solutions to the Coding Assessment for my Analytical Data Science Programmer application.  
The work covers SDTM creation, ADaM derivations, and regulatory-style TLG outputs using modern R and Pharmaverse packages.

The repository structure follows the submission requirements specified in the assessment instructions.

---

# Repository Structure

```
.
├── question_1_sdtm/
│   ├── 01_create_ds_domain.R
│   ├── ds.csv
│   ├── sdtm_ct.csv
│   └── log_question_1_sdtm.txt
│
├── question_2_adam/
│   ├── create_adsl.R
│   ├── adsl.csv
│   └── log_question_2_adam.txt
│
├── question_3_tlg/
│   ├── 01_create_ae_summary_table.R
│   ├── 02_create_visualizations.R
│   ├── ae_summary_table.html
│   ├── plot1.png
│   ├── plot2.png
│   ├── log_question_3_table.txt
│   └── log_question_3_plots.txt
```

Each question is contained within its own folder and includes:

- Script(s)
- Output dataset or visualization
- Log file demonstrating error-free execution

---

# Environment

- **R Version:** ≥ 4.2.0  
- **Primary Packages Used:**
  - sdtm.oak
  - admiral
  - gtsummary
  - ggplot2
  - dplyr
  - gt
  - pharmaverseadam
  - pharmaversesdtm

All scripts are designed to run end-to-end without manual intervention.

---

# Question 1 – SDTM DS Domain Creation

**Folder:** `question_1_sdtm/`  
**Script:** `01_create_ds_domain.R`

## Objective

Create the SDTM Disposition (DS) domain from raw clinical data using `{sdtm.oak}`.

## Key Features

- Input: `pharmaverseraw::ds_raw`
- Applied study controlled terminology (`sdtm_ct.csv`)
- Derived required SDTM variables:
  - STUDYID
  - DOMAIN
  - USUBJID
  - DSSEQ
  - DSTERM
  - DSDECOD
  - DSCAT
  - VISITNUM
  - VISIT
  - DSDTC
  - DSSTDTC
  - DSSTDY
- Followed SDTMIG v3.4 conventions
- Script runs without errors (see log file)

## Output

- `ds.csv`
- `log_question_1_sdtm.txt`

---

# Question 2 – ADaM ADSL Dataset Creation

**Folder:** `question_2_adam/`  
**Script:** `create_adsl.R`

## Objective

Create an ADaM ADSL (Subject-Level) dataset using `{admiral}` and SDTM source data.

## Source Datasets

- `pharmaversesdtm::dm`
- `vs`
- `ex`
- `ds`
- `ae`

## Derived Variables

- **AGEGR9 / AGEGR9N** – Age grouping (<18, 18–50, >50)
- **TRTSDTM / TRTSTMF** – Treatment start datetime with imputation rules applied
- **ITTFL** – Intent-to-Treat flag
- **LSTAVLDT** – Last known alive date derived from:
  - Vital signs
  - AE onset
  - Disposition
  - Treatment exposure

Where applicable, derivations were implemented using `{admiral}` functions following ADaM best practices.

## Output

- `adsl.csv`
- `log_question_2_adam.txt`

---

# Question 3 – TLG: Adverse Events Reporting

**Folder:** `question_3_tlg/`

This section contains regulatory-style Tables and Graphs using ADAE and ADSL datasets.

---

## 3.1 AE Summary Table (FDA Table 10 Style)

**Script:** `01_create_ae_summary_table.R`  
**Output:** `ae_summary_table.html`

### Description

Hierarchical summary table of Treatment-Emergent Adverse Events (TEAEs) created using `{gtsummary}`.

- TEAEs filtered using `TRTEMFL == "Y"`
- Subject-level counts (`id = USUBJID`)
- Rows:
  - AESOC (System Organ Class)
  - AEDECOD (Preferred Term)
- Columns:
  - ACTARM (Treatment groups)
  - Overall column
- Cell values: n (%)
- Sorted in descending frequency using native `sort_hierarchical()`
- Exported to HTML using `{gt}`

### Output Files

- `ae_summary_table.html`
- `log_question_3_table.txt`

---

## 3.2 Plot 1 – AE Severity Distribution by Treatment

**Script:** `02_create_visualizations.R`  
**Output:** `plot1.png`

- Bar chart showing AE severity distribution by treatment arm
- Variable: AESEV
- TEAEs only
- PNG output

---

## 3.3 Plot 2 – Top 10 Most Frequent AEs (Forest Plot)

**Script:** `02_create_visualizations.R`  
**Output:** `plot2.png`

- Top 10 most frequent AEs (subject-level incidence)
- Exact 95% Clopper–Pearson confidence intervals
- Incidence expressed as percentage of subjects
- Ordered by descending frequency
- PNG output

---

# Reproducibility

Each script:

- Runs independently
- Produces its respective output
- Generates a log file confirming error-free execution

No manual editing of outputs was performed.

---
