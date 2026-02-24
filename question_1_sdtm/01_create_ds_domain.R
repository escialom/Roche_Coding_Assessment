library(pharmaverseraw)
library(sdtm.oak)

# Read raw dataset and load study CT
ds.raw <- pharmaverseraw::ds_raw
study.ct <- read.csv("question_1/sdtm_ct.csv")
