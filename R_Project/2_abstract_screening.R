
#
##
### Set up ----

rm(list = ls())

library(tidyr)
library(readr)
library(stringi)
library(dplyr)
library(stringr)
library(janitor)



#
##
### Import abstract screening outcomes ----

abstract_screened_full <- read_csv("data/20260211_abstract_screened_full.csv", locale = locale(encoding = "UTF-8"))

## Inspect decisions
anyNA(abstract_screened_full$abstract_decision) #FALSE
table(abstract_screened_full$abstract_decision)
# FAIL      PASS UNCERTAIN 
# 459      3161       504



#
##
### Inspect PASS studies (#3161) ----

abstract_screened_pass <- abstract_screened_full %>%
  filter(abstract_screened_full$abstract_decision == "PASS") %>%
  select(ref_id, title, abstract, abstract_decision, abstract_reason, abstract_reason_tag, confidence)

# inspect low confidence studies (NAs are human classified)
summary(abstract_screened_pass$confidence)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
#   80.00   85.00   90.00   87.75   90.00   90.00       4 

pass_low_confidence <- abstract_screened_pass %>%
  filter(abstract_screened_pass$confidence <= 80)
# seems like some sampling methods comparison studies along with monitoring has passed
# will deal with it later



#
##
### Inspect FAIL studies (#459) ----

abstract_screened_fail <- abstract_screened_full %>%
  filter(abstract_screened_full$abstract_decision == "FAIL") %>%
  select(ref_id, title, abstract, abstract_decision, abstract_reason, abstract_reason_tag, confidence)

# inspect exclusion reasons
tag_freq <- abstract_screened_fail %>%
  select(abstract_reason_tag) %>%
  filter(!is.na(abstract_reason_tag)) %>%
  separate_rows(abstract_reason_tag, sep = ";") %>%
  mutate(abstract_reason_tag = str_trim(abstract_reason_tag)) %>%
  count(abstract_reason_tag, sort = TRUE)

tag_freq
# 1 ecological_scope      245
# 2 sampling_method       142
# 3 target_organism       103
# 4 study_type             49
# 5 abstract_missing       23

# rearrange exclusion reason and keep only one tag per study
# define priority
tag_priority <- c(
  "abstract_missing" = 1, # high priority
  "study_type" = 2,
  "target_organism" = 3,
  "ecological_scope" = 4,
  "sampling_method" = 5 # low priority
)

# drop lower priority tags
abstract_screened_fail <- abstract_screened_fail %>%
  separate_rows(abstract_reason_tag, sep = ";") %>%
  mutate(abstract_reason_tag = str_trim(abstract_reason_tag)) %>%
  mutate(tag_rank = tag_priority[abstract_reason_tag]) %>%
  group_by(ref_id) %>%
  slice_min(tag_rank, with_ties = FALSE) %>%
  ungroup() %>%
  select(-tag_rank)

# inspect exclusion tags
anyNA(abstract_screened_fail$abstract_reason_tag) # FALSE
table(abstract_screened_fail$abstract_reason_tag)
# abstract_missing ecological_scope  sampling_method  study_type  target_organism 
# 23              223               63               49              101

# inspect low confidence studies (NAs are either missing_abstract or human classified)
summary(abstract_screened_fail$confidence)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
#   85.00   90.00   90.00   90.69   90.00   95.00      24 

fail_low_confidence <- abstract_screened_fail %>%
  filter(abstract_screened_fail$confidence <= 85) # all looks well categorized!



#
##
### Inspect UNCERTAIN studies (#504)

abstract_screened_uncertain <- abstract_screened_full %>%
  filter(abstract_screened_full$abstract_decision == "UNCERTAIN") %>%
  select(ref_id, title, abstract, abstract_decision, abstract_reason, abstract_reason_tag, confidence)

# inspect low confidence studies (NAs are human classified)
summary(abstract_screened_uncertain$confidence)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 40.00   60.00   60.00   59.48   60.00   65.00

# many deal with method- or management-validation
# I think it's safe to remove them



#
##
### Keep only PASS studies and export .csv

abstract_screened_pass <- abstract_screened_full %>%
  filter(abstract_screened_full$abstract_decision == "PASS") %>%
  select(ref_id, title, authors, abstract, published_year, journal, doi)

write_csv(abstract_screened_pass, "data/20260212_reference_abstract_pass.csv")


