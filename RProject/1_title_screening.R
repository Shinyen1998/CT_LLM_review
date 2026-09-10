
#
##
### Set up ----

rm(list = ls())

library(readr)
library(stringi)
library(dplyr)
library(stringr)
library(janitor)



#
##
### Prepare file for title screening ----

## Import references
refs_raw <- read_csv("data/20260106_Covidence_merged_databases.csv", locale = locale(encoding = "UTF-8"))

## Normalise to clean UTF-8
refs_raw <- refs_raw |>
  mutate(across(where(is.character), ~ enc2utf8(.)))

refs_raw <- refs_raw |>
  mutate(across(
    where(is.character),
    ~ stri_trans_general(., "Latin-ASCII")
  ))

## Remove duplicated references
# turn all titles into lower cases
refs_clean <- refs_raw %>%
  mutate(
    title_norm = Title %>%
      str_to_lower() %>%
      str_replace_all("[[:punct:]]", " ") %>%
      str_squish()
  )

# identify all caps title
refs_clean <- refs_clean %>%
  mutate(
    is_all_caps = Title == str_to_upper(Title)
  )

table(refs_clean$is_all_caps)
# FALSE  TRUE 
# 4198    97 

# drop duplicated caps rows
refs_dedup <- refs_clean %>%
  arrange(title_norm, is_all_caps) %>%  # FALSE (keep) comes before TRUE (drop)
  distinct(title_norm, .keep_all = TRUE)

# clean the data frame
refs_dedup <- refs_dedup %>%
  select(-title_norm, -is_all_caps)

# sanity check
nrow(refs_raw) - nrow(refs_dedup) # 75 rows dropped
anti_join(refs_raw, refs_dedup, by = "Title")

## Add reference ID
refs_dedup <- refs_dedup %>%
  mutate(ref_id = sprintf("R%04d", row_number())) %>%
  relocate(ref_id, .before = everything())

## Clean column names
refs_dedup <- refs_dedup %>%
  janitor::clean_names()

names(refs_dedup) # all good!

## Get 10% random references for testing
set.seed(123)  # ensures reproducibility

refs_title_test10 <- refs_dedup %>% # randomly select 10% of refs
  sample_frac(0.10)

refs_title_test1 <- refs_dedup %>% # randomly select 10% of refs
  sample_frac(0.01)

## Export cleaned refs csv
write_csv(refs_dedup, "data/20260206_reference_full.csv")

rm(refs_raw, refs_clean)

#
##
### Inspect title screening full ----

title_screened_full <- read_csv("data/20260209_title_screened_full.csv")

table(title_screened_full$title_decision)
# FAIL      PASS UNCERTAIN 
# 96      3988       136

title_screened_full_pass <- title_screened_full %>%
  filter(title_screened_full$title_decision == "PASS")
title_screened_full_pass$title

title_screened_full_fail <- title_screened_full %>%
  select(ref_id, title, abstract, title_reason) %>%
  filter(title_screened_full$title_decision == "FAIL")
title_screened_full_fail$title
title_screened_full_fail$title_reason

title_screened_full_uncertain <- title_screened_full %>%
  select(ref_id, title, abstract, title_decision, title_reason) %>%
  filter(title_screened_full$title_decision == "UNCERTAIN")
title_screened_full_uncertain$title
title_screened_full_uncertain$title_reason

#
##
### Export PASSED and UNCERTAIN studies

title_pass_uncertain <- title_screened_full %>%
  filter(title_decision %in% c("PASS", "UNCERTAIN"))

write_csv(title_pass_uncertain, "data/20260209_reference_abstract_full.csv")


