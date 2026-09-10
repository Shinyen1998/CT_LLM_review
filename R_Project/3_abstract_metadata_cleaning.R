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
library(countrycode)
library(rnaturalearthdata)
library(sf)
library(ggplot2)

#
##
### Import abstract extraction outcomes ----

abstract_metadata_raw <- read_csv("data/20260309_abstract_metadata_raw.csv", locale = locale(encoding = "UTF-8"))

#
##
### Modify data structure ----

## convert data - character, factor, and intergers

abstract_metadata <- abstract_metadata_raw %>%
  mutate(
    ref_id = as.character(ref_id),
    authors = as.character(authors),
    published_year = as.integer(published_year),
    doi = as.character(doi),
    title = as.character(title),
    abstract = as.character(abstract),
    country = as.character(country),
    country_imputed = as.character(country_imputed),
    biome_classified = as.character(biome_classified),
    biome_imputed = as.character(biome_imputed),
    spatial_scale = as.factor(spatial_scale),
    deployment_number = as.integer(deployment_number),
    site_number = as.integer(site_number),
    trap_night = as.integer(trap_night),
    temporal_scale_month = as.integer(temporal_scale_month),
    taxonomy_scope = as.factor(taxonomy_scope),
    primary_taxonomic_group = as.factor(primary_taxonomic_group),
    scientific_name = as.character(scientific_name),
    species_count_minimum = as.integer(species_count_minimum),
    focal_species_emphasis = as.numeric(focal_species_emphasis),
    focal_species_scientific_name = as.character(focal_species_scientific_name),
    sampling_method = as.character(sampling_method),
    camera_main = as.numeric(camera_main),
    statistical_method_listed = as.character(statistical_method_listed),
    statistical_method_other = as.character(statistical_method_other),
    study_approach = as.factor(study_approach),
    temporal_design = as.factor(temporal_design),
    extraction_confidence = as.integer(extraction_confidence)
  )

## format ecological scope

anyNA(abstract_metadata_raw[, 23:55]) # T

na_rows <- abstract_metadata %>%
  filter(if_any(23:55, is.na)) # NAs are all just zeros!

abstract_metadata <- abstract_metadata %>%
  mutate(
    across(23:55, ~ as.numeric(ifelse(is.na(.x), 0, .x)))
  )

anyNA(abstract_metadata[, 23:55]) # F
glimpse(abstract_metadata)

rm(na_rows)

#
##
### Exclude duplicated rows ----

# R0348 & R0349
# 


#
##
### Exclude camera_main = 0 ----

anyNA(abstract_metadata$camera_main) # F
table(abstract_metadata$camera_main)
# 0    1 
# 141 3020

# inspect these studies
camera_minor <- abstract_metadata %>%
  filter(abstract_metadata$camera_main == 0)
table(camera_minor$sampling_method)
# nice classification, safe to remove

abstract_metadata <- abstract_metadata %>%
  filter(abstract_metadata$camera_main == 1)

#
##
### Inspect data: published_year ----

anyNA(abstract_metadata$published_year) # F

hist(abstract_metadata$published_year)

#
##
### Inspect data: country & country_imputed ----

## standardize country 

sum(!is.na(abstract_metadata$country)) # 2509
table(abstract_metadata$country) # need standardization

# expand into long format
country_long <- abstract_metadata %>%
  separate_rows(country, sep = ";")

country_long <- country_long %>%
  mutate(country_std = countrycode(country,
                                   origin = "country.name",
                                   destination = "country.name"))

# inspect unmatched names and manually fix them
unique(country_long$country[is.na(country_long$country_std)])

country_long <- country_long %>%
  mutate(country = case_when(
    country %in% c("England", "Scotland", "Wales") ~ "United Kingdom",
    country %in% c("Hawai'i", "Hawaiian Islands") ~ "United States",
    country == "Scandinavia" ~ "Norway;Sweden;Denmark",
    country == "Borneo" ~ "Indonesia;Malaysia;Brunei",
    country == "S" ~ NA_character_,
    TRUE ~ country
  ))

country_long <- country_long %>%
  separate_rows(country, sep = ";")

country_long <- country_long %>%
  mutate(country_std = countrycode(country,
                                   origin = "country.name",
                                   destination = "country.name"))

unique(country_long$country[is.na(country_long$country_std)]) # NA, all good!

# collapse back to unique ref_id per row

abstract_metadata <- country_long %>%
  group_by(ref_id) %>%
  mutate(country = paste(sort(unique(country_std)), collapse = ";")) %>%
  ungroup() %>%
  distinct(ref_id, .keep_all = TRUE) %>%
  select(-country_std) %>%
  mutate(country = na_if(country, ""))


## standardize country_imputed

sum(!is.na(abstract_metadata$country_imputed)) # 107
table(abstract_metadata$country) # need standardization

# expand into long format
country_imputed_long <- abstract_metadata %>%
  separate_rows(country_imputed, sep = ";")

country_imputed_long <- country_imputed_long %>%
  mutate(country_std = countrycode(country_imputed,
                                   origin = "country.name",
                                   destination = "country.name"))

# inspect unmatched names and manually fix them
unique(country_imputed_long$country_imputed[is.na(country_imputed_long$country_std)])

country_imputed_long <- country_imputed_long %>%
  mutate(country_imputed = case_when(
    country_imputed == "Borneo" ~ "Indonesia;Malaysia;Brunei",
    country_imputed %in% c("Africa", "global") ~ NA_character_, # information insufficient
    TRUE ~ country_imputed
  ))

country_imputed_long <- country_imputed_long %>%
  separate_rows(country_imputed, sep = ";")

country_imputed_long <- country_imputed_long %>%
  mutate(country_std = countrycode(country_imputed,
                                   origin = "country.name",
                                   destination = "country.name"))

unique(country_long$country[is.na(country_long$country_std)]) # NA, all good!

# collapse back to unique ref_id per row

abstract_metadata <- country_imputed_long %>%
  group_by(ref_id) %>%
  mutate(country_imputed = paste(sort(unique(country_std)), collapse = ";")) %>%
  ungroup() %>%
  distinct(ref_id, .keep_all = TRUE) %>%
  select(-country_std) %>%
  mutate(country_imputed = na_if(country_imputed, ""))

# check country_imputed accuracy
country_imputed <- abstract_metadata %>%
  filter(!is.na(abstract_metadata$country_imputed)) # looks pretty accurate!

# check if they all are country = NA
sum(is.na(country_imputed$country)) # 107, great!


## integrate country and country_imputed

abstract_metadata <- abstract_metadata %>%
  mutate(
    country = na_if(trimws(country), ""),
    country_imputed = na_if(trimws(country_imputed), ""),
    country_combined = coalesce(country_imputed, country),
    .after = country_imputed
  )

sum(!is.na(abstract_metadata$country_combined)) # 2615


## inspect

country_combined_long <- abstract_metadata %>%
  separate_rows(country_combined, sep = ";")

country_combined_long <- country_combined_long %>%
  filter(!is.na(country_combined_long$country_combined))

# create table
country_count <- country_combined_long %>%
  count(country_combined, sort = TRUE) %>%
  rename(frequency = n)

#
##
### Inspect data: biome_classified & biome_imputed ----

sum(!is.na(abstract_metadata$biome_classified)) # 947
table(abstract_metadata$biome_classified) # need standardization

sum(!is.na(abstract_metadata$biome_imputed)) # 1337
table(abstract_metadata$biome_imputed) # need standardization

## standardize biome_classified

biome_classified_long <- abstract_metadata %>%
  separate_rows(biome_classified, sep = ";")

table(biome_classified_long$biome_classified)

biome_classified_long <- biome_classified_long %>% # manually inspect their abstract
  mutate(biome_classified = case_when(
    biome_classified %in% c("dry_forest", "tropical_dry_forest") ~ "shrubland",
    biome_classified == "savanna" ~ "grassland",
    biome_classified %in% c("wetland", "marshland") ~ NA_character_, # information insufficient
    TRUE ~ biome_classified
  ))

abstract_metadata <- biome_classified_long %>%
  group_by(ref_id) %>%
  mutate(biome_classified = paste(sort(unique(biome_classified)), collapse = ";")) %>%
  ungroup() %>%
  distinct(ref_id, .keep_all = TRUE) %>%
  mutate(biome_classified = na_if(biome_classified, ""))


## standardize biome_imputed

biome_imputed_long <- abstract_metadata %>%
  separate_rows(biome_imputed, sep = ";")

table(biome_imputed_long$biome_imputed)

biome_imputed_long <- biome_imputed_long %>% # manually inspect their abstract
  mutate(biome_imputed = case_when(
    biome_imputed %in% c("dry_forest", "tropical_dry_forest") ~ "shrubland",
    biome_imputed == "boreal_forest" ~ "coniferous_forest",
    biome_imputed %in% c("savannah", "savanna") ~ "grassland",
    biome_imputed == "montane_forest" ~ "rainforest",
    biome_imputed %in% c("coastal", "karst", "mountain", "tropical", "tropical_forest", "urban", "wetland") ~ NA_character_, # information insufficient
    TRUE ~ biome_imputed
  ))

abstract_metadata <- biome_imputed_long %>%
  group_by(ref_id) %>%
  mutate(biome_imputed = paste(sort(unique(biome_imputed)), collapse = ";")) %>%
  ungroup() %>%
  distinct(ref_id, .keep_all = TRUE) %>%
  mutate(biome_imputed = na_if(biome_imputed, ""))

sum(!is.na(biome_imputed_long$biome_classified))

# check biome_imputed accuracy
biome_imputed <- abstract_metadata %>%
  filter(!is.na(abstract_metadata$biome_imputed)) # looks pretty accurate!

# check if they all are biome = NA
sum(is.na(biome_imputed$biome_classified)) # 1326, great!

## integrate biome and biome_imputed

abstract_metadata <- abstract_metadata %>%
  mutate(
    biome_classified = na_if(trimws(biome_classified), ""),
    biome_imputed = na_if(trimws(biome_imputed), ""),
    biome_combined = coalesce(biome_imputed, biome_classified),
    .after = biome_imputed
  )

sum(!is.na(abstract_metadata$biome_combined)) # 2271


## inspect

biome_combined_long <- abstract_metadata %>%
  separate_rows(biome_combined, sep = ";")

biome_combined_long <- biome_combined_long %>%
  filter(!is.na(biome_combined_long$biome_combined))

# create table
biome_count <- biome_combined_long %>%
  count(biome_combined, sort = TRUE) %>%
  rename(frequency = n)

#
##
### Inspect data: spatial_scale ----

sum(is.na(abstract_metadata$spatial_scale)) # 34
table(abstract_metadata$spatial_scale) # need standardization

# convert "statewide" to "regional"
abstract_metadata$spatial_scale <- as.character(abstract_metadata$spatial_scale)
abstract_metadata$spatial_scale[abstract_metadata$spatial_scale == "statewide"] <- "regional"

# create table
spatial_scale_count <- abstract_metadata %>%
  filter(!is.na(spatial_scale)) %>%
  count(spatial_scale, sort = TRUE) %>%
  rename(frequency = n)

#
##
### Inspect data: deployment_number, site_number, trap_night, temporal_scale_month, study_approach, temporal_design ----

summary(abstract_metadata$deployment_number)

summary(abstract_metadata$site_number)

summary(abstract_metadata$trap_night) # 50000 is correct

summary(abstract_metadata$temporal_scale_month) # 720, 444 are wrong, NA instead

abstract_metadata$temporal_scale_month[abstract_metadata$temporal_scale_month == 720] <- NA
abstract_metadata$temporal_scale_month[abstract_metadata$temporal_scale_month == 444] <- NA

sum(is.na(abstract_metadata$study_approach)) # 0
table(abstract_metadata$study_approach)

study_approach_count <- abstract_metadata %>%
  filter(!is.na(study_approach)) %>%
  count(study_approach, sort = TRUE) %>%
  rename(frequency = n)

sum(is.na(abstract_metadata$temporal_design)) # 671
table(abstract_metadata$temporal_design)

temporal_design_count <- abstract_metadata %>%
  filter(!is.na(temporal_design)) %>%
  count(temporal_design, sort = TRUE) %>%
  rename(frequency = n)

#
##
### Inspect data: sampling_method ----

anyNA(abstract_metadata$sampling_method) # F

# standardize methods

sampling_method_long <- abstract_metadata %>%
  separate_rows(sampling_method, sep = ";")

table(sampling_method_long$sampling_method)

sampling_method_long <- sampling_method_long %>%
  mutate(sampling_method = case_when(
    sampling_method %in% c("direct_observation", "interview", "ground_survey", "point_count", "questionnaire", "scat", "spotlight", "survey", "visual", "visual_survey") ~ "other",
    sampling_method == "radio_telemetry" ~ "focal_GPS",
    sampling_method %in% c("capture_recapture", "distance_sampling", "ecological_survey", "exclosure", "focal_observation", "fecal_pellets", "mark_resight", "marking", "plot", "pole_camera") ~ NA_character_,
    TRUE ~ sampling_method
  ))

table(sampling_method_long$sampling_method) # good

sampling_method_long <- sampling_method_long %>%
  distinct()

abstract_metadata <- sampling_method_long %>%
  group_by(ref_id) %>%
  mutate(sampling_method = paste(sort(unique(sampling_method)), collapse = ";")) %>%
  ungroup() %>%
  distinct(ref_id, .keep_all = TRUE) %>%
  mutate(sampling_method = na_if(sampling_method, ""))

# R2722 is missing camera record
abstract_metadata <- abstract_metadata %>%
  mutate(sampling_method = if_else(
    ref_id == "R2722",
    "acoustics;transect;camera",
    sampling_method
  ))

# inspect

sampling_method_count <- sampling_method_long %>%
  filter(!is.na(sampling_method)) %>%
  count(sampling_method, sort = TRUE) %>%
  rename(frequency = n)

#
##
### Inspect data: taxonomy_scope, primary_taxonomic_group, species_count_minimum ----

sum(is.na(abstract_metadata$taxonomy_scope)) # 1
table(abstract_metadata$taxonomy_scope)

taxonomy_scope_count <- abstract_metadata %>%
  filter(!is.na(taxonomy_scope)) %>%
  count(taxonomy_scope, sort = TRUE) %>%
  rename(frequency = n)

sum(is.na(abstract_metadata$primary_taxonomic_group)) # 2
table(abstract_metadata$primary_taxonomic_group) # need standardization

abstract_metadata$primary_taxonomic_group = as.character(abstract_metadata$primary_taxonomic_group)
abstract_metadata$primary_taxonomic_group[abstract_metadata$primary_taxonomic_group == "mammal;bird"] <- "multi_taxa"
abstract_metadata$primary_taxonomic_group[abstract_metadata$primary_taxonomic_group == "vertebrate"] <- "multi_taxa"

primary_taxonomic_group_count <- abstract_metadata %>%
  filter(!is.na(primary_taxonomic_group)) %>%
  count(primary_taxonomic_group, sort = TRUE) %>%
  rename(frequency = n)

sum(is.na(abstract_metadata$focal_species_emphasis)) # 0
table(abstract_metadata$focal_species_emphasis)

summary(abstract_metadata$species_count_minimum)

#
##
### Inspect data: ecological scopes (25-57) ----

anyNA(abstract_metadata[, 25:57]) # F

# select columns 24-56
scope_cols <- abstract_metadata[, 25:57]

# create summary table
ecological_scope_count <- data.frame(
  ecological_scope = colnames(scope_cols),
  frequency = colSums(scope_cols == 1, na.rm = TRUE)
)

#
##
### Inspect data: statistical_methods_listed, statistical_method_other  ----

sum(!is.na(abstract_metadata$statistical_method_listed)) # 2011
table(abstract_metadata$statistical_method_listed) # need standardization

statistical_method_listed_long <- abstract_metadata %>%
  separate_rows(statistical_method_listed, sep = ";")

table(statistical_method_listed_long$statistical_method_listed) # need LOTS OF standardization

## reassign methods

statistical_method_listed_long <- statistical_method_listed_long %>%
  mutate(
    
    statistical_method_listed_fix = case_when(
      
      # already valid levels
      statistical_method_listed %in% c(
        "abundance_model",
        "activity_overlap_model",
        "co_occurrence_model",
        "density_model",
        "distance_sampling",
        "gam",
        "glm",
        "glmm",
        "JSDM",
        "mark_resight",
        "multi_scale_occupancy",
        "multi_season_occupancy",
        "multi_species_occupancy",
        "N_mixture",
        "occupancy_model",
        "RAI",
        "REM",
        "Royle_Nichols",
        "SCR",
        "scr_bayesian",
        "SDM",
        "SEM",
        "TTE",
        "other"
      ) ~ statistical_method_listed,
      
      # MaxEnt standardization
      statistical_method_listed == "Maxent" ~ "MaxEnt",
      
      # occupancy variants
      statistical_method_listed %in% c(
        "multi_method_occupancy",
        "multi_state_occupancy"
      ) ~ "occupancy_model",
      
      # activity models
      statistical_method_listed %in% c(
        "diel_activity_model",
        "kernel_density"
      ) ~ "activity_overlap_model",
      
      # capture recapture too vague
      statistical_method_listed == "capture_recapture" ~ NA,
      
      # everything else -> other
      statistical_method_listed %in% c(
        "ANOSIM",
        "community_composition",
        "community_hierarchical_model",
        "detectability",
        "diversity_or_evenness",
        "model_averaging",
        "modeling",
        "PERMANOVA",
        "population_growth_rate_model",
        "RDA",
        "RLQ",
        "SIMPER"
      ) ~ "other",
      
      TRUE ~ statistical_method_listed
    ),
    
    
    # store original name when mapped to OTHER
    statistical_method_other_fix = if_else(
      statistical_method_listed_fix == "other" & statistical_method_listed != "other",
      statistical_method_listed,
      NA_character_
    )
    
  )

## clean the hierarchical structure

statistical_method_listed_long <- statistical_method_listed_long %>%
  group_by(ref_id) %>%
  mutate(
    # mark original NAs
    original_na = is.na(statistical_method_listed_fix)
  ) %>%
  mutate(
    statistical_method_listed_fix = case_when(
      
      # remove generic occupancy if specific occupancy exists
      statistical_method_listed_fix == "occupancy_model" & 
        any(statistical_method_listed_fix %in% c("multi_season_occupancy","multi_species_occupancy","multi_scale_occupancy")) ~ NA_character_,
      
      # remove generic abundance if specific abundance exists
      statistical_method_listed_fix == "abundance_model" &
        any(statistical_method_listed_fix %in% c("N_mixture","Royle_Nichols")) ~ NA_character_,
      
      # remove generic density if specific density exists
      statistical_method_listed_fix == "density_model" &
        any(statistical_method_listed_fix %in% c("SCR","scr_bayesian","distance_sampling","REM","TTE","mark_resight")) ~ NA_character_,
      
      # remove SCR if Bayesian SCR exists
      statistical_method_listed_fix == "SCR" & any(statistical_method_listed_fix == "scr_bayesian") ~ NA_character_,
      
      TRUE ~ statistical_method_listed_fix
    )
  ) %>%
  # remove rows where hierarchy replacement produced NA, but keep original NAs
  filter(!(is.na(statistical_method_listed_fix) & !original_na)) %>%
  ungroup() %>%
  select(-original_na)

## add other method back to the original column

statistical_method_listed_long <- statistical_method_listed_long %>%
  mutate(
    statistical_method_other = case_when(
      is.na(statistical_method_other) & !is.na(statistical_method_other_fix) ~ 
        statistical_method_other_fix,
      !is.na(statistical_method_other) & !is.na(statistical_method_other_fix) ~ 
        paste0(statistical_method_other, ";", statistical_method_other_fix),
      TRUE ~ statistical_method_other  # keep as is if fix is NA
    )
  )

## Create the cleaned metadata

# drop and replace unwanted columns
statistical_method_listed_long <- statistical_method_listed_long %>%
  select(-statistical_method_other_fix)

statistical_method_listed_long <- statistical_method_listed_long %>%
  mutate(statistical_method_listed = statistical_method_listed_fix) %>%
  select(-statistical_method_listed_fix)

# remove duplicates just in case
statistical_method_listed_long <- statistical_method_listed_long %>%
  distinct()

# collapse back
abstract_metadata <- statistical_method_listed_long %>%
  group_by(ref_id) %>%
  mutate(statistical_method_listed = paste(sort(unique(statistical_method_listed)), collapse = ";")) %>%
  ungroup() %>%
  distinct(ref_id, .keep_all = TRUE) %>%
  mutate(statistical_method_listed = na_if(statistical_method_listed, ""))


## standardize other methods

statistical_method_other_long <- abstract_metadata %>%
  separate_rows(statistical_method_other, sep = ";")

# check if "other" were correctly recorded 
d <- abstract_metadata %>%
  filter(!is.na(statistical_method_other))
table(d$statistical_method_listed) # all good!

## map obvious methods to valid categories

statistical_method_other_mapping <- tribble(
  ~statistical_method_other, ~statistical_method_other_mapped,
  
  # ---- activity overlap methods (temporal only) ----
  "95% adaptive Kernel density estimator", "activity_overlap_model",
  "activity kernels", "activity_overlap_model",
  "activity overlap analysis", "activity_overlap_model",
  "activity patterns", "activity_overlap_model",
  "coefficient of temporal overlap", "activity_overlap_model",
  "coefficients of activity overlap", "activity_overlap_model",
  "Dhat overlap analysis", "activity_overlap_model",
  "diel-vertical overlap analysis", "activity_overlap_model",
  "temporal activity analysis", "activity_overlap_model",
  "temporal activity kernel density curves", "activity_overlap_model",
  "temporal overlap analysis", "activity_overlap_model",
  "temporal overlap coefficient", "activity_overlap_model",
  
  # ---- REM / REST ----
  "random encounter and staying time (REST) model", "REM",
  
  # ---- capture–recapture frameworks ----
  "Barker robust design model", "mark_resight",
  "capture-recapture analyses", "mark_resight",
  "capture-recapture models for closed populations", "mark_resight",
  "mark-recapture", "mark_resight",
  
  # ---- relative abundance indices ----
  "photo rates", "RAI",
  "abundance index", "RAI",
  
  # ---- co-occurrence ----
  "co-occurrence activity analysis", "co_occurrence_model",
  "spatial cooccurrence pattern", "co_occurrence_model",
  
  # ---- Important: added the kernel density level ----
  "kernel-density estimator", "kernel_density",
  "kernel density", "kernel_density",
  "kernel density approach", "kernel_density",
  "kernel density estimates", "kernel_density",
  "Kernel Density estimates", "kernel_density",
  "kernel density estimation", "kernel_density",
  "Kernel density estimation", "kernel_density",
  "kernel density estimations", "kernel_density",
  "kernel density functions", "kernel_density",
  "Kernel density functions", "kernel_density",
  "Kernel density method", "kernel_density",
  "kernel density plots", "kernel_density",
  "circular kernel density models", "kernel_density",
  "univariate kernel density estimates", "kernel_density"
)

# map columns
statistical_method_other_long <- statistical_method_other_long %>%
  left_join(statistical_method_other_mapping, 
            by = "statistical_method_other")

# safety check to see how those remapped methods were originally classified
d <- statistical_method_other_long %>%
  select(ref_id, statistical_method_listed, statistical_method_other, statistical_method_other_mapped) %>%
  filter(!is.na(statistical_method_other_mapped))
table(d$statistical_method_listed) # good, all only "other", with only one problematic R4018

d <- statistical_method_other_long %>%
  select(ref_id, statistical_method_listed, statistical_method_other) %>%
  filter(ref_id == "R4018") # checked, safe to just remove the other

statistical_method_other_long <- statistical_method_other_long %>%
  mutate(statistical_method_other = if_else(ref_id == "R4018", NA_character_, statistical_method_other),
         statistical_method_other_mapped = if_else(ref_id == "R4018", NA_character_, statistical_method_other_mapped),
         statistical_method_listed = if_else(ref_id == "R4018", "REM;TTE", statistical_method_listed))

## replace the method with the mapped value

statistical_method_other_long <- statistical_method_other_long %>%
  mutate(
    statistical_method_listed = if_else(
      !is.na(statistical_method_other_mapped),
      statistical_method_other_mapped,
      statistical_method_listed
    ),
    statistical_method_other = if_else(
      !is.na(statistical_method_other_mapped),
      NA_character_,
      statistical_method_other
    )
  )

## Collapse back

# drop the mapping column
statistical_method_other_long <- statistical_method_other_long %>%
  select(-statistical_method_other_mapped)

# remove duplicates just in case
statistical_method_listed_long <- statistical_method_listed_long %>%
  distinct()

# collapse back
abstract_metadata <- statistical_method_other_long %>%
  group_by(ref_id) %>%
  mutate(statistical_method_other = paste(sort(unique(statistical_method_other)), collapse = ";")) %>%
  ungroup() %>%
  distinct(ref_id, .keep_all = TRUE) %>%
  mutate(statistical_method_other = na_if(statistical_method_other, ""))


## check again if "other" were correctly recorded 
d <- abstract_metadata %>%
  filter(!is.na(statistical_method_other))
table(d$statistical_method_listed) # all good!

# add ";other" back to those kernel density ones
abstract_metadata <- abstract_metadata %>%
  mutate(
    statistical_method_listed = case_when(
      !is.na(statistical_method_other) & statistical_method_listed == "kernel_density" ~ "kernel_density;other",
      TRUE ~ statistical_method_listed
    )
  )

## Inspect statistical methods

statistical_method_listed_long <- abstract_metadata %>%
  separate_rows(statistical_method_listed, sep = ";")

statistical_method_other_long <- abstract_metadata %>%
  separate_rows(statistical_method_other, sep = ";")

statistical_method_count <- statistical_method_listed_long %>%
  filter(!is.na(statistical_method_listed_long$statistical_method_listed)) %>%
  count(statistical_method_listed, sort = TRUE) %>%
  rename(frequency = n)

statistical_method_other_count <- statistical_method_other_long %>%
  filter(!is.na(statistical_method_other_long$statistical_method_other)) %>%
  count(statistical_method_other, sort = TRUE) %>%
  rename(frequency = n)

#
##
### Inspect data: extraction_confidence ----

anyNA(abstract_metadata$extraction_confidence) # F
table(abstract_metadata$extraction_confidence)

#
##
### Now check everything again! ----

abstract_metadata <- abstract_metadata %>%
  select(-country_combined, -biome_combined) %>%
  mutate(
    ref_id = as.character(ref_id),
    authors = as.character(authors),
    published_year = as.integer(published_year),
    doi = as.character(doi),
    title = as.character(title),
    abstract = as.character(abstract),
    country = as.character(country),
    country_imputed = as.character(country_imputed),
    biome_classified = as.character(biome_classified),
    biome_imputed = as.character(biome_imputed),
    spatial_scale = as.factor(spatial_scale),
    deployment_number = as.integer(deployment_number),
    site_number = as.integer(site_number),
    trap_night = as.integer(trap_night),
    temporal_scale_month = as.integer(temporal_scale_month),
    taxonomy_scope = as.factor(taxonomy_scope),
    primary_taxonomic_group = as.factor(primary_taxonomic_group),
    scientific_name = as.character(scientific_name),
    species_count_minimum = as.integer(species_count_minimum),
    focal_species_emphasis = as.numeric(focal_species_emphasis),
    focal_species_scientific_name = as.character(focal_species_scientific_name),
    sampling_method = as.character(sampling_method),
    camera_main = as.numeric(camera_main),
    statistical_method_listed = as.character(statistical_method_listed),
    statistical_method_other = as.character(statistical_method_other),
    study_approach = as.factor(study_approach),
    temporal_design = as.factor(temporal_design),
    extraction_confidence = as.integer(extraction_confidence)
  )

anyDuplicated(abstract_metadata_raw$ref_id) #0
anyDuplicated(abstract_metadata_raw$title) #0

glimpse(abstract_metadata)

#
##
### export data ----

write_csv(abstract_metadata, "data/20260311_abstract_metadata.csv")


