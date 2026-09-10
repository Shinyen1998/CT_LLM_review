#
##
### Set up ----
   

#
##
### Prepare file for manual abstract screening ----

# all extracted data
abstract_metadata <- read_csv("data/20260311_abstract_metadata.csv")

# select the tested 3%
abstract_95 <- abstract_metadata %>%
  sample_frac(0.03)

# export csv
write_csv(abstract_95, "data/20260313_abstract_metadata_95.csv")

#
##
### Inspection ----

## Import data

abstract_95 <- read_csv("data/20260313_abstract_metadata_95.csv")
abstract_manual_95 <- read_csv("data/20260316_abstract_manual_verification_95.csv")

#
##
### Accuracy for binary ecological variables ----

## sanity check before caculating accuracy
identical(colnames(abstract_95), colnames(abstract_manual_95)) # TRUE
any(is.na(abstract_95[,19:51])) # FALSE
any(is.na(abstract_manual_95[,19:51])) # FALSE

## define columns
cols <- 19:51
vars <- colnames(abstract_95)[cols]

## create table to store accuracy results
ecological_scope_accuracy <- data.frame(
  variable = vars,
  kappa = NA,
  accuracy = NA
)

## calculate Cohen's Kappa and accuracy
for(i in seq_along(cols)){
  
  llm <- abstract_95[[cols[i]]]
  human <- abstract_manual_95[[cols[i]]]
  
  # Cohen's kappa
  k <- kappa2(data.frame(llm, human))$value
  
  # accuracy
  acc <- mean(llm == human, na.rm = TRUE)
  
  ecological_scope_accuracy$kappa[i] <- k
  ecological_scope_accuracy$accuracy[i] <- acc
}

print(ecological_scope_accuracy)

#
##
### Accuracy for other binary variables ----

vars <- c("focal_species_emphasis", "camera_main")

binary_other_accuracy <- data.frame(
  variable = vars,
  kappa = NA,
  accuracy = NA
)

for(i in seq_along(vars)){
  
  llm <- abstract_95[[vars[i]]]
  human <- abstract_manual_95[[vars[i]]]
  
  # Cohen's kappa
  k <- kappa2(data.frame(llm, human))$value
  
  # accuracy
  acc <- mean(llm == human, na.rm = TRUE)
  
  binary_other_accuracy$kappa[i] <- k
  binary_other_accuracy$accuracy[i] <- acc
}

print(binary_other_accuracy)

#
##
### Accuracy for single- and multi-level variables ----

## define columns
factor_cols <- c(
  "country",
  "country_imputed",
  "biome_classified",
  "biome_imputed",
  "spatial_scale",
  "taxonomy_scope",
  "primary_taxonomic_group",
  "scientific_name",
  "focal_species_scientific_name",
  "sampling_method",
  "statistical_method_listed",
  "study_approach",
  "temporal_design"
)

## table to store results
factor_accuracy <- data.frame(variable = factor_cols, kappa = NA, accuracy = NA)

## compute kappa & accuracy
for(col in factor_cols){
  llm <- abstract_95[[col]]
  human <- abstract_manual_95[[col]]
  
  # treat as factor
  llm_f <- as.factor(llm)
  human_f <- as.factor(human)
  
  # Cohen's kappa
  factor_accuracy[factor_accuracy$variable == col, "kappa"] <- kappa2(data.frame(llm_f, human_f))$value
  
  # Accuracy
  factor_accuracy[factor_accuracy$variable == col, "accuracy"] <- mean(llm == human, na.rm = TRUE)
}

print(factor_accuracy)

#
##
### Accuracy for numeric variables ----

## define numeric columns
numeric_cols <- c("deployment_number", "site_number", "trap_night",
                  "temporal_scale_month", "species_count_minimum")

## create table to store accuracy results
numeric_accuracy <- data.frame(
  variable = numeric_cols,
  accuracy = NA
)

## compute exact match (accuracy) for each column
for(i in seq_along(numeric_cols)){
  llm <- abstract_95[[numeric_cols[i]]]
  human <- abstract_manual_95[[numeric_cols[i]]]
  
  acc <- mean(llm == human, na.rm = TRUE)
  numeric_accuracy$accuracy[i] <- acc
}

print(numeric_accuracy)

#
##
### Inspect overall accuracy ----

## NOTE: ALL VERY GOOD!!

print(ecological_scope_accuracy)
print(binary_other_accuracy)
print(factor_accuracy)
print(numeric_accuracy)

## combine into one overview table
# add "type" column to each table
ecological_scope_accuracy$type <- "binary_ecological_scope"
binary_other_accuracy$type <- "other_binary"
factor_accuracy$type <- "factor"
numeric_accuracy$type <- "numeric"

# for numeric_accuracy, add Kappa column as NA to match others
numeric_accuracy$kappa <- NA

# ensure all tables have columns: variable, kappa, accuracy, Type
ecological_scope_accuracy <- ecological_scope_accuracy[, c("type", "variable", "kappa", "accuracy")]
binary_other_accuracy <- binary_other_accuracy[, c("type", "variable", "kappa", "accuracy")]
factor_accuracy <- factor_accuracy[, c("type", "variable", "kappa", "accuracy")]
numeric_accuracy <- numeric_accuracy[, c("type", "variable", "kappa", "accuracy")]

# combine all tables
combined_accuracy <- rbind(
  ecological_scope_accuracy,
  binary_other_accuracy,
  factor_accuracy,
  numeric_accuracy
)

# Sort by type first, then variable
combined_accuracy <- combined_accuracy[order(combined_accuracy$type, combined_accuracy$variable), ]

## inspect
print(combined_accuracy)

# Export to CSV for supplementary info
write.csv(combined_accuracy, "data/20260317_LLM_validation_accuracy_table.csv", row.names = FALSE)




