
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
library(ComplexUpset)
library(ggVennDiagram)
library(patchwork)
library(WDI)
library(rgbif)
library(purrr)
library(httr2)
library(jsonlite)
library(devtools)
library(rredlist)
library(traitdata)
library(vegan)
library(scales)
library(openxlsx)
library(effectsize)

metadata <- read.csv("data/20260409_abstract_metadata.csv")

names(metadata)
glimpse(metadata)


## Read data
country_lookup <- read.csv("country_lookup_04092026.csv")
gbif_lookup <- read.csv("gbif_lookup_27082026.csv")
iucn_lookup <- read.csv("iucn_lookup_27082026.csv")
traits_lookup <- read.csv("traits_lookup_27082026.csv")
species_lookup <- read.csv("species_lookup_27082026.csv")


## metadata expansion
## Combine country and country_imputed and rename

metadata <- metadata %>%
  mutate(
    country = na_if(trimws(country), ""),
    country_imputed = na_if(trimws(country_imputed), ""),
    country_combined = coalesce(country_imputed, country),
    .after = country_imputed
  )

sum(!is.na(metadata$country_combined))

metadata <- metadata %>%
  mutate(
    country_combined = case_when(
      country_combined == "DRC" ~ "Democratic Republic of the Congo",
      country_combined == "United States" ~ "USA",
      TRUE ~ country_combined
    )
  )

metadata <- metadata %>%
  left_join(
    country_lookup,
    by = "country_combined"
  )


## Group the temporal scale

metadata <- metadata %>%
  mutate(
    temporal_scale_category = case_when(
      is.na(temporal_scale_month) ~ NA_character_,
      temporal_scale_month < 12 ~ "<1 year",
      temporal_scale_month < 60 ~ "1–5 years",
      temporal_scale_month < 120 ~ "5–10 years",
      temporal_scale_month < 240 ~ "10–20 years",
      TRUE ~ ">20 years"
    )
  )


## Group the study scope variables

metadata <- metadata %>%
  mutate(
    population_ecology = as.integer(
      abundance == 1 |
        occupancy == 1 |
        density == 1 |
        species_distribution == 1 |
        detectability == 1 |
        demographic_rate == 1 |
        population_growth_rate == 1
    ),
    
    social_behavioural_ecology = as.integer(
      social_dynamics == 1 |
        group_size == 1 |
        group_composition == 1 |
        age_structure == 1 |
        sex_ratio == 1 |
        behavioral_response_to_factor == 1 |
        diel_activity == 1 |
        behavioral_state_classification == 1
    ),
    
    spatial_ecology_habitat = as.integer(
      space_use_extent == 1 |
        habitat_landscape_use == 1 |
        dispersal == 1 |
        connectivity == 1 |
        within_species_spatial_overlap == 1
    ),
    
    species_interactions = as.integer(
      predator_prey_interaction == 1 |
        interspecific_competition == 1 |
        scavenging == 1 |
        herbivory == 1 |
        seed_dispersal == 1 |
        interspecific_temporal_spatial_avoidance == 1
    ),
    
    community_ecology_biodiversity = as.integer(
      species_richness == 1 |
        diversity_or_evenness == 1 |
        community_composition == 1 |
        trophic_or_functional_structure == 1
    ),
    
    human_wildlife_disturbance = as.integer(
      human_wildlife_overlap == 1 |
        anthropogenic_disturbance_effect == 1 |
        direct_human_pressure == 1
    )
  )

scope_vars <- c(
  "population_ecology",
  "spatial_ecology_habitat",
  "social_behavioural_ecology",
  "species_interactions",
  "community_ecology_biodiversity",
  "human_wildlife_disturbance"
)

metadata <- metadata %>%
  rowwise() %>%
  mutate(
    study_scope = {
      selected <- scope_vars[
        c_across(all_of(scope_vars)) == 1
      ]
      
      if (length(selected) == 0) {
        NA_character_
      } else {
        paste(selected, collapse = ";")
      }
    }
  ) %>%
  ungroup()

scope_vars_detailed <- names(metadata)[25:57]

metadata <- metadata %>%
  rowwise() %>%
  mutate(
    study_scope_detailed = {
      selected <- scope_vars_detailed[
        c_across(all_of(scope_vars_detailed)) == 1
      ]
      
      if (length(selected) == 0) {
        NA_character_
      } else {
        paste(selected, collapse = ";")
      }
    }
  ) %>%
  ungroup()

glimpse(metadata)


#
##
### 1. Study growth ----

## Colour palette

okabe_ito <- c(
  "#0072B2",  # blue
  "#E69F00",  # orange
  "#009E73",  # green
  "#CC79A7",  # purple
  "#D55E00",  # vermillion
  "#56B4E9",  # sky blue
  "#F0E442",  # yellow
  "#000000",  # black
  "#999999",  # grey
  "#CCBB44"   # olive
)


## Remove 2026 for plotting

metadata <- metadata %>%
  mutate(
    published_year_plot = if_else(
      published_year < 2026,
      published_year,
      NA_real_
    )
  )


## Function: growth by categorical variable

plot_growth <- function(data,
                        variable = NULL,
                        title = NULL,
                        split_semicolon = FALSE,
                        cumulative = FALSE,
                        palette = okabe_ito,
                        show_title = TRUE,
                        show_x_label = TRUE,
                        show_y_label = TRUE) {
  
  # Overall growth
  if (is.null(variable)) {
    
    year_counts <- data %>%
      filter(!is.na(published_year_plot)) %>%
      count(published_year_plot)
    
    if (cumulative) {
      year_counts <- year_counts %>%
        arrange(published_year_plot) %>%
        mutate(n = cumsum(n))
    }
    
    p <- ggplot(
      year_counts,
      aes(x = published_year_plot, y = n)
    ) +
      geom_line(linewidth = 0.6)
    
  } else {
    
    # Accept both unquoted and quoted variable names
    variable_name <- rlang::as_name(rlang::ensym(variable))
    variable_sym <- rlang::sym(variable_name)
    
    # Prepare data
    plot_data <- data %>%
      filter(
        !is.na(published_year_plot),
        !is.na(!!variable_sym)
      )
    
    # Split semicolon-separated categories
    if (split_semicolon) {
      plot_data <- plot_data %>%
        separate_rows(
          !!variable_sym,
          sep = ";"
        ) %>%
        mutate(
          !!variable_sym := trimws(!!variable_sym)
        )
    }
    
    # Order categories from most to least common
    category_order <- plot_data %>%
      count(!!variable_sym, sort = TRUE) %>%
      pull(!!variable_sym)
    
    plot_data <- plot_data %>%
      mutate(
        !!variable_sym := factor(
          !!variable_sym,
          levels = category_order
        )
      )
    
    # Count studies per year
    year_counts <- plot_data %>%
      count(
        published_year_plot,
        !!variable_sym
      )
    
    # Calculate cumulative number
    if (cumulative) {
      year_counts <- year_counts %>%
        arrange(!!variable_sym, published_year_plot) %>%
        group_by(!!variable_sym) %>%
        mutate(n = cumsum(n)) %>%
        ungroup()
    }
    
    # Plot
    p <- ggplot(
      year_counts,
      aes(
        x = published_year_plot,
        y = n,
        color = !!variable_sym,
        group = !!variable_sym
      )
    ) +
      geom_line(linewidth = 0.6) +
      scale_color_manual(values = palette)
  }
  
  # Common formatting
  p +
    scale_x_continuous(
      breaks = seq(
        min(data$published_year_plot, na.rm = TRUE),
        max(data$published_year_plot, na.rm = TRUE),
        by = 5
      )
    ) +
    labs(
      x = if (show_x_label) "Publication year" else NULL,
      y = if (show_y_label) {
        if (cumulative) "Cumulative number of studies" else "Number of studies"
      } else NULL,
      color = NULL,
      title = if (show_title) title else NULL
    ) +
    theme_classic() +
    theme(
      plot.title = element_text(hjust = 0.5),
      axis.title.x = element_text(
        margin = margin(t = 10)
      ),
      axis.title.y = element_text(
        margin = margin(r = 10)
      ),
      legend.position = c(0.03, 0.97),
      legend.justification = c(0, 1),
      legend.background = element_blank(),
      legend.key = element_blank()
    )
}


## Function: Annual growth

calculate_overall_growth <- function(data, group_var,
                                        year_var = "published_year",
                                        start_year = 2005,
                                        exclude_year = 2026) {
  
  annual <- data |>
    dplyr::filter(
      !is.na(.data[[group_var]]),
      !is.na(.data[[year_var]]),
      .data[[year_var]] >= start_year,
      .data[[year_var]] != exclude_year
    ) |>
    dplyr::count(
      .data[[group_var]],
      .data[[year_var]],
      name = "n"
    )
  
  groups <- unique(annual[[group_var]])
  
  results <- lapply(groups, function(g) {
    
    x <- annual[annual[[group_var]] == g, ]
    
    if (nrow(x) < 2 || dplyr::n_distinct(x[[year_var]]) < 2) {
      return(tibble::tibble(
        !!group_var := g,
        annual_growth = NA_real_,
        SE = NA_real_,
        R2 = NA_real_,
        n_studies = sum(x$n),
        n_years = nrow(x)
      ))
    }
    
    model <- lm(
      log(n) ~ x[[year_var]],
      data = x
    )
    
    beta <- coef(model)[2]
    se_beta <- summary(model)$coefficients[2, 2]
    
    tibble::tibble(
      !!group_var := g,
      annual_growth = (exp(beta) - 1) * 100,
      SE = exp(beta) * se_beta * 100,
      R2 = summary(model)$r.squared,
      n_studies = sum(x$n),
      n_years = nrow(x)
    )
  })
  
  dplyr::bind_rows(results) |>
    dplyr::arrange(dplyr::desc(annual_growth))
}


## Function: Growth at different period

calculate_period_growth <- function(data, group_var,
                                    year_var = "published_year_plot",
                                    periods = c("1995–1999", "2000–2004", "2005–2009",
                                                "2010–2014", "2015–2019", "2020–2025")) {
  
  period_breaks <- c(1995, 2000, 2005, 2010, 2015, 2020, 2026)
  
  annual_data <- data %>%
    dplyr::filter(
      !is.na(.data[[group_var]]),
      !is.na(.data[[year_var]])
    ) %>%
    dplyr::mutate(
      period = cut(
        .data[[year_var]],
        breaks = period_breaks,
        labels = periods,
        right = FALSE
      )
    ) %>%
    dplyr::count(
      .data[[group_var]],
      period,
      .data[[year_var]],
      name = "n_studies"
    )
  
  results <- annual_data %>%
    dplyr::group_by(.data[[group_var]], period) %>%
    dplyr::group_modify(~ {
      
      # Need at least 2 distinct years to estimate a growth rate
      if (dplyr::n_distinct(.x[[year_var]]) < 2) {
        return(tibble::tibble(
          annual_growth = NA_real_,
          SE = NA_real_
        ))
      }
      
      model <- lm(
        reformulate(year_var, response = "log(n_studies)"),
        data = .x
      )
      
      beta <- coef(model)[2]
      annual_growth <- (exp(beta) - 1) * 100
      
      # SE cannot be estimated with <= 2 observations
      if (df.residual(model) > 0) {
        se_beta <- summary(model)$coefficients[2, 2]
        SE <- exp(beta) * se_beta * 100
      } else {
        SE <- NA_real_
      }
      
      tibble::tibble(
        annual_growth = annual_growth,
        SE = SE
      )
    }) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(
      growth_SE = dplyr::case_when(
        is.na(annual_growth) ~ NA_character_,
        is.na(SE) ~ sprintf("%.1f%%", annual_growth),
        TRUE ~ sprintf("%.1f ± %.1f%%", annual_growth, SE)
      )
    ) %>%
    dplyr::select(
      dplyr::all_of(group_var),
      period,
      growth_SE
    ) %>%
    tidyr::pivot_wider(
      names_from = period,
      values_from = growth_SE
    )
  
  # Force periods into chronological order
  period_order <- c(
    "1995–1999",
    "2000–2004",
    "2005–2009",
    "2010–2014",
    "2015–2019",
    "2020–2025"
  )
  
  results %>%
    dplyr::select(
      dplyr::all_of(group_var),
      dplyr::all_of(period_order)
    )
}


## P1: Overall growth

annual_counts <- metadata |>
  dplyr::filter(published_year != 2026, !is.na(published_year)) |>
  dplyr::count(published_year, name = "n")

# Since 2005

growth_model_2005 <- lm(log(n) ~ published_year,
                   data = annual_counts |>
                     dplyr::filter(published_year >= 2005))

beta_2005 <- coef(growth_model_2005)[["published_year"]]
growth_rate_2005 <- (exp(beta_2005) - 1) * 100

se_beta_2005 <- summary(growth_model_2005)$coefficients["published_year", "Std. Error"]
se_growth_2005 <- exp(beta_2005) * se_beta_2005 * 100

summary(growth_model_2005)
growth_rate_2005
se_growth_2005

# Since 2015

growth_model_2015 <- lm(log(n) ~ published_year,
                        data = annual_counts |>
                          dplyr::filter(published_year >= 2015))

beta_2015 <- coef(growth_model_2015)[["published_year"]]
growth_rate_2015 <- (exp(beta_2015) - 1) * 100

se_beta_2015 <- summary(growth_model_2015)$coefficients["published_year", "Std. Error"]
se_growth_2015 <- exp(beta_2015) * se_beta_2015 * 100

summary(growth_model_2015)
growth_rate_2015
se_growth_2015

# Periods

periods <- data.frame(
  start = c(1995, 2000, 2005, 2010, 2015, 2020),
  end = c(1999, 2004, 2009, 2014, 2019, 2025)
)

growth_results <- purrr::map_dfr(seq_len(nrow(periods)), function(i) {
  dat <- annual_counts |>
    dplyr::filter(
      published_year >= periods$start[i],
      published_year <= periods$end[i]
    )
  
  model <- lm(log(n) ~ published_year, data = dat)
  beta <- coef(model)[["published_year"]]
  se_beta <- summary(model)$coefficients["published_year", "Std. Error"]
  
  data.frame(
    period = paste0(periods$start[i], "–", periods$end[i]),
    annual_growth = (exp(beta) - 1) * 100,
    SE = exp(beta) * se_beta * 100,
    R2 = summary(model)$r.squared
  )
})

growth_results

p1 <- plot_growth(
  metadata,
  title = "All articles",
  show_title = TRUE,
  show_x_label = FALSE,
  show_y_label = TRUE
)
p1


## P2: Growth by taxonomic scope

calculate_overall_growth(
  metadata,
  group_var = "taxonomy_scope",
  start_year = 2015
)

calculate_period_growth(
  metadata,
  group_var = "taxonomy_scope"
)

p2 <- plot_growth(
  metadata,
  "taxonomy_scope",
  title = "Taxonomic scope",
  show_title = TRUE,
  show_x_label = TRUE,
  show_y_label = FALSE
) +
  ggplot2::scale_color_manual(
    values = okabe_ito,
    labels = c(
      "single_species" = "Single-species",
      "multi_species" = "Multi-species",
      "community" = "Community-level"
    )
  )
p2


## P3: Growth by taxonomic group

calculate_overall_growth(
  metadata,
  group_var = "primary_taxonomic_group",
  start_year = 2015
)

calculate_period_growth(
  metadata,
  group_var = "primary_taxonomic_group"
)

p3 <- plot_growth(
  metadata,
  "primary_taxonomic_group",
  title = "Primary taxonomic group",
  show_title = TRUE,
  show_x_label = TRUE,
  show_y_label = TRUE
) +
  ggplot2::scale_color_manual(
    values = okabe_ito,
    labels = c(
      "mammal" = "Mammal",
      "multi_taxa" = "Multi-taxa",
      "bird" = "Bird",
      "reptile" = "Reptile",
      "amphibian" = "Amphibian"
    )
  )
p3


## P4: Growth by biome

biome_long <- metadata %>%
  separate_rows(biome_combined, sep = ";")

calculate_overall_growth(
  biome_long,
  group_var = "biome_combined",
  start_year = 2015
)

calculate_period_growth(
  biome_long,
  group_var = "biome_combined"
)

p4 <- plot_growth(
  biome_long,
  "biome_combined",
  title = "Biomes",
  show_title = TRUE,
  show_x_label = TRUE,
  show_y_label = FALSE
) +
  ggplot2::scale_color_manual(
    values = okabe_ito,
    labels = c(
      "rainforest" = "Rainforest",
      "temperate_deciduous_forest" = "Temporate deciduous forest",
      "grassland" = "Grassland",
      "shrubland" = "Shrubland",
      "desert" = "Desert",
      "coniferous_forest" = "coniferous forest",
      "tundra" = "Tundra"
    )
  )
p4


## P5: Growth by study approach

calculate_overall_growth(
  metadata,
  group_var = "study_approach",
  start_year = 2005
)

calculate_period_growth(
  metadata,
  group_var = "study_approach"
)

p5 <- plot_growth(
  metadata,
  "study_approach",
  title = "Study approach",
  show_title = TRUE,
  show_x_label = FALSE,
  show_y_label = FALSE
) +
  ggplot2::scale_color_manual(
    values = okabe_ito,
    labels = c(
      "observational" = "Observational",
      "experimental" = "Experimental",
      "mixed" = "Mixed desgin"
    )
  )
p5


## P6: Growth by temporal design

calculate_overall_growth(
  metadata,
  group_var = "temporal_design",
  start_year = 2005
)

calculate_period_growth(
  metadata,
  group_var = "temporal_design"
)

p6 <- plot_growth(
  metadata,
  "temporal_design",
  title = "Temporal design",
  show_title = TRUE,
  show_x_label = FALSE,
  show_y_label = FALSE
) +
  ggplot2::scale_color_manual(
    values = okabe_ito,
    labels = c(
      "cross_sectional" = "Cross-sectional",
      "longitudinal" = "Longitudinal",
      "before_after" = "Before-and-after",
      "mixed" = "Mixed desgin"
    )
  )
p6


# P7: Growth by temporal scale

calculate_overall_growth(
  metadata,
  group_var = "temporal_scale_category",
  start_year = 2005
)

calculate_period_growth(
  metadata,
  group_var = "temporal_scale_category"
)

p7 <- plot_growth(
  metadata,
  "temporal_scale_category",
  title = "Sampling duration",
  show_title = TRUE,
  show_x_label = TRUE,
  show_y_label = TRUE
)
p7


# P8: Growth by spatial scale

calculate_overall_growth(
  metadata,
  group_var = "spatial_scale",
  start_year = 2015
)

calculate_period_growth(
  metadata,
  group_var = "spatial_scale"
)

p8 <- plot_growth(
  metadata,
  "spatial_scale",
  title = "Spatial scale",
  show_title = TRUE,
  show_x_label = TRUE,
  show_y_label = TRUE
)
p8


# P9: Growth by region

calculate_overall_growth(
  metadata,
  group_var = "region",
  start_year = 2015
)

calculate_period_growth(
  metadata,
  group_var = "region"
)

p9 <- plot_growth(
  metadata,
  "region",
  title = "Region",
  show_title = TRUE,
  show_x_label = TRUE,
  show_y_label = FALSE
)
p9


# P10: Growth by study scope

scope_long <- metadata %>%
  separate_rows(study_scope, sep = ";")

Scope_growth_2005 <- calculate_overall_growth(
  scope_long,
  group_var = "study_scope",
  start_year = 2005
)

Scope_growth_2015 <- calculate_overall_growth(
  scope_long,
  group_var = "study_scope",
  start_year = 2015
)

Scope_growth_compare <- Scope_growth_2005 %>%
  rename(
    growth_2005 = annual_growth,
    SE_2005 = SE
  ) %>%
  left_join(
    Scope_growth_2015 %>%
      select(study_scope, annual_growth, SE) %>%
      rename(
        growth_2015 = annual_growth,
        SE_2015 = SE
      ),
    by = "study_scope"
  )

Scope_growth_compare <- Scope_growth_compare %>%
  mutate(
    growth_change = growth_2015 - growth_2005
  )

calculate_period_growth(
  scope_long,
  group_var = "study_scope"
)

p10 <- plot_growth(
  scope_long,
  "study_scope",
  title = "Study scope",
  show_title = TRUE,
  show_x_label = FALSE,
  show_y_label = FALSE
) +
  ggplot2::scale_color_manual(
    values = okabe_ito,
    labels = c(
      "population_ecology" = "Population ecology",
      "spatial_ecology_habitat" = "Spatial ecology and habitat use",
      "social_behavioural_ecology" = "Social and behavioural ecology",
      "species_interactions" = "Species interactions",
      "community_ecology_biodiversity" = "Community richness and diversity",
      "human_wildlife_disturbance" = "Human–wildlife interactions"
    )
  )
p10

## Make panel figure

panel <- (p1 | p10 | p5 | p6) /
  (p3 | p2 | p9 | p4)

panel <- panel &
  theme(
    text = element_text(size = 8),
    plot.title = element_text(size = 9, hjust = 0.5),
    axis.title = element_text(size = 8),
    axis.text = element_text(size = 5),
    legend.text = element_text(size = 5),
    legend.key.width = unit(0.5, "cm"),
    legend.key.height = unit(0.3, "cm"),
    legend.spacing.y = unit(0.05, "cm")
  )

panel

ggsave("output/growth.jpeg", width = 8, height = 4, dpi = 300)

rm(p1, p2, p3, p4, p5, p6, p7, p8, p9, p10)


#
##
### 2. Geographic location ----

## Create geographic summary table: by region

# # Country look up
# 
# country_lookup <- metadata %>%
#   separate_rows(country_combined, sep = ";") %>%
#   mutate(
#     country_combined = trimws(country_combined),
#     iso3c = countrycode(
#       country_combined,
#       origin = "country.name",
#       destination = "iso3c"
#     ),
#     continent = countrycode(
#       iso3c,
#       origin = "iso3c",
#       destination = "continent"
#     )
#   ) %>%
#   distinct(
#     country_combined,
#     iso3c,
#     continent
#   )
# 
# country_lookup %>%
#   filter(is.na(iso3c)) # all good
# 
# country_lookup <- country_lookup %>%
#   mutate(
#     region = case_when(
# 
#       # North America
#       iso3c %in% c("CAN", "USA") ~ "North America",
# 
#       # Central, Caribbean & South America
#       iso3c %in% c(
#         "MEX", "BLZ", "CRI", "SLV", "GTM", "HND", "NIC", "PAN",
#         "ATG", "BHS", "BRB", "CUB", "DMA", "DOM", "GRD",
#         "HTI", "JAM", "KNA", "LCA", "VCT", "TTO",
#         "GLP", "MTQ", "PRI",
#         "ARG", "BOL", "BRA", "CHL", "COL", "ECU", "GUY",
#         "PRY", "PER", "SUR", "URY", "VEN", "GUF"
#       ) ~ "Central, Caribbean, South America",
# 
#       TRUE ~ continent
#     )
#   )
# 
# country_lookup %>%
#   count(region, sort = TRUE) # all good
# 
# # Add GDP and country area information
# 
# wb_gdp <- WDI(
#   country = "all",
#   indicator = "NY.GDP.PCAP.CD",
#   start = 2024,
#   end = 2024
# ) %>%
#   select(
#     iso3c,
#     gdp_pc = NY.GDP.PCAP.CD
#   )
# 
# wb_area <- WDI(
#   country = "all",
#   indicator = "AG.SRF.TOTL.K2",
#   start = 2023,
#   end = 2023
# ) %>%
#   select(
#     iso3c,
#     country_area_km2 = AG.SRF.TOTL.K2
#   )
# 
# country_lookup <- country_lookup %>%
#   left_join(wb_gdp, by = "iso3c") %>%
#   left_join(wb_area, by = "iso3c")
# 
# country_lookup
# 
# # Manual input NA information
# 
# area_na <- country_lookup %>%
#   filter(is.na(country_lookup$country_area_km2))
# 
# country_lookup <- country_lookup %>%
#   filter(!is.na(country_lookup$country_combined)) %>%
#   mutate(
#     country_area_km2 = case_when(
#       iso3c == "TWN" ~ 36197,
#       iso3c == "GLP" ~ 1628,
#       iso3c == "MTQ" ~ 1128,
#       iso3c == "REU" ~ 2512,
#       iso3c == "GUF" ~ 83534,
#       iso3c == "ATA" ~ 14200000,
#       TRUE ~ country_area_km2
#     )
#   )
# 
# write.csv(country_lookup, "country_lookup_04092026.csv", row.names = FALSE)

# Create long format

metadata_region <- metadata %>%
  separate_rows(
    country_combined,
    sep = ";"
  ) %>%
  mutate(
    country_combined = trimws(country_combined)
  ) %>%
  filter(!is.na(country_combined)) %>%
  distinct()

region_count <- metadata %>%
  separate_rows(region, sep = ";") %>%
  mutate(
    region = trimws(region)
  ) %>%
  filter(!is.na(region)) %>%
  group_by(region) %>%
  summarise(
    frequency = n(),
    
    # Spatial scale
    spatial_local = mean(spatial_scale == "local", na.rm = TRUE),
    spatial_regional = mean(spatial_scale == "regional", na.rm = TRUE),
    spatial_national = mean(spatial_scale == "national", na.rm = TRUE),
    spatial_continental = mean(spatial_scale == "continental", na.rm = TRUE),
    
    # Study duration
    temporal_1 = mean(temporal_scale_category == "<1 year", na.rm = TRUE),
    temporal_1_5 = mean(temporal_scale_category == "1–5 years", na.rm = TRUE),
    temporal_5_10 = mean(temporal_scale_category == "5–10 years", na.rm = TRUE),
    temporal_10_20 = mean(temporal_scale_category == "10–20 years", na.rm = TRUE),
    temporal_20 = mean(temporal_scale_category == ">20 years", na.rm = TRUE),
    
    # Temporal design
    temporal_cross_sectional = mean(temporal_design == "cross_sectional", na.rm = TRUE),
    temporal_longitudinal = mean(temporal_design == "longitudinal", na.rm = TRUE),
    temporal_before_after = mean(temporal_design == "before_after", na.rm = TRUE),
    temporal_mixed = mean(temporal_design == "mixed", na.rm = TRUE),
    
    # Study approach
    study_observational = mean(study_approach == "observational", na.rm = TRUE),
    study_experimental = mean(study_approach == "experimental", na.rm = TRUE),
    
    .groups = "drop"
  ) %>%
  mutate(
    # Overall percentage of all studies
    percentage = round(100 * frequency / sum(frequency), 1),
    
    # Convert all proportions to percentages
    across(
      c(
        spatial_local,
        spatial_regional,
        spatial_national,
        spatial_continental,
        temporal_1,
        temporal_1_5,
        temporal_5_10,
        temporal_10_20,
        temporal_20,
        temporal_cross_sectional,
        temporal_longitudinal,
        temporal_before_after,
        temporal_mixed,
        study_observational,
        study_experimental
      ),
      ~ round(100 * .x, 1)
    )
  ) %>%
  arrange(desc(frequency))

region_total <- metadata_region %>%
  filter(!is.na(region)) %>%
  summarise(
    region = "Total",
    frequency = n(),
    
    spatial_local = mean(spatial_scale == "local", na.rm = TRUE),
    spatial_regional = mean(spatial_scale == "regional", na.rm = TRUE),
    spatial_national = mean(spatial_scale == "national", na.rm = TRUE),
    spatial_continental = mean(spatial_scale == "continental", na.rm = TRUE),
    
    temporal_1 = mean(temporal_scale_category == "<1 year", na.rm = TRUE),
    temporal_1_5 = mean(temporal_scale_category == "1–5 years", na.rm = TRUE),
    temporal_5_10 = mean(temporal_scale_category == "5–10 years", na.rm = TRUE),
    temporal_10_20 = mean(temporal_scale_category == "10–20 years", na.rm = TRUE),
    temporal_20 = mean(temporal_scale_category == ">20 years", na.rm = TRUE),
    
    temporal_cross_sectional = mean(temporal_design == "cross_sectional", na.rm = TRUE),
    temporal_longitudinal = mean(temporal_design == "longitudinal", na.rm = TRUE),
    temporal_before_after = mean(temporal_design == "before_after", na.rm = TRUE),
    temporal_mixed = mean(temporal_design == "mixed", na.rm = TRUE),
    
    study_observational = mean(study_approach == "observational", na.rm = TRUE),
    study_experimental = mean(study_approach == "experimental", na.rm = TRUE)
  ) %>%
  mutate(
    percentage = 100,
    across(
      c(
        spatial_local, spatial_regional, spatial_national, spatial_continental,
        temporal_1, temporal_1_5, temporal_5_10, temporal_10_20, temporal_20,
        temporal_cross_sectional, temporal_longitudinal,
        temporal_before_after, temporal_mixed,
        study_observational, study_experimental
      ),
      ~ round(100 * .x, 1)
    ),
    broad_scale = spatial_regional + spatial_national + spatial_continental,
    long_duration = temporal_5_10 + temporal_10_20 + temporal_20
  )

region_count <- region_count %>%
  mutate(
    broad_scale = spatial_regional +
      spatial_national +
      spatial_continental
  )

region_count <- region_count %>%
  mutate(
    long_duration = temporal_5_10 +
      temporal_10_20 +
      temporal_20
  )

region_count <- bind_rows(region_count, region_total)

write.xlsx(region_count, "region_summary_06092026.xlsx")


## Create geographic summary table: by country

country_count <- metadata %>%
  separate_rows(country_combined, sep = ";") %>%
  mutate(
    country_combined = trimws(country_combined)
  ) %>%
  filter(!is.na(country_combined)) %>%
  group_by(country_combined) %>%
  summarise(
    frequency = n(),
    
    # Spatial scale
    spatial_local = mean(spatial_scale == "local", na.rm = TRUE),
    spatial_regional = mean(spatial_scale == "regional", na.rm = TRUE),
    spatial_national = mean(spatial_scale == "national", na.rm = TRUE),
    spatial_continental = mean(spatial_scale == "continental", na.rm = TRUE),
    
    # Study duration
    temporal_1 = mean(temporal_scale_category == "<1 year", na.rm = TRUE),
    temporal_1_5 = mean(temporal_scale_category == "1–5 years", na.rm = TRUE),
    temporal_5_10 = mean(temporal_scale_category == "5–10 years", na.rm = TRUE),
    temporal_10_20 = mean(temporal_scale_category == "10–20 years", na.rm = TRUE),
    temporal_20 = mean(temporal_scale_category == ">20 years", na.rm = TRUE),
    
    # Temporal design
    temporal_cross_sectional = mean(temporal_design == "cross_sectional", na.rm = TRUE),
    temporal_longitudinal = mean(temporal_design == "longitudinal", na.rm = TRUE),
    temporal_before_after = mean(temporal_design == "before_after", na.rm = TRUE),
    temporal_mixed = mean(temporal_design == "mixed", na.rm = TRUE),
    
    # Study approach
    study_observational = mean(study_approach == "observational", na.rm = TRUE),
    study_experimental = mean(study_approach == "experimental", na.rm = TRUE),
    
    .groups = "drop"
  ) %>%
  mutate(
    # Overall percentage of all studies
    percentage = round(100 * frequency / sum(frequency), 1),
    
    # Convert all proportions to percentages
    across(
      c(
        spatial_local,
        spatial_regional,
        spatial_national,
        spatial_continental,
        temporal_1,
        temporal_1_5,
        temporal_5_10,
        temporal_10_20,
        temporal_20,
        temporal_cross_sectional,
        temporal_longitudinal,
        temporal_before_after,
        temporal_mixed,
        study_observational,
        study_experimental
      ),
      ~ round(100 * .x, 1)
    )
  ) %>%
  arrange(desc(frequency))

country_count <- country_count %>%
  mutate(
    broad_scale = spatial_regional +
      spatial_national +
      spatial_continental
  )

country_count <- country_count %>%
  mutate(
    long_duration = temporal_5_10 +
      temporal_10_20 +
      temporal_20
  )

country_count <- country_count %>%
  left_join(
    country_lookup,
    by = "country_combined"
  ) %>%
  mutate(
    studies_per_100k_km2 = frequency / country_area_km2 * 100000
  )

country_count %>%
  summarise(
    min = min(studies_per_100k_km2, na.rm = TRUE),
    median = median(studies_per_100k_km2, na.rm = TRUE),
    mean = mean(studies_per_100k_km2, na.rm = TRUE),
    max = max(studies_per_100k_km2, na.rm = TRUE)
  )
country_count %>%
  select(country_combined, frequency, country_area_km2, studies_per_100k_km2) %>%
  arrange(desc(studies_per_100k_km2)) %>%
  head(20)

write.xlsx(country_count, "country_summary_06092026.xlsx")


## Q1: What countries has the most camera trap studies?

world_map <- map_data("world")

world_map_count <- world_map %>%
  left_join(
    country_count,
    by = c("region" = "country_combined")
  )

# Basic

ggplot() +
  geom_polygon(
    data = world_map_count,
    aes(x = long, y = lat, group = group, fill = frequency),
    color = "white", linewidth = 0.2) +
  scale_fill_gradient(
    low = "#E5F5E0", high = "#006D2C", na.value = "grey90") +
  labs(
    fill = "Number of studies", x = NULL, y = NULL) +
  coord_fixed(1.2) +
  theme_minimal() +
  theme(
    legend.position = "right",
    legend.title = element_text(size = 11),
    legend.text = element_text(size = 9),
    axis.text = element_blank(),
    axis.title = element_blank(),
    panel.grid = element_blank()
  )

ggsave("output/country_map.jpeg", width = 9, height = 4, dpi = 300)

# Normalize by country size

ggplot() +
  geom_polygon(
    data = world_map_count,
    aes(x = long, y = lat, group = group, fill = studies_per_100k_km2),
    color = "white", linewidth = 0.2
  ) +
  scale_fill_gradient(
    low = "#FFF1CC",
    high = "#A65F00",
    trans = "log10",
    breaks = c(0.01, 0.1, 1, 10, 100, 1000),
    labels = c("0.01", "0.1", "1", "10", "100", "1,000"),
    na.value = "grey90"
  ) +
  labs(
    fill = "Studies per 100,000 km²",
    x = NULL,
    y = NULL
  ) +
  coord_fixed(1.2) +
  theme_minimal() +
  theme(
    legend.position = "right",
    legend.title = element_text(size = 11),
    legend.text = element_text(size = 9),
    axis.text = element_blank(),
    axis.title = element_blank(),
    panel.grid = element_blank()
  )

ggsave("output/country_map_normalized_area.jpeg", width = 9, height = 4, dpi = 300)

# Accumulation of countries

country_accumulation <- metadata_region %>%
  filter(!is.na(country_combined), !is.na(published_year)) %>%
  group_by(country_combined) %>%
  summarise(first_year = min(published_year), .groups = "drop") %>%
  count(first_year, name = "new_countries") %>%
  arrange(first_year) %>%
  mutate(
    cumulative_countries = cumsum(new_countries)
  )

country_accumulation

country_growth_model <- country_accumulation %>%
  filter(first_year >= 2005, first_year <= 2025) %>%
  lm(log(cumulative_countries) ~ first_year, data = .)

slope <- coef(country_growth_model)[["first_year"]]
se <- summary(country_growth_model)$coefficients["first_year", "Std. Error"]

growth <- (exp(slope) - 1) * 100
growth_se <- exp(slope) * se * 100

growth
growth_se

ggplot(
  country_accumulation,
  aes(x = first_year, y = cumulative_countries)
) +
  geom_line(linewidth = 1, color = "#0072B2") +
  geom_point(size = 2, color = "#0072B2") +
  scale_x_continuous(
    breaks = seq(1995, 2025, 5)
  ) +
  scale_y_continuous(
    labels = scales::label_comma()
  ) +
  labs(
    x = "Year",
    y = "Cumulative number of countries"
  ) +
  theme_classic() +
  theme(
    text = element_text(size = 9)
  )

ggsave("output/country_accumulation.jpeg", width = 5, height = 3, dpi = 300)


## Q2: What continental trends on spatial and temporal design can we observe?

glimpse(region_count)

# Temporal trend

temporal_plot <- region_count %>%
  select(
    region,
    temporal_1,
    temporal_1_5,
    temporal_5_10,
    temporal_10_20,
    temporal_20
  ) %>%
  pivot_longer(
    cols = starts_with("temporal_"),
    names_to = "temporal_scale",
    values_to = "percentage"
  ) %>%
  mutate(
    temporal_scale = factor(
      temporal_scale,
      levels = c(
        "temporal_1",
        "temporal_1_5",
        "temporal_5_10",
        "temporal_10_20",
        "temporal_20"
      ),
      labels = c(
        "<1 year",
        "1–5 years",
        "5–10 years",
        "10–20 years",
        ">20 years"
      )
    )
  )

p_temporal <- ggplot(
  temporal_plot,
  aes(
    x = temporal_scale,
    y = percentage,
    color = region,
    group = region
  )
) +
  geom_line(linewidth = 0.7) +
  geom_point(size = 1.5) +
  scale_color_manual(values = okabe_ito) +
  labs(
    x = "Study duration",
    y = "Percentage of studies (%)",
    color = NULL
  ) +
  theme_classic() +
  theme(
    legend.position = c(0.6, 0.97),
    legend.justification = c(0, 1),
    legend.background = element_blank(),
    legend.key = element_blank(),
    axis.title.x = element_text(margin = margin(t = 10)),
    axis.title.y = element_text(margin = margin(r = 10))
  )
p_temporal

# Spatial trend

spatial_plot <- region_count %>%
  select(
    region,
    spatial_local,
    spatial_regional,
    spatial_national,
    spatial_continental
  ) %>%
  pivot_longer(
    cols = starts_with("spatial_"),
    names_to = "spatial_scale",
    values_to = "percentage"
  ) %>%
  mutate(
    spatial_scale = factor(
      spatial_scale,
      levels = c(
        "spatial_local",
        "spatial_regional",
        "spatial_national",
        "spatial_continental"
      ),
      labels = c(
        "Local",
        "Regional",
        "National",
        "Continental"
      )
    )
  )

p_spatial <- ggplot(
  spatial_plot,
  aes(
    x = spatial_scale,
    y = percentage,
    color = region,
    group = region
  )
) +
  geom_line(linewidth = 0.7) +
  geom_point(size = 1.5) +
  scale_color_manual(values = okabe_ito) +
  labs(
    x = "Spatial scale",
    y = "Percentage of studies (%)",
    color = NULL
  ) +
  theme_classic() +
  theme(
    legend.position = c(0.6, 0.97),
    legend.justification = c(0, 1),
    legend.background = element_blank(),
    legend.key = element_blank(),
    axis.title.x = element_text(margin = margin(t = 10)),
    axis.title.y = element_text(margin = margin(r = 10))
  )
p_spatial

# Panel figure

panel <- (p_spatial / p_temporal)

panel <- panel &
  theme(
    text = element_text(size = 8),
    axis.title = element_text(size = 9),
    legend.key.width = unit(0.5, "cm"),
    legend.key.height = unit(0.5, "cm"),
    legend.spacing.y = unit(0.1, "cm")
  )
panel

ggsave("output/region_scale.jpeg", width = 6, height = 8, dpi = 300)


## Q3: How is GDP linked with the study design?

plot_data <- country_count %>%
  filter(
    !is.na(gdp_pc),
    !is.na(broad_scale),
    !is.na(long_duration),
    !is.na(study_experimental),
    frequency >= 5
  )

# Spatial

cor_test <- cor.test(
  plot_data$gdp_pc,
  plot_data$broad_scale,
  method = "spearman"
)

rho <- cor_test$estimate
p <- cor_test$p.value

x_annot <- min(plot_data$gdp_pc, na.rm = TRUE) * 1.2
y_annot <- max(plot_data$broad_scale, na.rm = TRUE) - 2

GDP_spatial <- ggplot(
  plot_data,
  aes(x = gdp_pc, y = broad_scale, size = frequency)
) +
  geom_point(color = "darkblue", alpha = 0.7) +
  geom_smooth(method = "lm", se = TRUE, color = "darkred", linewidth = 1) +
  scale_x_log10(
    labels = scales::label_dollar()
  ) +
  labs(
    x = NULL,
    y = "Broad-scale studies (%)",
    size = "Number of studies"
  ) +
  annotate(
    "text", x = x_annot, y = y_annot,
    label = paste0(
      "Spearman's \u03c1 = ", round(rho, 2),
      "\nP = ", format.pval(p, digits = 2)
    ),
    hjust = 0, vjust = 1, size = 3.5
  ) +
  theme_classic()

# Temporal

cor_test <- cor.test(
  plot_data$gdp_pc,
  plot_data$long_duration,
  method = "spearman"
)

rho <- cor_test$estimate
p <- cor_test$p.value

x_annot <- min(plot_data$gdp_pc, na.rm = TRUE) * 1.2
y_annot <- max(plot_data$long_duration, na.rm = TRUE) - 2

GDP_temporal <- ggplot(
  plot_data,
  aes(x = gdp_pc, y = long_duration, size = frequency)
) +
  geom_point(color = "darkblue", alpha = 0.7) +
  geom_smooth(method = "lm", se = TRUE, color = "darkred", linewidth = 1) +
  scale_x_log10(
    labels = scales::label_dollar()
  ) +
  labs(
    x = NULL,
    y = "Long duration studies (%)",
    size = "Number of studies"
  ) +
  annotate(
    "text", x = x_annot, y = y_annot,
    label = paste0(
      "Spearman's \u03c1 = ", round(rho, 2),
      "\nP = ", format.pval(p, digits = 2)
    ),
    hjust = 0, vjust = 1, size = 3.5
  ) +
  theme_classic()

# Experimental

cor_test <- cor.test(
  plot_data$gdp_pc,
  plot_data$study_experimental,
  method = "spearman"
)

rho <- cor_test$estimate
p <- cor_test$p.value

x_annot <- min(plot_data$gdp_pc, na.rm = TRUE) * 1.2
y_annot <- max(plot_data$study_experimental, na.rm = TRUE) - 2

GDP_experimental <- ggplot(
  plot_data,
  aes(x = gdp_pc, y = study_experimental, size = frequency)
) +
  geom_point(color = "darkblue", alpha = 0.7) +
  geom_smooth(method = "lm", se = TRUE, color = "darkred", linewidth = 1) +
  scale_x_log10(
    labels = scales::label_dollar()
  ) +
  labs(
    x = "GDP per capita (USD)",
    y = "Experimental studies (%)",
    size = "Number of studies"
  ) +
  annotate(
    "text", x = x_annot, y = y_annot,
    label = paste0(
      "Spearman's \u03c1 = ", round(rho, 2),
      "\nP = ", format.pval(p, digits = 2)
    ),
    hjust = 0, vjust = 1, size = 3.5
  ) +
  theme_classic()
GDP_experimental

# Trap nights

plot_data <- metadata %>%
  filter(!is.na(gdp_pc), !is.na(trap_night), trap_night > 0)

cor_test <- cor.test(
  plot_data$gdp_pc,
  plot_data$trap_night,
  method = "spearman"
)

rho <- cor_test$estimate
p <- cor_test$p.value

x_annot <- min(plot_data$gdp_pc, na.rm = TRUE) * 1.2
y_annot <- max(plot_data$trap_night, na.rm = TRUE) / 2

GDP_trap_night <- ggplot(
  plot_data,
  aes(x = gdp_pc, y = trap_night)
) +
  geom_point(color = "darkblue", alpha = 0.3) +
  geom_smooth(
    method = "lm",
    se = TRUE,
    color = "darkred",
    linewidth = 1
  ) +
  scale_x_log10(labels = scales::label_dollar()) +
  scale_y_log10(labels = scales::label_comma()) +
  labs(
    x = "GDP per capita (USD)",
    y = "Trap nights (log scale)"
  ) +
  annotate(
    "text",
    x = x_annot,
    y = y_annot,
    label = paste0(
      "Spearman's \u03c1 = ", round(rho, 2),
      "\nP = ", format.pval(p, digits = 2)
    ),
    hjust = 0,
    vjust = 1,
    size = 3.5
  ) +
  theme_classic()
GDP_trap_night

# Panel figure

panel <- (GDP_spatial / GDP_temporal / GDP_experimental)

panel <- panel &
  theme(
    text = element_text(size = 8),
    axis.title = element_text(size = 9),
    axis.title.x = element_text(margin = margin(t = 8)),
    axis.title.y = element_text(margin = margin(r = 8)),
    legend.key.width = unit(0.5, "cm"),
    legend.key.height = unit(0.5, "cm"),
    legend.spacing.y = unit(0.1, "cm")
  )
panel

ggsave("output/GDP.jpeg", width = 6, height = 10, dpi = 300)


#
##
### 3. Spatial vs. temporal scale ----

## Q1: Do studies conducted at broader spatial scales have longer study duration? 

# Study duration

plot_data <- metadata %>%
  filter(!is.na(spatial_scale), !is.na(temporal_scale_month),
         spatial_scale != "global") %>%
  mutate(spatial_scale = factor(spatial_scale,
                                levels = c("local", "regional", "national", "continental")),
         spatial_scale_score = as.numeric(spatial_scale))

kw_test <- kruskal.test(temporal_scale_month ~ spatial_scale, data = plot_data)
cor_test <- cor.test(plot_data$spatial_scale_score, plot_data$temporal_scale_month,
                     method = "spearman")
kw_test
cor_test

test_label <- paste0(
  "Kruskal–Wallis: p = ", format.pval(kw_test$p.value, digits = 3),
  "\nSpearman's ρ = ", round(cor_test$estimate, 2),
  ", p = ", format.pval(cor_test$p.value, digits = 3)
)

p1 <- ggplot(plot_data, aes(x = spatial_scale, y = temporal_scale_month, fill = spatial_scale)) +
  geom_boxplot(outlier.shape = NA, width = 0.6, alpha = 0.8) +
  geom_jitter(width = 0.15, alpha = 0.15, size = 1) +
  scale_fill_manual(values = c(
    "local" = "#D9EAF7",
    "regional" = "#9ECAE1",
    "national" = "#4292C6",
    "continental" = "#08519C"
  )) +
  scale_x_discrete(labels = c(
    "local" = "Local",
    "regional" = "Regional",
    "national" = "National",
    "continental" = "Continental"
  )) +
  scale_y_log10() +
  labs(x = NULL, y = "Study duration (months, log scale)") +
  theme_classic() +
  theme(
    legend.position = "none",
    axis.title = element_text(size = 11),
    axis.title.y = element_text(size = 11, margin = margin(r = 10)),
    axis.text = element_text(size = 9)
  )
p1

# Trap nights

plot_data <- metadata %>%
  filter(!is.na(spatial_scale), !is.na(trap_night),
         spatial_scale != "global") %>%
  mutate(spatial_scale = factor(spatial_scale,
                                levels = c("local", "regional", "national", "continental")),
         spatial_scale_score = as.numeric(spatial_scale))

kw_test <- kruskal.test(trap_night ~ spatial_scale, data = plot_data)
cor_test <- cor.test(plot_data$spatial_scale_score, plot_data$trap_night,
                     method = "spearman")
kw_test
cor_test

test_label <- paste0(
  "Kruskal–Wallis: p = ", format.pval(kw_test$p.value, digits = 3),
  "\nSpearman's ρ = ", round(cor_test$estimate, 2),
  ", p = ", format.pval(cor_test$p.value, digits = 3)
)

p2 <- ggplot(plot_data, aes(x = spatial_scale, y = trap_night, fill = spatial_scale)) +
  geom_boxplot(outlier.shape = NA, width = 0.6, alpha = 0.8) +
  geom_jitter(width = 0.15, alpha = 0.15, size = 1) +
  scale_fill_manual(values = c(
    "local" = "#D9EAF7",
    "regional" = "#9ECAE1",
    "national" = "#4292C6",
    "continental" = "#08519C"
  )) +
  scale_x_discrete(labels = c(
    "local" = "Local",
    "regional" = "Regional",
    "national" = "National",
    "continental" = "Continental"
  )) +
  scale_y_log10(labels = scales::label_comma()) +
  labs(x = "Spatial scale", y = "Trap nights (log scale)") +
  theme_classic() +
  theme(
    legend.position = "none",
    axis.title.x = element_text(size = 11, margin = margin(t = 5)),
    axis.title.y = element_text(size = 11),
    axis.text = element_text(size = 9)
  )
p2

# Panel figure

panel <- p1 / p2
panel

ggsave("output/spatial_temporal.jpeg", width = 5, height = 6, dpi = 300)


## Q2: How have camera-trap study designs changed over time in scale and study approach? 

# Temporal scale

cor_data <- metadata |>
  dplyr::filter(
    !is.na(published_year),
    !is.na(temporal_scale_month)
  )

# Pearson correlation
cor.test(
  cor_data$published_year,
  cor_data$temporal_scale_month,
  method = "pearson"
)

cor.test(
  cor_data$published_year,
  cor_data$temporal_scale_month,
  method = "spearman"
)

cor_test <- cor.test(
  cor_data$published_year,
  cor_data$temporal_scale_month,
  method = "spearman"
)

ggplot2::ggplot(
  cor_data,
  ggplot2::aes(
    x = published_year,
    y = temporal_scale_month
  )
) +
  ggplot2::geom_point(alpha = 0.25, size = 1) +
  ggplot2::geom_smooth(
    method = "lm",
    se = TRUE
  ) +
  ggplot2::scale_y_log10() +
  ggplot2::labs(
    x = "Publication year",
    y = "Study duration (months; log scale)"
  ) +
  ggplot2::annotate(
    "text",
    x = Inf, y = Inf,
    label = sprintf(
      "Spearman's ρ = %.2f, P %s",
      cor_test$estimate,
      ifelse(cor_test$p.value < 0.001, "< 0.001",
             paste0("= ", round(cor_test$p.value, 3)))
    ),
    hjust = 1.1, vjust = 1.5
  ) +
  ggplot2::theme_classic()


## Spatial scale

broad_scale_metadata <- metadata %>%
  filter(spatial_scale %in% c("national", "continental", "global"))

table(broad_scale_metadata$published_year)


#
##
### 4. Focal species ----

# ## GBIF taxonomy lookup
# 
# get_gbif <- function(x, field) {
#   value <- x[[field]]
#   if (is.null(value)) NA_character_ else as.character(value)
# }
# 
# # Test GBIF lookup on 20 species
# 
# test_species <- species_lookup %>%
#   slice_head(n = 20)
# 
# test_gbif <- test_species %>%
#   mutate(gbif = map(scientific_name, ~ name_backbone(name = .x))) %>%
#   mutate(
#     accepted_name = map_chr(gbif, get_gbif, field = "scientificName"),
#     gbif_key = map_chr(gbif, get_gbif, field = "usageKey"),
#     class = map_chr(gbif, get_gbif, field = "class"),
#     order = map_chr(gbif, get_gbif, field = "order"),
#     family = map_chr(gbif, get_gbif, field = "family"),
#     match_type = map_chr(gbif, get_gbif, field = "matchType"),
#     confidence = map_dbl(gbif, ~ {
#       value <- .x[["confidence"]]
#       if (is.null(value)) NA_real_ else as.numeric(value)
#     })
#   ) %>%
#   select(-gbif)
# test_gbif
# 
# # Run GBIF full lookup
# 
# gbif_lookup <- species_lookup %>%
#   mutate(gbif = map(scientific_name, ~ name_backbone(name = .x))) %>%
#   mutate(
#     accepted_name = map_chr(gbif, get_gbif, field = "scientificName"),
#     gbif_key = map_chr(gbif, get_gbif, field = "usageKey"),
#     class = map_chr(gbif, get_gbif, field = "class"),
#     order = map_chr(gbif, get_gbif, field = "order"),
#     family = map_chr(gbif, get_gbif, field = "family"),
#     match_type = map_chr(gbif, get_gbif, field = "matchType"),
#     confidence = map_dbl(gbif, ~ {
#       value <- .x[["confidence"]]
#       if (is.null(value)) NA_real_ else as.numeric(value)
#     })
#   ) %>%
#   select(-gbif)
# 
# write.csv(gbif_lookup, "data/gbif_lookup_27082026.csv", row.names = FALSE)


# ## IUCN conservation status look up
# 
# iucn_token <- Sys.getenv("IUCN_TOKEN")
# 
# iucn_lookup <- gbif_lookup %>%
#   filter(
#     match_type == "EXACT",
#     !is.na(accepted_name)
#   ) %>%
#   distinct(accepted_name) %>%
#   mutate(
#     genus = word(accepted_name, 1),
#     species = word(accepted_name, 2)
#   )
# 
# get_iucn_status <- function(genus, species) {
#   
#   out <- tryCatch(
#     rl_species(
#       genus = genus,
#       species = species,
#       key = iucn_token
#     ),
#     error = function(e) NULL
#   )
#   
#   # Check that the API returned a valid list
#   if (!is.list(out) || is.null(out$assessments)) {
#     return(tibble(
#       iucn_category = NA_character_,
#       iucn_assessment_date = as.Date(NA)
#     ))
#   }
#   
#   df <- out$assessments
#   
#   # Check that assessments are available
#   if (!is.data.frame(df) || nrow(df) == 0) {
#     return(tibble(
#       iucn_category = NA_character_,
#       iucn_assessment_date = as.Date(NA)
#     ))
#   }
#   
#   # Keep the latest assessment
#   df_latest <- df %>%
#     filter(latest == TRUE)
#   
#   # If no assessment is explicitly marked latest,
#   # use the first available assessment
#   if (nrow(df_latest) == 0) {
#     df_latest <- df %>%
#       slice(1)
#   }
#   
#   # Extract the information we need
#   tibble(
#     iucn_category = df_latest$red_list_category_code[1],
#     iucn_assessment_date = as.Date(df_latest$assessment_date[1])
#   )
# }
# 
# # Test run
# 
# set.seed(123)
# 
# test_iucn <- iucn_lookup %>%
#   slice_sample(n = min(20, nrow(iucn_lookup))) %>%
#   mutate(
#     result = map2(genus, species, get_iucn_status)
#   ) %>%
#   unnest(result)
# 
# View(test_iucn)
# table(test_iucn$iucn_category, useNA = "ifany")
# 
# # Run all data
# 
# iucn_lookup <- iucn_lookup %>%
#   mutate(
#     result = map2(genus, species, get_iucn_status)
#   ) %>%
#   unnest(result) %>%
#   select(
#     accepted_name,
#     iucn_category,
#     iucn_assessment_date
#   )
# 
# table(iucn_lookup$iucn_category, useNA = "ifany")
# 
# write.csv(iucn_lookup, "data/iucn_lookup_27082026.csv", row.names = FALSE)


# ## Mammal traits lookup (PanTHERIA)
# 
# data(package = "traitdata")
# data(pantheria)
# 
# pantheria_traits <- pantheria %>%
#   select(
#     scientificNameStd,
#     AdultBodyMass_g,
#     ActivityCycle,
#     HomeRange_km2,
#     TrophicLevel,
#     SocialGrpSize
#   ) %>%
#   rename(
#     pantheria_name = scientificNameStd,
#     body_mass_g = AdultBodyMass_g,
#     activity_cycle = ActivityCycle,
#     home_range_km2 = HomeRange_km2,
#     trophic_level = TrophicLevel,
#     social_group_size = SocialGrpSize
#   ) %>%
#   distinct(pantheria_name, .keep_all = TRUE)
# 
# traits_lookup <- gbif_lookup %>%
#   filter(
#     match_type == "EXACT",
#     class == "Mammalia",
#     !is.na(accepted_name)
#   ) %>%
#   distinct(
#     scientific_name,
#     accepted_name,
#     gbif_key,
#     .keep_all = TRUE
#   ) %>%
#   mutate(
#     # Extract genus + species from accepted_name
#     matching_name = str_extract(
#       accepted_name,
#       "^[A-Z][a-z-]+ [a-z-]+"
#     ),
#     
#     # Manually correct four known taxonomic synonyms
#     matching_name = case_when(
#       matching_name == "Felis concolor" ~ "Puma concolor",
#       matching_name == "Felis caracal" ~ "Caracal caracal",
#       matching_name == "Pseudalopex gymnocercus" ~
#         "Lycalopex gymnocercus",
#       matching_name == "Tayassu tajacu" ~ "Pecari tajacu",
#       TRUE ~ matching_name
#     )
#   ) %>%
#   
#   # Match using the temporary matching_name
#   left_join(
#     pantheria_traits,
#     by = c("matching_name" = "pantheria_name")
#   ) %>%
#   
#   # Keep the original accepted_name unchanged
#   select(
#     scientific_name,
#     accepted_name,
#     gbif_key,
#     body_mass_g,
#     activity_cycle,
#     home_range_km2,
#     trophic_level,
#     social_group_size
#   )
# 
# write.csv(traits_lookup, "traits_lookup_27082026.csv", row.names = FALSE)


# # Clean species look up table and export
# 
# ## Mammal traits lookup (PanTHERIA)
# 
# data(package = "traitdata")
# data(pantheria)
# 
# pantheria_traits <- pantheria %>%
#   select(
#     scientificNameStd,
#     AdultBodyMass_g,
#     ActivityCycle,
#     HomeRange_km2,
#     TrophicLevel,
#     SocialGrpSize
#   ) %>%
#   rename(
#     pantheria_name = scientificNameStd,
#     body_mass_g = AdultBodyMass_g,
#     activity_cycle = ActivityCycle,
#     home_range_km2 = HomeRange_km2,
#     trophic_level = TrophicLevel,
#     social_group_size = SocialGrpSize
#   ) %>%
#   distinct(pantheria_name, .keep_all = TRUE)
# 
# traits_lookup <- gbif_lookup %>%
#   filter(
#     match_type == "EXACT",
#     class == "Mammalia",
#     !is.na(accepted_name)
#   ) %>%
#   distinct(
#     scientific_name,
#     accepted_name,
#     gbif_key,
#     .keep_all = TRUE
#   ) %>%
#   mutate(
#     # Extract genus + species from accepted_name
#     matching_name = str_extract(
#       accepted_name,
#       "^[A-Z][a-z-]+ [a-z-]+"
#     ),
# 
#     # Manually correct four known taxonomic synonyms
#     matching_name = case_when(
#       matching_name == "Felis concolor" ~ "Puma concolor",
#       matching_name == "Felis caracal" ~ "Caracal caracal",
#       matching_name == "Pseudalopex gymnocercus" ~
#         "Lycalopex gymnocercus",
#       matching_name == "Tayassu tajacu" ~ "Pecari tajacu",
#       TRUE ~ matching_name
#     )
#   ) %>%
# 
#   # Match using the temporary matching_name
#   left_join(
#     pantheria_traits,
#     by = c("matching_name" = "pantheria_name")
#   ) %>%
# 
#   # Keep the original accepted_name unchanged
#   select(
#     scientific_name,
#     accepted_name,
#     gbif_key,
#     body_mass_g,
#     activity_cycle,
#     home_range_km2,
#     trophic_level,
#     social_group_size
#   )
# 
# # remove duplicates
# 
# traits_lookup <- traits_lookup %>% distinct(gbif_key, .keep_all = TRUE)
# 
# write.csv(traits_lookup, "data/traits_lookup_27082026.csv", row.names = FALSE)


# ## Create species lookup table
# 
# gbif_lookup <- read.csv("gbif_lookup_27082026.csv")
# iucn_lookup <- read.csv("iucn_lookup_27082026.csv")
# traits_lookup <- read.csv("traits_lookup_27082026.csv")
# 
# species_lookup <- gbif_lookup %>%
#   left_join(iucn_lookup %>% select(accepted_name, iucn_category, iucn_assessment_date),
#             by = "accepted_name") %>%
#   left_join(traits_lookup %>% select(gbif_key, body_mass_g, activity_cycle, home_range_km2, trophic_level, social_group_size),
#             by = "gbif_key")
# 
# # Keep only animals
# 
# terrestrial_classes <- c("Mammalia", "Aves", "Amphibia", "Crocodylia", "Squamata", "Testudines")
# 
# species_lookup <- species_lookup %>%
#   filter(class %in% terrestrial_classes)
# 
# glimpse(species_lookup)
# 
# write.csv(species_lookup, "species_lookup_27082026.csv", row.names = FALSE)


## Join the species information back to metadata

metadata_species <- metadata %>%
  separate_rows(scientific_name, sep = ";") %>%
  mutate(scientific_name = str_squish(scientific_name)) %>%
  filter(!is.na(scientific_name), scientific_name != "") %>%
  left_join(species_lookup, by = "scientific_name") %>%
  filter(!is.na(gbif_key),
         match_type == "EXACT")

metadata_focal_species <- metadata %>%
  separate_rows(focal_species_scientific_name, sep = ";") %>%
  mutate(focal_species_scientific_name = str_squish(focal_species_scientific_name)) %>%
  filter(!is.na(focal_species_scientific_name), focal_species_scientific_name != "") %>%
  left_join(species_lookup, by = c("focal_species_scientific_name" = "scientific_name")) %>%
  filter(!is.na(gbif_key),
         match_type == "EXACT")

glimpse(metadata_species)
glimpse(metadata_focal_species)


## Q1: Which taxonomic groups dominate camera-trap research?

# Function: frequency and percentage at any taxonomic level
calc_frequency <- function(data, taxon, taxon_label = rlang::as_name(rlang::ensym(taxon))) {
  data %>%
    filter(!is.na({{ taxon }}), {{ taxon }} != "") %>%
    count({{ taxon }}, sort = TRUE, name = "frequency") %>%
    mutate(
      percentage = round(100 * frequency / sum(frequency), 1)
    )
}

# All species and focal species
species_frequency <- calc_frequency(metadata_species, scientific_name)
focal_species_frequency <- calc_frequency(
  metadata_focal_species, focal_species_scientific_name
)

# Family, order and class
family_frequency <- calc_frequency(metadata_species, family)
focal_family_frequency <- calc_frequency(metadata_focal_species, family)

order_frequency <- calc_frequency(metadata_species, order)
focal_order_frequency <- calc_frequency(metadata_focal_species, order)

class_frequency <- calc_frequency(metadata_species, class)
focal_class_frequency <- calc_frequency(metadata_focal_species, class)


## Q2: Has their representation changed over time? Are there emerging taxa?

# Function: taxonomic representation by period
calc_taxonomic_period <- function(data, taxon) {
  data %>%
    filter(!is.na({{ taxon }}), {{ taxon }} != "", !is.na(published_year)) %>%
    add_count({{ taxon }}, name = "total_frequency") %>%
    filter(total_frequency > 5) %>%
    mutate(
      period = case_when(
        published_year <= 2005 ~ "~2005",
        published_year <= 2010 ~ "2006–2010",
        published_year <= 2015 ~ "2011–2015",
        published_year <= 2020 ~ "2016–2020",
        published_year <= 2026 ~ "2021~",
        TRUE ~ NA_character_
      ),
      period = factor(
        period,
        levels = c("~2005", "2006–2010", "2011–2015", "2016–2020", "2021~")
      )
    ) %>%
    filter(!is.na(period)) %>%
    count({{ taxon }}, period, name = "frequency") %>%
    group_by(period) %>%
    mutate(percentage = frequency / sum(frequency) * 100) %>%
    ungroup() %>%
    arrange({{ taxon }}, period)
}

# All species
class_period <- calc_taxonomic_period(metadata_species, class)
order_period <- calc_taxonomic_period(metadata_species, order)
family_period <- calc_taxonomic_period(metadata_species, family)
species_period <- calc_taxonomic_period(metadata_species, scientific_name)

# Focal species
focal_class_period <- calc_taxonomic_period(metadata_focal_species, class)
focal_order_period <- calc_taxonomic_period(metadata_focal_species, order)
focal_family_period <- calc_taxonomic_period(metadata_focal_species, family)
focal_species_period <- calc_taxonomic_period(
  metadata_focal_species, focal_species_scientific_name
)


# Function: compare pre-2021 vs recent representation
calc_taxonomic_growth <- function(data, taxon) {
  data %>%
    filter(!is.na({{ taxon }}), {{ taxon }} != "", !is.na(published_year)) %>%
    mutate(
      period = if_else(published_year < 2021, "Pre-2021", "2021–2026")
    ) %>%
    count({{ taxon }}, period, name = "frequency") %>%
    group_by(period) %>%
    mutate(percentage = frequency / sum(frequency) * 100) %>%
    ungroup() %>%
    pivot_wider(
      names_from = period,
      values_from = c(frequency, percentage),
      values_fill = 0
    ) %>%
    rename(
      pre_2021_frequency = `frequency_Pre-2021`,
      recent_frequency = `frequency_2021–2026`,
      pre_2021_percentage = `percentage_Pre-2021`,
      recent_percentage = `percentage_2021–2026`
    ) %>%
    mutate(
      change_percentage_points = recent_percentage - pre_2021_percentage,
      growth_rate = if_else(
        pre_2021_percentage == 0,
        NA_real_,
        (recent_percentage - pre_2021_percentage) /
          pre_2021_percentage * 100
      )
    ) %>%
    arrange(desc(change_percentage_points))
}

# All species
class_growth <- calc_taxonomic_growth(metadata_species, class)
order_growth <- calc_taxonomic_growth(metadata_species, order)
family_growth <- calc_taxonomic_growth(metadata_species, family)
species_growth <- calc_taxonomic_growth(metadata_species, scientific_name)

# Focal species
focal_class_growth <- calc_taxonomic_growth(metadata_focal_species, class)
focal_order_growth <- calc_taxonomic_growth(metadata_focal_species, order)
focal_family_growth <- calc_taxonomic_growth(metadata_focal_species, family)
focal_species_growth <- calc_taxonomic_growth(
  metadata_focal_species, focal_species_scientific_name
)

# Export data

growth_all <- bind_rows(
  class_growth %>% mutate(taxonomic_level = "Class"),
  order_growth %>% mutate(taxonomic_level = "Order"),
  family_growth %>% mutate(taxonomic_level = "Family"),
  species_growth %>% mutate(taxonomic_level = "Species")
) %>%
  relocate(taxonomic_level)

growth_focal <- bind_rows(
  focal_class_growth %>% mutate(taxonomic_level = "Class"),
  focal_order_growth %>% mutate(taxonomic_level = "Order"),
  focal_family_growth %>% mutate(taxonomic_level = "Family"),
  focal_species_growth %>% mutate(taxonomic_level = "Species")
) %>%
  relocate(taxonomic_level)

period_all <- bind_rows(
  class_period %>% mutate(taxonomic_level = "Class"),
  order_period %>% mutate(taxonomic_level = "Order"),
  family_period %>% mutate(taxonomic_level = "Family"),
  species_period %>% mutate(taxonomic_level = "Species")
) %>%
  relocate(taxonomic_level)

period_focal <- bind_rows(
  focal_class_period %>% mutate(taxonomic_level = "Class"),
  focal_order_period %>% mutate(taxonomic_level = "Order"),
  focal_family_period %>% mutate(taxonomic_level = "Family"),
  focal_species_period %>% mutate(taxonomic_level = "Species")
) %>%
  relocate(taxonomic_level)

tables <- list(
  Growth_all = growth_all,
  Growth_focal = growth_focal,
  Period_all = period_all,
  Period_focal = period_focal
)

wb <- createWorkbook()

for (sheet in names(tables)) {
  addWorksheet(wb, sheet)
  writeData(wb, sheet, tables[[sheet]])
  
  n_cols <- ncol(tables[[sheet]])
  addFilter(wb, sheet, rows = 1, cols = 1:n_cols)
  setColWidths(wb, sheet, cols = 1:n_cols, widths = "auto")
  freezePane(wb, sheet, firstRow = TRUE)
}

saveWorkbook(
  wb,
  "taxonomic_representation_results_07092026.xlsx",
  overwrite = TRUE
)

# Emerging taxa for orders

order_class_lookup <- species_lookup %>%
  filter(!is.na(class), !is.na(order)) %>%
  distinct(class, order)

order_growth <- calc_taxonomic_growth(metadata_species, order) %>%
  left_join(order_class_lookup, by = "order") %>%
  relocate(class, .before = order)

order_focal_growth <- calc_taxonomic_growth(metadata_focal_species, order) %>%
  left_join(order_class_lookup, by = "order") %>%
  relocate(class, .before = order)

emerging_orders <- order_growth %>%
  mutate(
    recent_ratio = recent_frequency / (pre_2021_frequency + recent_frequency)
  ) %>%
  filter(
    recent_frequency >= 5,
    recent_ratio >= 0.50,
    change_percentage_points > 0
  ) %>%
  arrange(desc(recent_ratio), desc(recent_frequency))

emerging_focal_orders <- order_focal_growth %>%
  mutate(
    recent_ratio = recent_frequency / (pre_2021_frequency + recent_frequency)
  ) %>%
  filter(
    recent_frequency >= 5,
    recent_ratio >= 0.50,
    change_percentage_points > 0
  ) %>%
  arrange(desc(recent_ratio), desc(recent_frequency))


## Q3: What is the richness, diversity, and evenness growth?

# Growth by period

taxonomic_richness <- metadata_species %>%
  filter(!is.na(published_year)) %>%
  mutate(
    period = case_when(
      published_year <= 2005 ~ "~2005",
      published_year <= 2010 ~ "2006–2010",
      published_year <= 2015 ~ "2011–2015",
      published_year <= 2020 ~ "2016–2020",
      published_year <= 2026 ~ "2021~"
    )
  ) %>%
  group_by(period) %>%
  summarise(
    scientific_name = n_distinct(scientific_name, na.rm = TRUE),
    class = n_distinct(class, na.rm = TRUE),
    order = n_distinct(order, na.rm = TRUE),
    family = n_distinct(family, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  pivot_longer(
    cols = -period,
    names_to = "taxon",
    values_to = "richness"
  ) %>%
  pivot_wider(
    names_from = period,
    values_from = richness
  ) %>%
  mutate(
    growth = `2021~` / `~2005`
  ) %>%
  select(
    taxon,
    `~2005`,
    `2006–2010`,
    `2011–2015`,
    `2016–2020`,
    `2021~`,
    growth
  )

period <- metadata_species %>%
  filter(!is.na(published_year)) %>%
  mutate(
    period = case_when(
      published_year <= 2005 ~ "~2005",
      published_year <= 2010 ~ "2006–2010",
      published_year <= 2015 ~ "2011–2015",
      published_year <= 2020 ~ "2016–2020",
      published_year <= 2026 ~ "2021~"
    )
  )

taxonomic_diversity <- period %>%
  pivot_longer(
    cols = c(scientific_name, class, order, family),
    names_to = "taxon",
    values_to = "taxon_name"
  ) %>%
  filter(!is.na(taxon_name), taxon_name != "") %>%
  count(period, taxon, taxon_name) %>%
  group_by(period, taxon) %>%
  summarise(
    richness = n_distinct(taxon_name),
    shannon = vegan::diversity(n),
    evenness = shannon / log(richness),
    .groups = "drop"
  ) %>%
  pivot_longer(
    cols = c(richness, shannon, evenness),
    names_to = "metric",
    values_to = "value"
  ) %>%
  mutate(measure = paste(taxon, metric, sep = "_")) %>%
  select(measure, period, value) %>%
  pivot_wider(
    names_from = period,
    values_from = value
  ) %>%
  mutate(
    growth = `2021~` / `~2005`,
    growth_since_2005 = `2021~` / `2006–2010`,
    growth_since_2015 = `2021~` / `2011–2015`
  ) %>%
  select(
    measure,
    `~2005`,
    `2006–2010`,
    `2011–2015`,
    `2016–2020`,
    `2021~`,
    growth,
    growth_since_2005,
    growth_since_2015
  )

period_focal <- metadata_focal_species %>%
  filter(!is.na(published_year)) %>%
  mutate(
    period = case_when(
      published_year <= 2005 ~ "~2005",
      published_year <= 2010 ~ "2006–2010",
      published_year <= 2015 ~ "2011–2015",
      published_year <= 2020 ~ "2016–2020",
      published_year <= 2026 ~ "2021~"
    )
  )

taxonomic_focal_diversity <- period_focal %>%
  pivot_longer(
    cols = c(scientific_name, class, order, family),
    names_to = "taxon",
    values_to = "taxon_name"
  ) %>%
  filter(!is.na(taxon_name), taxon_name != "") %>%
  count(period, taxon, taxon_name) %>%
  group_by(period, taxon) %>%
  summarise(
    richness = n_distinct(taxon_name),
    shannon = vegan::diversity(n),
    evenness = shannon / log(richness),
    .groups = "drop"
  ) %>%
  pivot_longer(
    cols = c(richness, shannon, evenness),
    names_to = "metric",
    values_to = "value"
  ) %>%
  mutate(measure = paste(taxon, metric, sep = "_")) %>%
  select(measure, period, value) %>%
  pivot_wider(
    names_from = period,
    values_from = value
  ) %>%
  mutate(
    growth = `2021~` / `~2005`,
    growth_since_2005 = `2021~` / `2006–2010`,
    growth_since_2015 = `2021~` / `2011–2015`
  ) %>%
  select(
    measure,
    `~2005`,
    `2006–2010`,
    `2011–2015`,
    `2016–2020`,
    `2021~`,
    growth,
    growth_since_2005,
    growth_since_2015
  )

write.xlsx(taxonomic_diversity, "taxonomic_diversity_growth_07092026.xlsx")
write.xlsx(taxonomic_focal_diversity, "taxonomic_focal_diversity_growth_07092026.xlsx")

# Function to calculate cumulative species richness, Shannon diversity, and evenness by year

calc_cumulative_diversity <- function(data, species_col, label) {
  data %>%
    filter(
      !is.na(published_year),
      published_year <= 2025,
      !is.na({{ species_col }}),
      {{ species_col }} != ""
    ) %>%
    count(published_year, {{ species_col }}, name = "n") %>%
    complete(
      {{ species_col }},
      published_year = 1995:2025,
      fill = list(n = 0)
    ) %>%
    arrange({{ species_col }}, published_year) %>%
    group_by({{ species_col }}) %>%
    mutate(cumulative_n = cumsum(n)) %>%
    ungroup() %>%
    group_by(published_year) %>%
    summarise(
      richness = sum(cumulative_n > 0),
      shannon = vegan::diversity(cumulative_n),
      evenness = if_else(
        richness > 1,
        shannon / log(richness),
        NA_real_
      ),
      .groups = "drop"
    ) %>%
    mutate(species_type = label)
}

calc_growth <- function(data, type, start_year = 2005, end_year = 2025) {
  model <- data %>%
    filter(
      .data$species_type == type,
      published_year >= start_year,
      published_year <= end_year,
      richness > 0
    ) %>%
    lm(log(richness) ~ published_year, data = .)
  
  slope <- coef(model)[["published_year"]]
  se <- summary(model)$coefficients["published_year", "Std. Error"]
  
  tibble(
    species_type = type,
    growth = (exp(slope) - 1) * 100,
    growth_se = exp(slope) * se * 100
  )
}

# Apply to dataset and combine

diversity_all <- calc_cumulative_diversity(
  metadata_species,
  scientific_name,
  "All species"
)

diversity_focal <- calc_cumulative_diversity(
  metadata_focal_species,
  focal_species_scientific_name,
  "Focal species"
)

taxonomic_diversity_yearly <- bind_rows(
  diversity_all,
  diversity_focal
)

# Richness

richness_growth <- bind_rows(
  calc_growth(taxonomic_diversity_yearly, "All species"),
  calc_growth(taxonomic_diversity_yearly, "Focal species")
)
richness_growth

richness_accumulation <- ggplot(
  taxonomic_diversity_yearly,
  aes(x = published_year, y = richness, color = species_type)
) +
  geom_line(linewidth = 1) +
  geom_point(size = 2) +
  scale_x_continuous(breaks = seq(1995, 2025, 5)) +
  labs(
    x = NULL,
    y = "Cumulative number\nof species",
    color = NULL
  ) +
  scale_color_manual(
    values = c(
      "All species" = "#0072B2",
      "Focal species" = "#D55E00"
    )
  ) +
  theme_classic() +
  theme(
    text = element_text(size = 10),
    axis.title = element_text(size = 11),
    legend.text = element_text(size = 11),
    legend.position = c(0.05, 0.95),
    legend.justification = c(0, 1)
  )

richness_accumulation

# Shannon diversity

shannon_growth <- ggplot(
  taxonomic_diversity_yearly,
  aes(x = published_year, y = shannon, color = species_type)
) +
  geom_line(linewidth = 1) +
  geom_point(size = 2) +
  scale_x_continuous(breaks = seq(1995, 2025, 5)) +
  labs(
    x = NULL,
    y = "Shannon diversity\nper year",
    color = NULL
  ) +
  scale_color_manual(
    values = c(
      "All species" = "#0072B2",
      "Focal species" = "#D55E00"
    )
  ) +
  theme_classic() +
  theme(
    text = element_text(size = 10),
    axis.title = element_text(size = 11),
    legend.position = "none"
  )

shannon_growth

# Evenness

evenness_growth <- ggplot(
  taxonomic_diversity_yearly,
  aes(
    x = published_year, y = evenness, color = species_type
  )
) +
  geom_line(linewidth = 1) +
  geom_point(size = 2) +
  scale_x_continuous(
    breaks = seq(1995, 2025, 5)
  ) +
  labs(
    x = "Year",
    y = "Pielou's evenness\nper year",
    color = NULL
  ) +
  scale_color_manual(
    values = c(
      "All species" = "#0072B2",
      "Focal species" = "#D55E00"
    )
  ) +
  theme_classic() +
  theme(
    text = element_text(size = 10),
    axis.title = element_text(size = 11),
    axis.title.x = element_text(margin = margin(t = 5)),
    legend.position = "none"
  )

evenness_growth

# Panel figure

panel <- richness_accumulation / shannon_growth / evenness_growth
panel

ggsave("output/species_accumulation.jpeg", width = 5, height = 8, dpi = 300)


## Q4: Are focal species of greater conservation concern more likely to receive long-term, broad-scale, or experimental studies?

## A4: Nothing special... leave it!

iucn_order <- c("CR", "EN", "VU", "NT", "LC")

# Spatial scale

spatial_iucn <- metadata_focal_species %>%
  filter(
    iucn_category %in% iucn_order,
    !is.na(spatial_scale)
  ) %>%
  mutate(
    iucn_category = factor(iucn_category, levels = iucn_order)
  ) %>%
  count(iucn_category, spatial_scale) %>%
  group_by(iucn_category) %>%
  mutate(percentage = n / sum(n) * 100) %>%
  ungroup()

ggplot(
  spatial_iucn,
  aes(
    x = iucn_category,
    y = percentage,
    fill = spatial_scale
  )
) +
  geom_col() +
  scale_y_continuous(
    labels = \(x) paste0(x, "%")
  ) +
  theme_classic() +
  labs(
    x = "IUCN conservation status",
    y = "Studies (%)",
    fill = "Spatial scale"
  )

# Temporal scale

temporal_iucn <- metadata_focal_species %>%
  filter(
    iucn_category %in% iucn_order,
    !is.na(temporal_scale_category)
  ) %>%
  mutate(
    iucn_category = factor(iucn_category, levels = iucn_order)
  ) %>%
  count(iucn_category, temporal_scale_category) %>%
  group_by(iucn_category) %>%
  mutate(percentage = n / sum(n) * 100) %>%
  ungroup()

ggplot(
  temporal_iucn,
  aes(
    x = iucn_category,
    y = percentage,
    fill = temporal_scale_category
  )
) +
  geom_col() +
  scale_y_continuous(
    labels = \(x) paste0(x, "%")
  ) +
  theme_classic() +
  labs(
    x = "IUCN conservation status",
    y = "Studies (%)",
    fill = "Temporal scale"
  )

# Study approach

approach_iucn <- metadata_focal_species %>%
  filter(
    iucn_category %in% iucn_order,
    !is.na(study_approach)
  ) %>%
  mutate(
    iucn_category = factor(iucn_category, levels = iucn_order)
  ) %>%
  count(iucn_category, study_approach) %>%
  group_by(iucn_category) %>%
  mutate(percentage = n / sum(n) * 100) %>%
  ungroup()

ggplot(
  approach_iucn,
  aes(
    x = iucn_category,
    y = percentage,
    fill = study_approach
  )
) +
  geom_col() +
  scale_y_continuous(
    labels = \(x) paste0(x, "%")
  ) +
  theme_classic() +
  labs(
    x = "IUCN conservation status",
    y = "Studies (%)",
    fill = "Study approach"
  )


## Q5: How does the taxonomic, trophic guild, and conservation composition of focal species vary geographically? 

table(metadata_focal_species$region, useNA = "ifany")

region_order <- c(
  "Asia",
  "Central, Caribbean, South America",
  "North America",
  "Africa",
  "Europe",
  "Oceania",
  "Antarctica"
)

# Which classes dominate in each region?

region_class <- metadata_focal_species %>%
  filter(
    !is.na(region),
    !is.na(class)
  ) %>%
  count(region, class) %>%
  group_by(region) %>%
  mutate(
    percentage = n / sum(n) * 100
  ) %>%
  ungroup() %>%
  arrange(region, desc(percentage))

region_class <- region_class %>%
  mutate(
    region = factor(region, levels = region_order)
  )

ggplot(
  region_class,
  aes(x = region, y = percentage, fill = class)
) +
  geom_col() +
  scale_fill_manual(values = okabe_ito) +
  scale_y_continuous(limits = c(0, 100)) +
  theme_classic() +
  labs(
    x = "Region",
    y = "Focal species composition (%)",
    fill = "Class"
  )

# Which conservation status dominate in each region?

# region_iucn <- metadata_focal_species %>%
#   filter(
#     !is.na(region),
#     !is.na(iucn_category),
#     iucn_category %in% iucn_order
#   ) %>%
#   count(region, iucn_category) %>%
#   group_by(region) %>%
#   mutate(
#     percentage = n / sum(n) * 100
#   ) %>%
#   ungroup() %>%
#   mutate(
#     region = factor(region, levels = region_order),
#     iucn_category = factor(iucn_category, levels = iucn_order)
#   )
# 
# ggplot(
#   region_iucn,
#   aes(x = region, y = percentage, fill = iucn_category)
# ) +
#   geom_col() +
#   scale_fill_manual(values = okabe_ito[1:5]) +
#   scale_y_continuous(limits = c(0, 100)) +
#   theme_classic() +
#   labs(
#     x = "Region",
#     y = "Focal species composition (%)",
#     fill = "IUCN category"
#   )

# Group IUCN categories

region_iucn_grouped <- metadata_focal_species %>%
  filter(
    !is.na(region),
    !is.na(iucn_category),
    iucn_category %in% iucn_order
  ) %>%
  mutate(
    conservation_status = case_when(
      iucn_category %in% c("CR", "EN", "VU") ~ "Threatened",
      iucn_category %in% c("NT", "LC") ~ "Not threatened"
    )
  ) %>%
  count(region, conservation_status) %>%
  group_by(region) %>%
  mutate(
    percentage = n / sum(n) * 100
  ) %>%
  ungroup() %>%
  mutate(
    region = factor(region, levels = region_order),
    conservation_status = factor(
      conservation_status,
      levels = c("Threatened", "Not threatened")
    )
  )

ggplot(
  region_iucn_grouped,
  aes(x = region, y = percentage, fill = conservation_status)
) +
  geom_col() +
  scale_fill_manual(values = okabe_ito[c(1, 5)]) +
  scale_y_continuous(limits = c(0, 100)) +
  theme_classic() +
  labs(
    x = "Region",
    y = "Focal species composition (%)",
    fill = "Conservation status"
  )

# Which trophic guild dominate in each region?

region_trophic <- metadata_focal_species %>%
  filter(
    !is.na(region),
    !is.na(trophic_level)
  ) %>%
  mutate(
    trophic_level = case_when(
      trophic_level == 1 ~ "Herbivore",
      trophic_level == 2 ~ "Omnivore",
      trophic_level == 3 ~ "Carnivore",
      TRUE ~ NA_character_
    )
  ) %>%
  filter(!is.na(trophic_level)) %>%
  count(region, trophic_level) %>%
  group_by(region) %>%
  mutate(
    percentage = n / sum(n) * 100
  ) %>%
  ungroup() %>%
  mutate(
    region = factor(region, levels = region_order),
    trophic_level = factor(
      trophic_level,
      levels = c("Herbivore", "Omnivore", "Carnivore")
    )
  ) %>%
  arrange(region, trophic_level)

ggplot(
  region_trophic,
  aes(x = region, y = percentage, fill = trophic_level)
) +
  geom_col() +
  scale_fill_manual(
    values = okabe_ito[c(3, 7, 5)]
  ) +
  scale_y_continuous(limits = c(0, 100)) +
  theme_classic() +
  labs(
    x = "Region",
    y = "Focal species composition (%)",
    fill = "Trophic guild"
  )


## Q6: Do focal species characteristics predict the spatial and temporal
## scale of camera-trap studies?

## A6: Little evidence of strong associations. Carnivores may be
## disproportionately represented in larger-scale studies.


# 1. Prepare data


spatial_traits <- metadata_focal_species %>%
  filter(
    !is.na(spatial_scale),
    !is.na(body_mass_g),
    !is.na(home_range_km2),
    body_mass_g > 0,
    home_range_km2 > 0
  ) %>%
  mutate(
    spatial_num = as.numeric(factor(
      spatial_scale,
      levels = c("local", "regional", "national", "continental")
    ))
  )

temporal_traits <- metadata_focal_species %>%
  filter(
    !is.na(temporal_scale_month),
    !is.na(body_mass_g),
    !is.na(home_range_km2),
    temporal_scale_month > 0,
    body_mass_g > 0,
    home_range_km2 > 0
  )

# 2. Continuous traits vs spatial scale

# Body mass

ggplot(spatial_traits, aes(x = spatial_scale, y = body_mass_g)) +
  geom_boxplot(outlier.shape = NA) +
  geom_jitter(width = 0.15, alpha = 0.25, size = 1) +
  scale_y_log10() +
  theme_classic() +
  labs(
    x = "Spatial scale",
    y = "Body mass (g, log scale)"
  )

# Home range

ggplot(spatial_traits, aes(x = spatial_scale, y = home_range_km2)) +
  geom_boxplot(outlier.shape = NA) +
  geom_jitter(width = 0.15, alpha = 0.25, size = 1) +
  scale_y_log10() +
  theme_classic() +
  labs(
    x = "Spatial scale",
    y = "Home range (km², log scale)"
  )

# Spearman correlations

cor.test(
  spatial_traits$body_mass_g,
  spatial_traits$spatial_num,
  method = "spearman"
)

cor.test(
  spatial_traits$home_range_km2,
  spatial_traits$spatial_num,
  method = "spearman"
)

# 3. Continuous traits vs temporal scale

# Body mass

ggplot(
  temporal_traits,
  aes(x = body_mass_g, y = temporal_scale_month)
) +
  geom_point(alpha = 0.3) +
  geom_smooth(method = "lm", se = TRUE) +
  scale_x_log10() +
  scale_y_log10() +
  theme_classic() +
  labs(
    x = "Body mass (g, log scale)",
    y = "Study duration (months, log scale)"
  )

# Home range

ggplot(
  temporal_traits,
  aes(x = home_range_km2, y = temporal_scale_month)
) +
  geom_point(alpha = 0.3) +
  geom_smooth(method = "lm", se = TRUE) +
  scale_x_log10() +
  scale_y_log10() +
  theme_classic() +
  labs(
    x = "Home range (km², log scale)",
    y = "Study duration (months, log scale)"
  )

# Spearman correlations

cor.test(
  log10(temporal_traits$body_mass_g),
  log10(temporal_traits$temporal_scale_month),
  method = "spearman"
)

cor.test(
  log10(temporal_traits$home_range_km2),
  log10(temporal_traits$temporal_scale_month),
  method = "spearman"
)

# 4. Categorical traits vs spatial and temporal scale

# Recode categorical traits once

categorical_traits <- metadata_focal_species %>%
  mutate(
    activity_cycle = case_when(
      activity_cycle == 1 ~ "Nocturnal",
      activity_cycle == 2 ~ "Mixed",
      activity_cycle == 3 ~ "Diurnal",
      TRUE ~ NA_character_
    ),
    trophic_level = case_when(
      trophic_level == 1 ~ "Herbivore",
      trophic_level == 2 ~ "Omnivore",
      trophic_level == 3 ~ "Carnivore",
      TRUE ~ NA_character_
    )
  )


# Activity cycle × spatial scale

activity_spatial <- categorical_traits %>%
  filter(!is.na(spatial_scale), !is.na(activity_cycle)) %>%
  count(spatial_scale, activity_cycle) %>%
  group_by(spatial_scale) %>%
  mutate(percentage = n / sum(n) * 100) %>%
  ungroup()

ggplot(
  activity_spatial,
  aes(x = spatial_scale, y = percentage, fill = activity_cycle)
) +
  geom_col() +
  scale_y_continuous(limits = c(0, 100)) +
  theme_classic() +
  labs(
    x = "Spatial scale",
    y = "Focal species composition (%)",
    fill = "Activity cycle"
  )


# Activity cycle × temporal scale

activity_temporal <- categorical_traits %>%
  filter(
    !is.na(temporal_scale_category),
    !is.na(activity_cycle)
  ) %>%
  count(temporal_scale_category, activity_cycle) %>%
  group_by(temporal_scale_category) %>%
  mutate(percentage = n / sum(n) * 100) %>%
  ungroup()

ggplot(
  activity_temporal,
  aes(
    x = temporal_scale_category,
    y = percentage,
    fill = activity_cycle
  )
) +
  geom_col() +
  scale_y_continuous(limits = c(0, 100)) +
  theme_classic() +
  labs(
    x = "Temporal scale",
    y = "Focal species composition (%)",
    fill = "Activity cycle"
  )


# Trophic guild × spatial scale

trophic_spatial <- categorical_traits %>%
  filter(!is.na(spatial_scale), !is.na(trophic_level)) %>%
  count(spatial_scale, trophic_level) %>%
  group_by(spatial_scale) %>%
  mutate(percentage = n / sum(n) * 100) %>%
  ungroup()

ggplot(
  trophic_spatial,
  aes(x = spatial_scale, y = percentage, fill = trophic_level)
) +
  geom_col() +
  scale_y_continuous(limits = c(0, 100)) +
  theme_classic() +
  labs(
    x = "Spatial scale",
    y = "Focal species composition (%)",
    fill = "Trophic guild"
  )


# Trophic guild × temporal scale

trophic_temporal <- categorical_traits %>%
  filter(
    !is.na(temporal_scale_category),
    !is.na(trophic_level)
  ) %>%
  count(temporal_scale_category, trophic_level) %>%
  group_by(temporal_scale_category) %>%
  mutate(percentage = n / sum(n) * 100) %>%
  ungroup()

ggplot(
  trophic_temporal,
  aes(
    x = temporal_scale_category,
    y = percentage,
    fill = trophic_level
  )
) +
  geom_col() +
  scale_y_continuous(limits = c(0, 100)) +
  theme_classic() +
  labs(
    x = "Temporal scale",
    y = "Focal species composition (%)",
    fill = "Trophic guild"
  )

# 5. Statistical tests for categorical traits

# Activity cycle × spatial scale

activity_spatial_table <- with(
  categorical_traits,
  table(spatial_scale, activity_cycle)
)

chisq.test(activity_spatial_table)

cramers_v(activity_spatial_table)


# Trophic guild × spatial scale

trophic_spatial_table <- with(
  categorical_traits,
  table(spatial_scale, trophic_level)
)

chisq.test(trophic_spatial_table)

cramers_v(trophic_spatial_table)

# 6. Export results

# Statistical test results

continuous_tests <- tibble(
  comparison = c(
    "Body mass × spatial scale",
    "Home range × spatial scale",
    "Body mass × study duration",
    "Home range × study duration"
  ),
  spearman_rho = c(
    cor.test(
      spatial_traits$body_mass_g,
      spatial_traits$spatial_num,
      method = "spearman"
    )$estimate,
    cor.test(
      spatial_traits$home_range_km2,
      spatial_traits$spatial_num,
      method = "spearman"
    )$estimate,
    cor.test(
      log10(temporal_traits$body_mass_g),
      log10(temporal_traits$temporal_scale_month),
      method = "spearman"
    )$estimate,
    cor.test(
      log10(temporal_traits$home_range_km2),
      log10(temporal_traits$temporal_scale_month),
      method = "spearman"
    )$estimate
  ),
  p_value = c(
    cor.test(
      spatial_traits$body_mass_g,
      spatial_traits$spatial_num,
      method = "spearman"
    )$p.value,
    cor.test(
      spatial_traits$home_range_km2,
      spatial_traits$spatial_num,
      method = "spearman"
    )$p.value,
    cor.test(
      log10(temporal_traits$body_mass_g),
      log10(temporal_traits$temporal_scale_month),
      method = "spearman"
    )$p.value,
    cor.test(
      log10(temporal_traits$home_range_km2),
      log10(temporal_traits$temporal_scale_month),
      method = "spearman"
    )$p.value
  )
)

categorical_tests <- tibble(
  comparison = c(
    "Activity cycle × spatial scale",
    "Trophic guild × spatial scale"
  ),
  chi_square = c(
    unname(chisq.test(activity_spatial_table)$statistic),
    unname(chisq.test(trophic_spatial_table)$statistic)
  ),
  df = c(
    unname(chisq.test(activity_spatial_table)$parameter),
    unname(chisq.test(trophic_spatial_table)$parameter)
  ),
  p_value = c(
    chisq.test(activity_spatial_table)$p.value,
    chisq.test(trophic_spatial_table)$p.value
  ),
  cramers_v = c(
    cramers_v(activity_spatial_table)$Cramers_v,
    cramers_v(trophic_spatial_table)$Cramers_v
  )
)

categorical_tests <- tibble(
  comparison = c(
    "Activity cycle × spatial scale",
    "Trophic guild × spatial scale"
  ),
  chi_square = c(
    unname(chisq.test(activity_spatial_table)$statistic),
    unname(chisq.test(trophic_spatial_table)$statistic)
  ),
  df = c(
    unname(chisq.test(activity_spatial_table)$parameter),
    unname(chisq.test(trophic_spatial_table)$parameter)
  ),
  p_value = c(
    chisq.test(activity_spatial_table)$p.value,
    chisq.test(trophic_spatial_table)$p.value
  ),
  cramers_v = c(
    cramers_v(activity_spatial_table)$Cramers_v,
    cramers_v(trophic_spatial_table)$Cramers_v
  )
)

# Export to Excel

write.xlsx(
  list(
    "Activity_spatial" = activity_spatial,
    "Activity_temporal" = activity_temporal,
    "Trophic_spatial" = trophic_spatial,
    "Trophic_temporal" = trophic_temporal,
    "Continuous_tests" = continuous_tests,
    "Categorical_tests" = categorical_tests
  ),
  file = "species_traits_design_07092026.xlsx",
  overwrite = TRUE
)


#
##
### 5. Study scope ----


## Inspect 104 studies not assigned to any predefined study scope

d <- metadata%>%
  filter(is.na(metadata$study_scope))


## Q1: How do taxa vary in their study scope?

# Prepare data

scope_taxa <- metadata %>%
  filter(
    !is.na(primary_taxonomic_group),
    !is.na(ref_id)
  ) %>%
  select(
    ref_id,
    primary_taxonomic_group,
    all_of(scope_vars)
  ) %>%
  pivot_longer(
    cols = all_of(scope_vars),
    names_to = "study_scope",
    values_to = "included"
  ) %>%
  group_by(primary_taxonomic_group, study_scope) %>%
  summarise(
    n = sum(included == 1, na.rm = TRUE),
    total = n_distinct(ref_id),
    percentage = 100 * n / total,
    .groups = "drop"
  )


# Plot

ggplot(
  scope_taxa,
  aes(
    x = primary_taxonomic_group,
    y = percentage,
    fill = study_scope
  )
) +
  geom_col(position = "dodge") +
  scale_fill_manual(
    values = okabe_ito[1:6],
    labels = c(
      "Population ecology",
      "Spatial ecology and habitat use",
      "Social and behavioural ecology",
      "Species interactions",
      "Community ecology and biodiversity",
      "Human–wildlife interactions and disturbance"
    )
  ) +
  scale_y_continuous(limits = c(0, 100)) +
  theme_classic() +
  labs(
    x = "Primary taxonomic group",
    y = "Studies including objective (%)",
    fill = "Study scope"
  )

# Statistical tests

# Overall association: taxonomic group × each study scope

scope_tests <- lapply(scope_vars, function(scope) {
  
  tab <- metadata %>%
    filter(
      !is.na(primary_taxonomic_group),
      !is.na(.data[[scope]])
    ) %>%
    distinct(ref_id, primary_taxonomic_group, .data[[scope]]) %>%
    count(primary_taxonomic_group, .data[[scope]]) %>%
    pivot_wider(
      names_from = all_of(scope),
      values_from = n,
      values_fill = 0
    )
  
  tab_matrix <- as.matrix(tab[, -1])
  rownames(tab_matrix) <- tab$primary_taxonomic_group
  
  test <- chisq.test(tab_matrix)
  
  tibble(
    study_scope = scope,
    chi_square = unname(test$statistic),
    df = unname(test$parameter),
    p_value = test$p.value,
    cramers_v = effectsize::cramers_v(tab_matrix)$Cramers_v
  )
}) %>%
  bind_rows()

scope_tests


## Q2: How do regions vary in their study scope?

metadata_region_scope <- metadata_region %>%
  filter(
    !is.na(region),
    !is.na(study_scope)
  ) %>%
  select(ref_id, region, study_scope) %>%
  separate_rows(study_scope, sep = ";") %>%
  distinct(ref_id, region, study_scope)

region_totals <- metadata_region_scope %>%
  distinct(ref_id, region) %>%
  count(region, name = "total")

scope_region <- metadata_region_scope %>%
  count(region, study_scope, name = "n") %>%
  left_join(region_totals, by = "region") %>%
  mutate(
    percentage = 100 * n / total,
    region = factor(region, levels = region_order)
  )

ggplot(
  scope_region,
  aes(
    x = region,
    y = percentage,
    fill = study_scope
  )
) +
  geom_col(position = "dodge") +
  scale_fill_manual(
    values = okabe_ito[1:6],
    labels = c(
      "Population ecology",
      "Spatial ecology and habitat",
      "Social and behavioural ecology",
      "Species interactions",
      "Community ecology and biodiversity",
      "Human–wildlife interactions and disturbance"
    )
  ) +
  scale_y_continuous(limits = c(0, 100)) +
  theme_classic() +
  labs(
    x = "Region",
    y = "Studies including objective (%)",
    fill = "Study scope"
  )


## Q3: What are the emerging topics?

# P11: Growth by detailed study scope

scope_detailed_long <- metadata %>%
  separate_rows(study_scope_detailed, sep = ";")

scope_growth_2005 <- calculate_overall_growth(
  scope_detailed_long,
  group_var = "study_scope_detailed",
  start_year = 2005
)

scope_growth_2015 <- calculate_overall_growth(
  scope_detailed_long,
  group_var = "study_scope_detailed",
  start_year = 2015
)

scope_growth_compare <- scope_growth_2005 %>%
  rename(
    growth_2005 = annual_growth,
    SE_2005 = SE
  ) %>%
  left_join(
    scope_growth_2015 %>%
      select(study_scope_detailed, annual_growth, SE) %>%
      rename(
        growth_2015 = annual_growth,
        SE_2015 = SE
      ),
    by = "study_scope_detailed"
  )

scope_growth_compare <- scope_growth_compare %>%
  mutate(
    growth_change = growth_2015 - growth_2005
  )

scope_period <- calculate_period_growth(
  scope_detailed_long,
  group_var = "study_scope_detailed"
)

scope_lookup <- tibble(
  study_scope = c(
    rep("population_ecology", 7),
    rep("social_behavioural_ecology", 8),
    rep("spatial_ecology_habitat", 5),
    rep("species_interactions", 6),
    rep("community_ecology_biodiversity", 4),
    rep("human_wildlife_disturbance", 3)
  ),
  study_scope_detailed = c(
    "abundance", "occupancy", "density", "species_distribution",
    "detectability", "demographic_rate", "population_growth_rate",
    "social_dynamics", "group_size", "group_composition", "age_structure",
    "sex_ratio", "behavioral_response_to_factor", "diel_activity",
    "behavioral_state_classification",
    "space_use_extent", "habitat_landscape_use", "dispersal",
    "connectivity", "within_species_spatial_overlap",
    "predator_prey_interaction", "interspecific_competition", "scavenging",
    "herbivory", "seed_dispersal", "interspecific_temporal_spatial_avoidance",
    "species_richness", "diversity_or_evenness", "community_composition",
    "trophic_or_functional_structure",
    "human_wildlife_overlap", "anthropogenic_disturbance_effect",
    "direct_human_pressure"
  )
)

scope_period <- scope_period %>%
  left_join(scope_lookup, by = "study_scope_detailed") %>%
  select(study_scope, everything())

write.xlsx(scope_period, "scope_detailed_growth_05092026.xlsx")

scope_period2 <- calculate_period_growth(
  scope_long,
  group_var = "study_scope"
)

write.xlsx(scope_period2, "scope_growth_05092026.xlsx")


#
##
### 6. Count of NAs ----

metadata_na <- read.csv("data/20260409_abstract_metadata.csv")
glimpse(metadata_na)

## Create table

metadata_na <- metadata_na %>%
  mutate(
    statistical_method = case_when(
      !is.na(statistical_method_listed) & !is.na(statistical_method_other) ~
        paste(statistical_method_listed, statistical_method_other, sep = ";"),
      !is.na(statistical_method_listed) ~ statistical_method_listed,
      !is.na(statistical_method_other) ~ statistical_method_other,
      TRUE ~ NA_character_
    ),
    study_scope = if_else(
      rowSums(across(25:57), na.rm = TRUE) > 0,
      1L, 0L
    )
  ) %>%
  select(
    -(1:7),
    -(25:57),
    -country,
    -country_imputed,
    -biome_classified,
    -biome_imputed,
    -camera_main,
    -focal_species_emphasis,
    -focal_species_scientific_name,
    -statistical_method_listed,
    -statistical_method_other,
    -extraction_confidence
  )

na_table <- metadata_na %>%
  summarise(across(
    everything(),
    list(
      n_NA = ~sum(is.na(.)),
      pct_NA = ~mean(is.na(.)) * 100
    )
  )) %>%
  pivot_longer(
    everything(),
    names_to = c("variable", ".value"),
    names_pattern = "(.+)_(n_NA|pct_NA)"
  ) %>%
  mutate(pct_reported = 100 - pct_NA) %>%
  arrange(desc(pct_NA))

## Group variables

category_map <- c(
  trap_night = "Sampling design",
  deployment_number = "Sampling design",
  site_number = "Sampling design",
  temporal_scale_month = "Sampling design",
  statistical_method = "Analytical method",
  biome_combined = "Geographic context",
  temporal_design = "Study design",
  scientific_name = "Focal species",
  country_combined = "Geographic context",
  species_count_minimum = "Focal species",
  spatial_scale = "Geographic context",
  taxonomy_scope = "Focal species",
  primary_taxonomic_group = "Focal species",
  sampling_method = "Sampling design",
  study_approach = "Study design",
  study_scope = "Study design"
)

na_table <- na_table %>%
  mutate(
    category = category_map[variable],
    .before = variable
  ) %>%
  arrange(category, variable)

## Prepare heatmap data

heatmap_data <- na_table %>%
  mutate(
    variable = recode(
      variable,
      trap_night = "Trap nights",
      deployment_number = "Number of deployments",
      site_number = "Number of sites",
      temporal_scale_month = "Sampling duration",
      statistical_method = "Statistical method",
      biome_combined = "Biome",
      temporal_design = "Temporal design",
      scientific_name = "Scientific names",
      country_combined = "Country",
      species_count_minimum = "Species count",
      spatial_scale = "Spatial scale",
      taxonomy_scope = "Taxonomic scope",
      primary_taxonomic_group = "Primary taxonomic group",
      sampling_method = "Sampling method",
      study_approach = "Study approach",
      study_scope = "Study scope"
    ),
    category = factor(
      category,
      levels = c(
        "Study design",
        "Focal species",
        "Geographic context",
        "Sampling design",
        "Analytical method"
      )
    )
  ) %>%
  arrange(category, desc(pct_reported)) %>%
  mutate(
    variable = factor(variable, levels = rev(variable))
  )

okabe_ito <- c(
  "Study design" = "#D55E00",
  "Focal species" = "#0072B2",
  "Geographic context" = "#009E73",
  "Sampling design" = "#E69F00",
  "Analytical method" = "#56B4E9"
)

heatmap_data <- heatmap_data %>%
  mutate(
    fill = scales::alpha(
      okabe_ito[as.character(category)],
      0.15 + 0.85 * pct_reported / 100
    )
  )

## Plot

ggplot(heatmap_data, aes(x = pct_reported, y = variable, fill = fill)) +
  geom_col(width = 0.7) +
  geom_text(
    aes(
      x = pct_reported + 2,
      label = sprintf("%.1f%%", pct_reported)
    ),
    hjust = 0,
    size = 3.3
  ) +
  scale_fill_identity() +
  facet_grid(
    category ~ .,
    scales = "free_y",
    space = "free_y",
    switch = "y",
    labeller = as_labeller(c(
      "Study design" = "Study\ndesign",
      "Focal species" = "Focal\nspecies",
      "Geographic context" = "Geographic\ncontext",
      "Sampling design" = "Sampling\ndesign",
      "Analytical method" = "Analytical\nmethod"
    ))
  ) +
  scale_x_continuous(
    limits = c(0, 110),
    breaks = seq(0, 100, 20),
    labels = seq(0, 100, 20),
    expand = c(0, 0)
  ) +
  labs(
    x = "Abstracts reporting information (%)",
    y = NULL
  ) +
  theme_classic(base_size = 9) +
  theme(
    axis.text.y = element_text(size = 11),
    axis.title.x = element_text(size = 11),
    axis.line.y = element_blank(),
    axis.ticks.y = element_blank(),
    strip.background = element_blank(),
    strip.placement = "outside",
    strip.text.y.left = element_text(
      angle = 0,
      face = "bold",
      hjust = 1
    ),
    panel.spacing = unit(0.7, "lines"),
    legend.position = "none"
  )

ggsave("output/NA_percentage.jpeg", width = 9, height = 8, dpi = 300)










