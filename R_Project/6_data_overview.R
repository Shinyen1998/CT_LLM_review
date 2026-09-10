
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

metadata <- read_csv("data/20260409_abstract_metadata.csv")

names(metadata)
glimpse(metadata)


#
##
### Data overview ----

## time ----

hist(metadata$published_year)
summary(metadata$published_year)

# create bar plot
ggplot(metadata, aes(x = published_year)) +
  geom_bar(fill = "#0072B2", color = NA) +
  labs(
    x = "Year",
    y = "Number of studies"
  ) +
  theme_minimal(base_size = 10) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 9),
    axis.text.y = element_text(size = 9),
    axis.title = element_text(size = 9),
    panel.grid = element_blank(),
    panel.border = element_blank(),
    axis.line = element_line(color = "grey30"),
    plot.background = element_rect(fill = "white", color = NA)
  )

ggsave("output/year.jpeg", width = 3, height = 4, dpi = 300)


## country ----

metadata <- metadata %>%
  mutate(
    country = na_if(trimws(country), ""),
    country_imputed = na_if(trimws(country_imputed), ""),
    country_combined = coalesce(country_imputed, country),
    .after = country_imputed
  )

sum(!is.na(metadata$country_combined))

country_combined_long <- metadata %>%
  separate_rows(country_combined, sep = ";")

country_count <- country_combined_long %>%
  filter(!is.na(country_combined)) %>%
  count(country_combined, sort = TRUE) %>%
  rename(frequency = n) %>%
  mutate(
    percentage = round(100 * frequency / sum(frequency), 1)
  )
country_count

# create map
country_distribution <- country_count %>%
  mutate(
    country_combined = case_when(
      country_combined == "DRC" ~ "Democratic Republic of the Congo",
      country_combined == "United States" ~ "USA",
      TRUE ~ country_combined
    )
  )

world_map <- map_data("world")

world_map_count <- world_map %>%
  left_join(
    country_distribution,
    by = c("region" = "country_combined")
  )

ggplot() +
  geom_polygon(
    data = world_map_count,
    aes(x = long, y = lat, group = group, fill = frequency),
    color = "white", linewidth = 0.2) +
  scale_fill_gradient(
    low = "#E6F6F1", high = "#006B4F", na.value = "grey90") +
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


## biome ----

metadata <- metadata %>%
  mutate(
    biome_classified = na_if(trimws(biome_classified), ""),
    biome_imputed = na_if(trimws(biome_imputed), ""),
    biome_combined = coalesce(biome_imputed, biome_classified),
    .after = biome_imputed
  )

sum(!is.na(metadata$biome_combined))

biome_combined_long <- metadata %>%
  separate_rows(biome_combined, sep = ";")

biome_count <- biome_combined_long %>%
  filter(!is.na(biome_combined)) %>%
  count(biome_combined, sort = TRUE) %>%
  rename(frequency = n) %>%
  mutate(
    percentage = round(100 * frequency / sum(frequency), 1)
  )
biome_count

# create pie chart
gradient <- okabe_ito[1:nrow(biome_count)]

biome_count <- biome_count %>%
  arrange(frequency) %>%
  mutate(color = gradient)

biome_count <- biome_count %>%
  arrange(desc(frequency)) %>%
  mutate(
    biome_combined = factor(
      biome_combined,
      levels = biome_combined
    )
  )

biome_labels <- c(
  "rainforest" = "Rainforest",
  "temperate_deciduous_forest" = "Temperate deciduous forest",
  "grassland" = "Grassland",
  "shrubland" = "Shrubland",
  "desert" = "Desert",
  "coniferous_forest" = "Coniferous forest",
  "tundra" = "Tundra"
)

ggplot(biome_count, aes(x = "", y = frequency, fill = biome_combined)) +
  geom_bar(stat = "identity", width = 1, color = "white") +
  coord_polar(theta = "y") +
  scale_fill_manual(
    values = setNames(biome_count$color, biome_count$biome_combined),
    labels = biome_labels
  ) +
  labs(fill = "Biome") +
  theme_void()

ggsave("output/biome.jpeg", width = 6, height = 3, dpi = 300)


## spatial scale ----

sum(!is.na(metadata$spatial_scale))

spatial_scale_count <- metadata %>%
  filter(!is.na(spatial_scale)) %>%
  count(spatial_scale, sort = TRUE) %>%
  rename(frequency = n) %>%
  mutate(
    percentage = round(100 * frequency / sum(frequency), 1)
  )
spatial_scale_count

# create bar plot
spatial_scale_count <- metadata %>%
  filter(!is.na(spatial_scale)) %>%
  mutate(
    spatial_scale = case_when(
      spatial_scale %in% c("national", "continental", "global") ~ "National/Continental/Global",
      TRUE ~ spatial_scale
    )
  ) %>%
  count(spatial_scale, sort = TRUE) %>%
  rename(frequency = n) %>%
  mutate(
    percentage = round(100 * frequency / sum(frequency), 1)
  ) %>%
  arrange(desc(frequency)) %>%
  mutate(
    spatial_scale = factor(spatial_scale, levels = spatial_scale)
  )

ggplot(spatial_scale_count, aes(x = spatial_scale, y = frequency)) +
  geom_col(fill = "#0072B2", color = NA) +
  scale_x_discrete(
    labels = function(x) str_replace_all(x, "/", "\n")
  ) +
  labs(
    x = "Spatial scale",
    y = "Number of studies"
  ) +
  theme_minimal(base_size = 10) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 9),
    axis.text.y = element_text(size = 9),
    axis.title = element_text(size = 9),
    panel.grid = element_blank(),
    panel.border = element_blank(),
    axis.line = element_line(color = "grey30"),
    plot.background = element_rect(fill = "white", color = NA)
  )

ggsave("output/spatical_scale.jpeg", width = 3, height = 3, dpi = 300)


## temporal scale (months) ----

sum(!is.na(metadata$temporal_scale_month))

summary(metadata$temporal_scale_month)
hist(metadata$temporal_scale_month)

# categorize the temporal scale
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

temporal_count <- metadata %>%
  filter(!is.na(temporal_scale_category)) %>%
  count(temporal_scale_category) %>%
  mutate(
    percentage = round(100 * n / sum(n), 1)
  )
temporal_count

# create bar plot
temporal_count <- temporal_count %>%
  mutate(
    temporal_scale_category = factor(
      temporal_scale_category,
      levels = c(
        "<1 year",
        "1–5 years",
        "5–10 years",
        "10–20 years",
        ">20 years"
      )
    )
  )

ggplot(temporal_count, aes(x = temporal_scale_category, y = n)) +
  geom_col(fill = "#0072B2", color = NA) +
  labs(
    x = "Sampling duration",
    y = "Number of studies"
  ) +
  theme_minimal(base_size = 10) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 9),
    axis.text.y = element_text(size = 9),
    axis.title = element_text(size = 9),
    panel.grid = element_blank(),
    panel.border = element_blank(),
    axis.line = element_line(color = "grey30"),
    plot.background = element_rect(fill = "white", color = NA)
  )

ggsave("output/temporal_scale.jpeg", width = 3, height = 3, dpi = 300)


## taxonomy scope ----

sum(!is.na(metadata$taxonomy_scope))

taxonomy_scope_count <- metadata %>%
  filter(!is.na(taxonomy_scope)) %>%
  count(taxonomy_scope, sort = TRUE) %>%
  rename(frequency = n) %>%
  mutate(
    percentage = round(100 * frequency / sum(frequency), 1)
  )
taxonomy_scope_count


## primary taxonomy group ----

sum(!is.na(metadata$primary_taxonomic_group))

primary_taxonomic_group_count <- metadata %>%
  filter(!is.na(primary_taxonomic_group)) %>%
  count(primary_taxonomic_group, sort = TRUE) %>%
  rename(frequency = n)%>%
  mutate(
    percentage = round(100 * frequency / sum(frequency), 1)
  )
primary_taxonomic_group_count


## species count minimum ----

sum(!is.na(metadata$species_count_minimum))

summary(metadata$species_count_minimum)
hist(log10(metadata$species_count_minimum))


## focal species ----

sum(!is.na(metadata$focal_species_emphasis))
table(metadata$focal_species_emphasis)

focal_species_scientific_name_long <- metadata %>%
  separate_rows(focal_species_scientific_name, sep = ";")

focal_species_scientific_name_count <- metadata %>%
  filter(!is.na(focal_species_scientific_name)) %>%
  count(focal_species_scientific_name, sort = TRUE) %>%
  rename(frequency = n) %>%
  mutate(percentage = round(100 * frequency / sum(frequency), 1))

focal_species_scientific_name_count

# create stacked bar plot
taxonomy_scope_taxa <- metadata %>%
  filter(
    !is.na(taxonomy_scope),
    !is.na(primary_taxonomic_group)
  ) %>%
  mutate(
    primary_taxonomic_group = case_when(
      primary_taxonomic_group %in% c("reptile", "amphibian") ~ "reptile/amphibian",
      TRUE ~ primary_taxonomic_group
    )
  ) %>%
  count(taxonomy_scope, primary_taxonomic_group) %>%
  rename(frequency = n)

taxonomy_scope_order <- taxonomy_scope_taxa %>%
  group_by(taxonomy_scope) %>%
  summarise(total = sum(frequency)) %>%
  arrange(desc(total)) %>%
  pull(taxonomy_scope)

taxonomy_scope_taxa <- taxonomy_scope_taxa %>%
  mutate(
    taxonomy_scope = factor(
      taxonomy_scope,
      levels = taxonomy_scope_order
    )
  )

# labels
taxonomy_group_labels <- c(
  "mammal" = "Mammal",
  "multi_taxa" = "Multi-taxa",
  "bird" = "Bird",
  "reptile/amphibian" = "Reptile/amphibian"
)

taxonomy_scope_labels <- c(
  "single_species" = "Single-\nspecies",
  "multi_species" = "Multi-\nspecies",
  "community" = "Community-\nlevel"
)

# Okabe-Ito colours
taxa_colors <- c(
  "mammal" = okabe_ito[1],
  "multi_taxa" = okabe_ito[2],
  "bird" = okabe_ito[3],
  "reptile/amphibian" = okabe_ito[4]
)

taxonomic_group_order <- taxonomy_scope_taxa %>%
  group_by(primary_taxonomic_group) %>%
  summarise(total = sum(frequency)) %>%
  arrange(desc(total)) %>%
  pull(primary_taxonomic_group)

taxonomy_scope_taxa <- taxonomy_scope_taxa %>%
  mutate(
    primary_taxonomic_group = factor(
      primary_taxonomic_group,
      levels = taxonomic_group_order
    )
  )

ggplot(
  taxonomy_scope_taxa,
  aes(x = taxonomy_scope, y = frequency, fill = primary_taxonomic_group)
) +
  geom_col(color = NA) +
  scale_fill_manual(
    values = taxa_colors,
    labels = taxonomy_group_labels
  ) +
  scale_x_discrete(labels = taxonomy_scope_labels) +
  labs(
    x = "Taxonomy scope",
    y = "Number of studies",
    fill = "Primary taxonomic group"
  ) +
  theme_minimal(base_size = 10) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 8),
    axis.text.y = element_text(size = 9),
    axis.title = element_text(size = 9),
    legend.title = element_text(size = 9),
    legend.text = element_text(size = 9),
    panel.grid = element_blank(),
    panel.border = element_blank(),
    axis.line = element_line(color = "grey30"),
    plot.background = element_rect(fill = "white", color = NA)
  )

ggsave("output/taxonomy.jpeg", width = 5, height = 4, dpi = 300)


## ecological scope (study objectives) ----

scope_cols <- metadata[, 25:57]

ecological_scope_count <- data.frame(
  ecological_scope = colnames(scope_cols),
  frequency = colSums(scope_cols == 1, na.rm = TRUE),
  row.names = NULL
) %>%
  arrange(desc(frequency)) %>%
  mutate(
    percentage = round(100 * frequency / sum(frequency), 1)
  )
ecological_scope_count

# regroup the categories
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

scope_cols2 <- metadata[, 66:71]

ecological_scope_count2 <- data.frame(
  ecological_scope2 = colnames(scope_cols2),
  frequency = colSums(scope_cols2 == 1, na.rm = TRUE),
  row.names = NULL
) %>%
  arrange(desc(frequency)) %>%
  mutate(
    percentage = round(100 * frequency / sum(frequency), 1)
  )
ecological_scope_count2

# create bar plot
ecological_scope_count2 <- ecological_scope_count2 %>%
  mutate(
    ecological_scope_label = case_when(
      ecological_scope2 == "spatial_ecology_habitat" ~ "Spatial ecology\nand habitat use",
      ecological_scope2 == "population_ecology" ~ "Population \necology",
      ecological_scope2 == "social_behavioural_ecology" ~ "Social and\nBehavioural ecology",
      ecological_scope2 == "human_wildlife_disturbance" ~ "Human–wildlife \ninteractions",
      ecological_scope2 == "species_interactions" ~ "Species\ninteractions",
      ecological_scope2 == "community_ecology_biodiversity" ~ "Community richness\nand diversity"
    )
  )

# preserve frequency order
ecological_scope_count2 <- ecological_scope_count2 %>%
  arrange(desc(frequency)) %>%
  mutate(
    ecological_scope_label = factor(
      ecological_scope_label,
      levels = ecological_scope_label
    )
  )

ggplot(ecological_scope_count2,
       aes(x = ecological_scope_label, y = frequency)) +
  geom_col(fill = "#0072B2", color = NA) +
  labs(
    x = "Study scope",
    y = "Number of studies"
  ) +
  theme_minimal(base_size = 10) +
  theme(
    axis.text.x = element_text(
      angle = 45,
      hjust = 1,
      size = 11
    ),
    axis.text.y = element_text(size = 12),
    axis.title = element_text(size = 12),
    panel.grid = element_blank(),
    panel.border = element_blank(),
    axis.line = element_line(color = "grey30"),
    plot.background = element_rect(fill = "white", color = NA)
  )

ggsave("output/objectives.jpeg", width = 5, height = 4, dpi = 300)




