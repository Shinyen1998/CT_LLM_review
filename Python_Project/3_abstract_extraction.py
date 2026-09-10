"""
===========================================================
LLM STRUCTURED ABSTRACT METADATA EXTRACTION (OPTIMIZED)
===========================================================
"""

import pandas as pd
import json
import time
from tqdm import tqdm
from openai import OpenAI
import os

client = OpenAI()

# ============================================================
# FILE PATHS
# ============================================================

INPUT_FILE = "data/20260408_reference_abstract_missing.csv"
OUTPUT_FILE = "data/20260408_abstract_screened_full_missing.csv"
PARTIAL_FILE = "data/20260408_abstract_screen_partial.csv"

MODEL = "gpt-4o-2024-11-20"
REQUEST_SLEEP = 0.2
MAX_RETRIES = 2

# ============================================================
# LOAD DATA
# ============================================================

PROMPT_FILE = r"C:\Users\uqschiu2\Dropbox\ECL Shinyen Chiu\Shinyen WildObs\camtrap methods systematic review\LLM review PythonProject\prompts\3_abstract_extraction_system_prompt.txt"

with open(PROMPT_FILE, "r", encoding="utf-8") as f:
    system_prompt = f.read()

refs = pd.read_csv(INPUT_FILE)
print(f"Loaded {len(refs)} rows")

# ============================================================
# DEFINE SCHEMA
# ============================================================

metadata_columns = [
    "country","country_imputed","biome_classified","biome_imputed",
    "spatial_scale","deployment_number","site_number",
    "trap_night","temporal_scale_month",
    "taxonomy_scope","primary_taxonomic_group",
    "scientific_name","species_count_minimum",
    "focal_species_emphasis","focal_species_scientific_name",
    "abundance","occupancy","density","species_distribution",
    "detectability","demographic_rate","population_growth_rate",
    "social_dynamics","group_size","group_composition","age_structure",
    "sex_ratio","space_use_extent","habitat_landscape_use","dispersal",
    "connectivity","within_species_spatial_overlap",
    "behavioral_response_to_factor","diel_activity",
    "behavioral_state_classification","predator_prey_interaction",
    "interspecific_competition","scavenging","herbivory",
    "seed_dispersal","interspecific_temporal_spatial_avoidance",
    "species_richness","diversity_or_evenness",
    "community_composition","trophic_or_functional_structure",
    "human_wildlife_overlap","anthropogenic_disturbance_effect",
    "direct_human_pressure",
    "sampling_method","camera_main", "statistical_method_listed",
    "statistical_method_other", "study_approach","temporal_design",
    "extraction_confidence"
]

for col in metadata_columns:
    refs[col] = pd.NA
    refs[col] = refs[col].astype("object")

# ============================================================
# ✅ STRICT JSON SCHEMA (TOKEN EFFICIENT)
# ============================================================

response_schema = {
    "type": "json_schema",
    "json_schema": {
        "name": "metadata_extraction",
        "strict": True,
        "schema": {
            "type": "object",
            "properties": {
                col: {"type": ["string", "null"]} for col in metadata_columns
            },
            "required": metadata_columns,
            "additionalProperties": False
        }
    }
}

# ============================================================
# EXTRACTION LOOP
# ============================================================

for idx, row in tqdm(refs.iterrows(), total=len(refs)):

    abstract = str(row.get("abstract", ""))

    user_prompt = f"""
Abstract:
{abstract}

Extract metadata.
"""

    success = False

    for attempt in range(MAX_RETRIES):

        try:
            response = client.chat.completions.create(
                model=MODEL,
                temperature=0,
                timeout=60,
                response_format=response_schema,
                messages=[
                    {"role": "system", "content": system_prompt},
                    {"role": "user", "content": user_prompt}
                ]
            )

            content = response.choices[0].message.content

            # ✅ Structured output — safe parse
            extracted = json.loads(content)

            # --------------------------------------------------
            # ASSIGN VALUES
            # --------------------------------------------------

            for col in metadata_columns:
                value = extracted.get(col, "NA")

                if value == "":
                    value = "NA"

                refs.at[idx, col] = value

            # --------------------------------------------------
            # LOGICAL CONSISTENCY
            # --------------------------------------------------

            if str(refs.at[idx, "focal_species_emphasis"]) != "1":
                refs.at[idx, "focal_species_scientific_name"] = "NA"

            # Ensure confidence numeric
            try:
                refs.at[idx, "extraction_confidence"] = int(
                    extracted.get("extraction_confidence", 0)
                )
            except:
                refs.at[idx, "extraction_confidence"] = "NA"

            success = True
            break

        except Exception as e:
            print(f"[Retry {attempt+1}] Row {idx} failed:", e)
            time.sleep(1)

    if not success:
        refs.at[idx, "extraction_confidence"] = "ERROR"

    time.sleep(REQUEST_SLEEP)

    # --------------------------------------------------
    # CHECKPOINT
    # --------------------------------------------------

    if (idx + 1) % 10 == 0:
        refs.to_csv(PARTIAL_FILE, index=False)
        print(f"Checkpoint saved at row {idx+1}")

# ============================================================
# SAVE FINAL
# ============================================================

refs.to_csv(OUTPUT_FILE, index=False)

print("✅ Extraction completed.")
print(f"Saved to: {OUTPUT_FILE}")