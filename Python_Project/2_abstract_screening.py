"""
===========================================================
LLM ABSTRACT SCREENING
===========================================================
"""

import pandas as pd
import json
import time
from tqdm import tqdm
from openai import OpenAI

# ============================================================
# CONFIG
# ============================================================

INPUT_FILE = "data/20260407_reference_abstract_full_new.csv"
OUTPUT_FILE = "data/20260407_abstract_screened_full_new.csv"
PARTIAL_FILE = "data/20260407_abstract_screen_partial.csv"

PROMPT_FILE = r"C:\Users\user\Dropbox\ECL Shinyen Chiu\Shinyen WildObs\Camtrap methods systematic reviews\LLM review PythonProject\prompts\2_abstract_screening_system_prompt.txt"

MODEL = "gpt-4o-2024-11-20"
REQUEST_SLEEP = 0.2
MAX_RETRIES = 2

# ============================================================
# INIT
# ============================================================

client = OpenAI()

# ============================================================
# LOAD SYSTEM PROMPT
# ============================================================

with open(PROMPT_FILE, "r", encoding="utf-8") as f:
    system_prompt = f.read().strip()

# ============================================================
# LOAD DATA
# ============================================================

refs = pd.read_csv(INPUT_FILE)
print(f"Loaded {len(refs)} records")

# Ensure abstract column exists
if "abstract" not in refs.columns:
    raise ValueError("Column 'abstract' not found in input CSV")

# ============================================================
# OUTPUT COLUMNS
# ============================================================

refs["abstract_decision"] = pd.NA
refs["confidence"] = pd.NA
refs["abstract_reason_tag"] = pd.NA

# ============================================================
# STRICT JSON SCHEMA (LOW TOKEN + HIGH RELIABILITY)
# ============================================================

response_schema = {
    "type": "json_schema",
    "json_schema": {
        "name": "abstract_screening",
        "strict": True,
        "schema": {
            "type": "object",
            "properties": {
                "abstract_decision": {
                    "type": "string",
                    "enum": ["PASS", "FAIL", "UNCERTAIN"]
                },
                "confidence": {
                    "type": "integer",
                    "minimum": 0,
                    "maximum": 100
                },
                "abstract_reason_tag": {
                    "type": ["string", "null"],
                    "enum": [
                        "study_type",
                        "target_organism",
                        "ecological_scope",
                        "sampling_method",
                        None
                    ]
                }
            },
            "required": ["abstract_decision", "confidence", "abstract_reason_tag"],
            "additionalProperties": False
        }
    }
}

# ============================================================
# SCREENING LOOP
# ============================================================

for idx, row in tqdm(refs.iterrows(), total=len(refs), desc="Screening"):

    abstract = str(row["abstract"]).strip()

    # Skip empty abstracts
    if not abstract or abstract.lower() == "nan":
        refs.at[idx, "abstract_decision"] = "UNCERTAIN"
        refs.at[idx, "confidence"] = 0
        refs.at[idx, "abstract_reason_tag"] = None
        continue

    user_prompt = f"""
Abstract:
{abstract}

Classify.
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

            result = json.loads(response.choices[0].message.content)

            refs.at[idx, "abstract_decision"] = result["abstract_decision"]
            refs.at[idx, "confidence"] = result["confidence"]
            refs.at[idx, "abstract_reason_tag"] = result["abstract_reason_tag"]

            success = True
            break

        except Exception as e:
            print(f"[Retry {attempt+1}] Row {idx} failed: {e}")
            time.sleep(1)

    if not success:
        refs.at[idx, "abstract_decision"] = "ERROR"
        refs.at[idx, "confidence"] = pd.NA
        refs.at[idx, "abstract_reason_tag"] = pd.NA

    time.sleep(REQUEST_SLEEP)

    # ========================================================
    # CHECKPOINT
    # ========================================================

    if (idx + 1) % 10 == 0:
        refs.to_csv(PARTIAL_FILE, index=False)
        print(f"Checkpoint saved at row {idx+1}")

# ============================================================
# SAVE FINAL
# ============================================================

refs.to_csv(OUTPUT_FILE, index=False)

print("✅ Screening complete.")
print(f"Saved to: {OUTPUT_FILE}")