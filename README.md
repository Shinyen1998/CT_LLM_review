# CT_LLM_review

This repository contains the Python and R code, prompts, supporting files, and dataset associated with the paper:

> Global camera-trap research in transition: A standardized metadata database from an LLM-assisted systematic review

## Dataset

The standardised metadata on global camera-trap studies described in this article are provided as `2026_CT_review_metadata.csv`. The dataset contains metadata extracted from study abstracts, with each row representing an independent study record and each column representing an extracted variable. Definitions and classification levels for all variables are provided in Supporting Information II.

## LLM-assisted systematic review

The `Python_Project` contains the code used for the LLM-assisted systematic review workflow. It includes three independent scripts for:

* title screening;
* abstract screening; and
* abstract metadata extraction.

The project also includes a `prompts` folder containing the prompt templates used to guide the large language models. The `data` folder contains intermediate data files generated throughout the workflow, including `data/data_descriptor.csv`, which describes the purpose and structure of each dataset.

## R data processing and analysis

All metadata processing, cleaning, standardisation, and analysis were conducted using R. The `R_Project` contains the scripts used to process the extracted metadata, conduct technical validation, generate the final dataset, and perform the analyses presented in the article. A data_descriptor.csv describing the purpose and structure of each dataset can be found in the data folder.

## Repository structure

```text
global-camera-trap-literature-metadata/
│
├── 2026_CT_review_metadata.csv
│
├── Supporting_Information/
│   ├── Supporting Information I (reference screening and PRISMA items).docx
│   ├── Supporting Information II (abstract metadata variable definition).xlsx
│   └── Supporting Information III (LLM extraction accuracy).csv
│
├── Python_Project/
│   ├── 1_title_screening.py
│   ├── 2_abstract_screening.py
│   ├── 3_abstract_extraction.py
│   ├── prompts/
│   └── data/
│
└── R_Project/
    ├── 1_title_screening.R
    ├── 2_abstract_screening.R
    ├── 3_abstract_metadata_cleaning.R
    ├── 4_new_references_processing.R
    ├── 5_technical_validation.R
    ├── 6_data_overview.R
    ├── 7_data_analysis.R
    └── data/
```

## Reproducibility

The repository provides the dataset, code, prompt templates, and supporting documentation needed to understand and reproduce the data-processing workflow described in the article. The archived version of this repository associated with the published study is available through Zenodo [TO BE UPDATED].
