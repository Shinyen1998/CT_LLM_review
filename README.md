This repository stores the Python, R codes, and dataset used related to this paper: Global camera-trap research in transition: A standardized metadata database from an LLM-assisted systematic review.

The standardised metadata on camera-trap studies described in this article have been deposited in a GitHub repository “global-camera-trap-literature-metadata” as a CSV file named “2026_CT_review_metadata”. The file contains the extracted metadata from study abstracts, with each row representing an independent study record and each column representing an extracted variable. Definitions and level descriptions for all variables are provided in Supporting Information II.

The Python project used for the LLM-assisted systematic review workflow has been deposited in the same GitHub repository. It includes three independent scripts containing the code used for title screening, abstract screening, and abstract extraction, as well as a “prompts” folder containing the prompt templates used by the scripts to guide large language models. The “data” folder within the Python project contains the intermediate data files generated throughout the workflow, including a CSV file named “data_descriptor” describing the purpose and structure of each dataset.

All metadata processing, cleaning, standardisation, and analysis were conducted using R. The corresponding R project, including scripts used to generate the final metadata dataset, is available in the same GitHub repository. The organization of the GitHub repository and associated files is shown below:

global-camera-trap-literature-metadata/
│
├── 2026_CT_review_metadata.csv
│
├── Supporting_Information/
│   ├── Supporting Information I (reference screening and PRISMA items).docx
│   ├── Supporting Information II (abstract metadata variable definition).xlsx
│   ├── Supporting Information III (LLM extraction accuracy).csv
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
    ├── 4_new_references_proccessing.R
    ├── 5_technical_validation.R
    ├── 6_data_overview.R
    ├── 7_data_analysis.R
    └── data/

The camera-trap literature metadata dataset described in this article is available from GitHub [URL]. The repository contains the final standardised metadata dataset “2026_CT_review_metadata.csv” and associated documentation.

