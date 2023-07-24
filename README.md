# Reproductive material for "On the effect of the New Year’s Eve sexual assaults on anti-refugee violence"

### Summary 

This repository contains the material, data, and code needed to reproduce the results in "Frey, A. 2023. 'On the effect of the New Year's sexual assaults on anti-refugee violence: A rejoinder to NAME (2023)', *European Sociological Review*.". 

### Setup

#### Install packages
Run this code once to install all the necessary packages: 

```
install.packages(c("ggmap", "sf", "yaml", "lubridate", "forcats", "stringr", "dplyr", "purrr", "readr", "tidyr", "tibble", "ggplot2", "tidyverse", "zoo", "patchwork", "xtable"))
```

#### R version
```
R version 4.3.0 (2023-04-21)
Platform: aarch64-apple-darwin20 (64-bit)
Running under: macOS Monterey 12.2.1
```

#### Configuration

All code configurations are specified in `config.yaml`.

### Run


#### Generate data

To generate the dataset for the analysis, execute the following file: 

```
src/data/make_data.R
```

This will generate a clean dataset, and save it in `data/processed/district_attack_date.csv`


#### Reproduce analysis

To generate the models, open `src/analysis/analysis.do`, set your working directory at `"path-to-repo"` to the repository folder, and then run the following file: 

```
src/analysis/analysis.do
```

To generate the descriptive plots and tables, run: 

```
src/analysis/desc.R
```

To generate the Regression-Discontinuity-in-Time models, run: 

```
src/analysis/rdit.R
```

#### Folder structure
```
├── LICENSE
├── README.md
├── config.yaml
├── data
│   ├── interim
│   ├── processed
│   └── raw
│       ├── country
│       ├── district
│       ├── kleineanfragen
│       ├── scraped
│       └── shp
│           └── simplified
├── nye.Rproj
├── output
│   ├── plots
│   │   └── main
│   └── tables
├── src
│   ├── analysis
│   │   ├── analysis.do
│   │   ├── attaching_labels.do
│   │   ├── desc.R
│   │   └── rdit.R
│   └── data
│       ├── make_data.R
│       ├── process_anfragen_data.R
│       ├── process_attack_data.R
│       ├── process_district_data.R
│       ├── process_events_data.R
│       ├── process_scraped_data.R
│       └── process_shp_data.R
└── tmp
```