# Compiles functions to make processed datasets

library(tidyverse)
library(yaml)
library(sf)

source("src/data/process_anfragen_data.R")
source("src/data/process_scraped_data.R")
source("src/data/process_attack_data.R")
source("src/data/process_district_data.R")
source("src/data/process_shp_data.R")
source("src/data/process_events_data.R")

# suppress warnings
defaultW <- getOption("warn") 
options(warn = -1) 


make_data <- function(config) {
  
  # process shp data
  shp <- process_shp_data(config)
  st_write(shp, config$data_config$shp$output, append = FALSE)
  cat("Shp data saved at", config$data_config$shp$output, "\n")
  
  # process attacks data
  attacks <- bind_rows(process_anfragen_data(config),
                       process_scraped_data(config))
  
  # subtract nonshelter data from attacks
  attacks <- bind_rows(attacks, 
                       remove_noshelter_attacks(attacks))
  
  # georeference attacks
  attacks <- attacks %>%
    english_categories() %>%
    geocode_address(
      addresses_filepath = config$data_config$attacks$stored_addresses,
      api_key = Sys.getenv("google_api")
    ) %>%
    add_district_keys(shp = shp) %>%
    drop_demos_and_suspected_attacks()
  
  write_csv(attacks, file = config$data_config$attacks$output)
  cat("Combined attack data saved at",
      config$data_config$attacks$output,
      "\n")
  
  # process district data
  district <- process_district_data(config)
  
  # add geometry and distance measure to data
  district <- district %>%
    left_join(shp, by = "key") %>%
    select(-geometry)
  
  write_csv(district, file = config$data_config$district$output)
  cat("District data saved at",
      config$data_config$district$output,
      "\n")
  
  # process district_date data
  district_date <- process_district_date_data(district, config)
  write_csv(district_date, file = config$data_config$district_date$output)
  cat("District-date data saved at",
      config$data_config$district_date$output,
      "\n")
  
  # process district_date_attack data
  district_attack_date <- process_district_attack_date_data(attacks, 
                                                            district_date, 
                                                            config)
  
  # add events
  district_attack_date <- add_events(district_attack_date, config)
  
  # standardize variables
  district_attack_date_z <- district_attack_date %>%
    ungroup() %>% 
    select_if(is.numeric) %>% 
    sapply(function(x) (x - mean(x, na.rm = T)) / (2*sd(x, na.rm = T))) %>%
    as_tibble() %>%
    setNames(paste0('z_', names(.)))
  
  district_attack_date <- district_attack_date %>%
    bind_cols(district_attack_date_z)
  
  # save data
  write_csv(district_attack_date,
            file = config$data_config$district_attack_date$output)
  haven::write_dta(district_attack_date,
                   path = config$data_config$district_attack_date$output_stata)
  
  cat("District-date-attack data saved at",
    config$data_config$district_attack_date$output,
    "\n")
  return(district_attack_date)
}

# execute when sourcing script
config <- read_yaml("config.yaml")
data <- make_data(config)

# set warning to standard
options(warn = defaultW)
