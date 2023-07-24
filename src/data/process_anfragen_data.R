# Functions to process ocr data

library(tidyverse)
library(ggmap)
library(sf)



#' Extract and clean data
#'
#' Function to extract and clean data obtained from OCR of PDF files
#'
#' @param data csv containing data extracted from pdf tables
#'
#' @return Returns cleaned dataset
#' @export
clean_extracted_tables <- function(data) {
  names(data) <- names(data) %>%
    tolower() %>%
    stringr::str_remove_all(., '\\.|-|\r|­| ') %>%
    stringr::str_replace(., 'pmkrechts', 'pmk') %>%
    stringr::str_remove_all(., '\\.')
  
  data <- data %>%
    filter(nr != 'Nr.') %>%
    mutate(nr = as.numeric(nr),
           datum = as.Date(datum, format = '%d.%m.%Y'),
    ) %>%
    rename(
      date = datum,
      location = ort,
      state = land,
      description = deliktsart
    )
  
  # check if all rows were extracted
  if ((max(data$nr) > nrow(data)) |
      (length(min(data$nr):max(data$nr)) > length(unique(data$nr)))) {
    # return row numbers that are missing
    missing <- setdiff(min(data$nr):max(data$nr), data$nr)
    print('Extraction did not recognize all rows. Returning missing rows instead')
    return(missing)
  } else {
    return(data)
  }
}


#' Get state names
#'
#' Function to rename state abbreviations to full state names
#'
#' @param data dataset with abbreviated state names
#'
#' @return dataset with full state names
#' @export
get_state_from_code <- function(data) {
  data <- data %>%
    mutate(
      state = case_when(
        state == 'BW' ~ 'Baden-Württemberg',
        state == 'BY' ~ 'Bayern',
        state %in% c('BE', 'BR') ~ 'Berlin',
        state == 'BB' ~ 'Brandenburg',
        state == 'HB' ~ 'Bremen',
        state == 'HH' ~ 'Hamburg',
        state == 'HE' ~ 'Hessen',
        state == 'MV' ~ 'Mecklenburg-Vorpommern',
        state == 'NI' ~ 'Niedersachsen',
        state == 'NW' ~ 'Nordrhein-Westfalen',
        state == 'RP' ~ 'Rheinland-Pfalz',
        state == 'SL' ~ 'Saarland',
        state == 'SN' ~ 'Sachsen',
        state %in% c('ST', 'AN') ~ 'Sachsen-Anhalt',
        state == 'SH' ~ 'Schleswig-Holstein',
        state == 'TH' ~ 'Thüringen',
        TRUE ~ state
      )
    )
  
  return(data)
}


#' Function to create standardized categories of attacks
#'
#' @param data
#'
#' @return data
#' @export
gen_categories <- function(data) {
  cat_misc <-
    c(
      'versammlung',
      'sachbeschädigung',
      'beleidigung',
      'beschimpfung',
      'verleumdung',
      'propaganda',
      'stgb243',
      'verunglimpfungdesstaates',
      'versuchderbeteiligung',
      'kennzeichenverfassungswidrigerorg',
      'volksverhetzung',
      'bedrohung',
      'störungdesöffentlichenfriedens',
      'störungdöffentlichenfriedens',
      'bildungkriminellervereinigungen',
      'gefährlicheeingriffeindenstraßenverkehr',
      'gefährlichereingriffindenstraßenverkehr',
      'landfriedensbruch',
      'diebstahl',
      'raub',
      'missbrauchvonnotrufen',
      'missbrauchvnotrufen',
      'öffentlicheaufforderungzustraftaten',
      'zerstörungvonbauwerken',
      'bildungterroristischervereinigungen',
      'bodenverunreinigung',
      'hausfriedensbruch',
      'urkundenfälschung',
      'briefgeheimnis',
      'verunglimpfungdesandenkensverstorbener',
      'amtsanmaßung',
      'anleitungzustraftaten',
      'üblenachrede',
      "fälschung",
      "verletzungdervertraulichkeitdeswortes",
      "gewaltdarstellung",
      "verfassungsfeindlicheverunglimpfung",
      "gesetzzumschutzderjugendinderöffentlichkeit",
      "belohnungundbilligungvonstraftaten"
    )
  
  cat_assault <-
    c(
      'körperverletzung',
      'mord',
      'totschlag',
      'waffen',
      'waffg',
      "kwkg",
      'nötigung',
      'erpressung',
      'widerstand'
    )
  
  cat_arson <- c('explosion', 'sprengstoff', 'brand', 'spreng')
  
  
  data <- data %>%
    mutate(
      category_de = tolower(description),
      category_de = str_remove_all(category_de, ' |­\r|\r|- |-|\\.')
    )
  
  data <- data %>%
    mutate(
      category_de = case_when(
        str_detect(category_de, paste(cat_misc, collapse = '|')) ~ 'Sonstige Angriffe',
        str_detect(category_de, paste(cat_assault, collapse = '|')) ~ 'Tätlicher Übergriff/Körperverletzung',
        str_detect(category_de, paste(cat_arson, collapse = '|')) ~ 'Brandanschlag',
        TRUE ~ 'NA'
      )
    )
  
  return(data)
}


#' Executes all processing steps
process_anfragen_data <- function(config) {
  path <- config$data_config$attacks$input[1]
  files <- list.files(path, pattern = "*.csv", full.names = T)
  
  # load data
  data <- tibble()
  for (f in files) {
    # specify source type of attack
    if (grepl('noshelter', f)) {
      source_type <- 'official_noshelter'
    } else {
      source_type <- 'official_shelter'
    }
    
    # separately load retrospectively added cases for october -- december
    if (grepl('nachtrag', f)) {
      temp_nachtrag <-  read_csv(f, show_col_types = FALSE) %>%
        clean_extracted_tables() %>%
        mutate(source_type = source_type,
               month = month(date))
      data <- bind_rows(data, temp_nachtrag)
    } else {
      # load yearly data, but remove months october -- december
      temp <-  read_csv(f, show_col_types = FALSE) %>%
        clean_extracted_tables() %>%
        mutate(source_type = source_type,
               month = month(date)) %>%
        filter(!month %in% c(10, 11, 12))
      data <- bind_rows(data, temp)
    }
  }
  
  # process
  data <- data %>%
    get_state_from_code() %>%
    gen_categories()
  
  # add source and data information
  data <- data %>%
    mutate(
      source = 'bundestag',
      source_nr = 1,
      source_off_nr = 1,
      source_off_any = TRUE,
      source_off_all = TRUE,
      data_type = "kleineanfragen"
    )
  
  # # select columns
  data <- data %>%
    select(-pmk,-nr)
  
  return(data)
}
