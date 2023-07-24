# Functions to process scraped data

library(tidyverse)


clean_categories <- function(data) {
  data <- data %>%
    mutate(category_de = ifelse(is.na(category_de),
                                "Sonstige Angriffe",
                                category_de))
}


#' Split multiple categories into single elements
#'
#' Function to split categories with multiple elements into separate instances
#'
#' @param data Dataset with category_de with possible multiple categories
#'
#' @return Dataset with separate row for each unique category
#' @export
split_categories <- function(data) {
  max_cat <- max(str_count(data$category_de, "& "), na.rm = T) + 1
  vars <- NULL
  for (i in 1:max_cat) {
    vars <- c(vars, paste0("var_", i))
  }
  
  data <- data %>%
    separate(category_de, vars, sep = " & ") %>%
    gather(helper, category_de, all_of(vars), na.rm = T) %>%
    select(-helper)
  
  return(data)
}


#' Function to clean source column
#'
#' @param data Dataset with raw source column
#'
#' @return Dataset with cleaned and standardized sources
#' @export
#'
#' @examples
clean_sources <- function(data) {
  data <- data %>%
    mutate(
      source = str_remove(source, "Quelle:|Quellen:"),
      source = str_remove(source, ",$|, $"),
      source = trimws(source),
      source = tolower(source),
      # fix broken html strings
      source = str_replace_all(source, ", -", ",-"),
      source = str_replace_all(source, ", \\d", ",\\d"),
      # fix empty htmls
      source = str_replace_all(source, "http[s]?://[:]? ", ""),
      # fix specific mistakes
      source = str_remove(
        source,
        "p_\\{_margin-bottom:_0.1in;_line-height:_120%;_\\}a:link_\\{__\\}  "
      ),
      source = str_replace_all(source, "http://freie presse", "freie presse"),
      source = str_replace_all(source, "http://unbekannte", "unbekannte"),
      source = str_replace_all(
        source,
        "http://strafermittlungsverfahren",
        "strafermittlungsverfahren"
      ),
      source = str_replace_all(source, ", camp120.html", ",camp120.html")
    )
}


#' Function to extract unique sources
#'
#' This function extracts the number of unique sources from the list of sources in the source column
#'
#' @param data Dataset with the source column, which may list multiple sources
#'
#' @return Dataset with separate columns for each unique source
#' @export
extract_sources <- function(data) {
  pattern <-
    "http[s]?://(?:[a-zA-Z]|[0-9]|[$-_@.&+#~|]|[!*\\(\\),]|(?:%[0-9a-fA-F][0-9a-fA-F]))+"
  
  # save urls
  urls <- str_extract_all(data$source, pattern, simplify = T)
  urls <- as_tibble(urls)
  names(urls) <- paste0("source_url", 1:length(urls))
  
  
  # save leftover text
  data <- data %>%
    mutate(
      source_text = source,
      source_text = str_remove_all(source_text, pattern),
      source_text = trimws(source_text),
      source_text = str_remove_all(source_text, "^und$|^,$|^;$|^ $|^.$|^\\u0085$|"),
      source_text = str_replace_all(source_text, " ", "_")
    ) %>%
    bind_cols(urls) %>%
    mutate_at(vars(starts_with("source")), function(x)
      ifelse(x == "", NA, x))
  
  return(data)
  
}


#' Function to identify sources that came from government
#'
#' @param data Dataset which includes columns for each unique source "source_"
#'
#' @return Dataset which lists number of sources, number of official sources, whether any source is official, and whether all sources are official.
#' @export
official_sources <- function(data) {
  pattern <-
    "bundestag|bundesregierung|linksfraktion|bundeskriminalamt|kleineanfrage|hakan-tas|martinarenner|landtag"
  
  # select source columns
  vars <- names(data)[grep("source_", names(data))]
  
  tmp <- data.frame(matrix(nrow = nrow(data), ncol = length(vars)))
  names(tmp) <- vars
  for (v in vars) {
    tmp[[v]] = str_detect(data[[v]], pattern)
  }
  
  tmp <- tmp %>%
    mutate(
      source_nr = rowSums(!is.na(.[vars])),
      source_off_nr = rowSums(.[vars], na.rm = T),
      source_off_any = ifelse(source_off_nr > 0, TRUE, FALSE),
      source_off_all = ifelse((source_off_nr > 0) &
                                (source_nr == source_off_nr), TRUE, FALSE)
    ) %>%
    select(source_nr, source_off_nr, source_off_any, source_off_all)
  
  data <- bind_cols(data, tmp)
  
  return(data)
}


#' Executes all processing steps
process_scraped_data <- function(config) {
  path <- config$data_config$attacks$input[2]
  files <- list.files(path, pattern = "*.csv", full.names = T)
  # only extract specified years
  files <- files[str_extract(files, "\\d+") %in%
                   config$data_config$attacks$years]
  
  # load and bind data
  data <- tibble()
  for (f in files) {
    temp <-  read_csv(f, show_col_types = FALSE)
    data <- bind_rows(data, temp)
  }
  
  # process data
  data <- data %>%
    clean_categories() %>%
    split_categories() %>%
    clean_sources() %>%
    extract_sources() %>%
    official_sources() %>%
    select(-starts_with("source_url"),-starts_with("source_text"))
  
  data <- data %>%
    mutate(
      source_type =
        case_when(
          # unofficial sources
          source_off_all == FALSE ~ "other",
          # official sources prior to 2016 (have to have taken place at shelters)
          (date < as.Date('2016-01-01') &
             source_off_all ==  TRUE) ~ "official_shelter",
          # official sources after 2016 (could have taken place at or outside of shelters)
          (date >= as.Date('2016-01-01') &
             source_off_all ==  TRUE) ~ "official_unclear",
          TRUE ~ "NA"
        ),
      data_type = "scraped"
    )
  
  return(data)
}
