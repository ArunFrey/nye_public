# Functions to process both attack datasets


#' Function to translate German categories
#'
#' @param data Dataset with German categories
#'
#' @return Dataset with english categories
#' @export
english_categories <- function(data) {
  data <- data %>%
    mutate(
      category_en = case_when(
        str_detect(category_de, "Brand") ~ "arson",
        str_detect(tolower(category_de), "sonstige") ~ "miscellaneous attack",
        str_detect(category_de, "Kundgebung") ~ "demonstration",
        str_detect(category_de, "Tätlicher") ~ "assault",
        str_detect(category_de, "Verdachtsfall") ~ "suspected attack",
        TRUE ~ as.character(NA)
      )
    )
  
  return(data)
}


#' Function to drop demonstration and suspected attacks from list
#'
#' @param data Data on all attacks, including demonstrations and suspected attacks
#'
#' @return data without demonstrations and suspected attacks
#' @export
drop_demos_and_suspected_attacks <- function(data) {
  data <- data %>%
    filter(!category_en %in% c("demonstration", "suspected attack"))
}


#' Geocode addresses
#'
#' Function to obtain latitude and longitude from address information
#'
#' @param addresses_filepath Filepath where geocoded addresses will be stored. If file exists, only those addresses that have not yet been geocoded will be added.
#' @param api_key Your Google Maps API key
#' @export
geocode_address <- function(data,
                            addresses_filepath,
                            api_key) {
  require(tidyverse)
  require(ggmap)
  
  register_google(key = api_key, write = FALSE)
  
  # get already geocoded addresses
  if (file.exists(addresses_filepath)) {
    address_done <- read_csv(addresses_filepath, show_col_types = FALSE)
  } else {
    address_done <- NULL
  }
  
  # only select those addresses not yet done
  addresses <- data %>%
    mutate(address = paste(location, state, "Deutschland", sep = ", ")) %>%
    group_by(address) %>%
    summarise_all(first) %>%
    select(address, location, state)
  
  address_notdone <- addresses %>%
    filter(!(address %in% address_done$address))
  
  # geocode leftover addresses
  if (nrow(address_notdone) == 0) {
    cat("All addresses geocoded")
  } else {
    cat(paste("Geocoding", nrow(address_notdone), "addresses...\n"))
    address_notdone <- address_notdone  %>%
      mutate_geocode(address)
    address_done <- bind_rows(address_done, address_notdone)
    write_csv(address_done, file = addresses_filepath)
  }
  
  data <- data %>%
    left_join(address_done, by = c("location", "state")) %>%
    select(-address)
  
  return(data)
}



#' Add district keys to attack data
#'
#' Function to add community and district keys to attack data
#'
#' @param data Data to add keys to
#' @param shp Shp file
#'
#' @return Data with keys
#' @export
add_district_keys <- function(data,
                              shp) {
  data <- st_as_sf(data, coords = c("lon", "lat"), crs = 4326)
  
  shp <- shp %>%
    select(key, geometry)
  
  data <- st_join(data, shp)
  
  data <- extract(data,
                  geometry,
                  into = c('lat', 'lon'),
                  '\\((.*),(.*)\\)',
                  conv = T)
  
  return(data)
}


#' Function to remove attacks away from shelters
#'
#' @param data Dataset with all attacks
#'
#' @return Dataset with attacks that were listed as "official_noshelter" excluded
#' @export
remove_noshelter_attacks <- function(data) {
  
  # select official attacks outside of shelter
  data_noshelt <- data %>%
    filter(source_type == "official_noshelter") 
  
  # select scraped data
  data_scraped <- data %>%
    filter(data_type == "scraped")
  
  # standardize location names
  data_noshelt <- data_noshelt %>%
    mutate(
      location2 = tolower(location),
      # join split words
      #location2 = str_remove_all(location2, "- |-\r"),
      # remove filler words
      location2 = str_remove_all(
        location2,
        " an der (.|\r)+| bei (.|\r)+| am (.|\r)+| in (.|\r)+| im (.|\r)+"
      ),
      location2 = str_replace_all(location2, " |­|-|\r", ""),
      # remove text in brackets
      location2 = str_remove_all(location2, "\\s*\\([^\\)]+\\)"),
      # remove text after /
      location2 = str_remove_all(location2, "/.+|,.+"),
    ) %>%
    arrange(date, location2, state) %>%
    select(date, location2, state) %>%
    group_by(date, location2, state) %>%
    mutate(attack_nr = row_number())
  
  data_scraped <- data_scraped %>%
    mutate(
      location2 = tolower(location),
      # join split words
      location2 = str_remove_all(location2, "- |-\r"),
      # remove filler words
      location2 = str_remove_all(
        location2,
        " an der (.|\r)+| bei (.|\r)+| am (.|\r)+| in (.|\r)+| im (.|\r)+"
      ),
      location2 = str_replace_all(location2, " |­|-|\r", ""),
      # remove text in brackets
      location2 = str_remove_all(location2, "\\s*\\([^\\)]+\\)"),
      # remove text after /
      location2 = str_remove_all(location2, "/.+|,.+"),
    )
  
  # manual correction
  data_scraped <- data_scraped %>%
    mutate(
      location2 = case_when(
        str_detect(location, ", Berlin$") ~ "berlin",
        str_detect(location, ", Chemnitz$") ~ "chemnitz",
        str_detect(location, ", Dresden$") ~ "dresden",
        str_detect(location, ", Neuruppin$") ~ "neuruppin",
        str_detect(location, ", Wolfenbüttel$") ~ "wolfenbüttel",
        str_detect(location, ", Erlangen$") ~ "erlangen",
        str_detect(location, ", Kellinghusen$") ~ "keilinghusen",
        location2 == "dessauroßlau" ~ "roßlau",
        location2 == "neustadtglew" ~ "neustadtglewe",
        location2 == "cotbus" ~ "cottbus",
        location2 == "badwünnenberg" ~ "wünnenberg",
        location2 == "cotbus" ~ "cottbus",
        location2 == "bernhaldswald" ~ "bernhardswald",
        location2 == "wittenberge" &
          state == "Sachsen-Anhalt" ~ "wittenberg",
        location2 == "badneustadt" ~ "badneustadtandersaale",
        location2 == "weilheim" ~ "weilheiminoberbayern",
        location2 == "radolfzell" ~ "radolfzellambodensee",
        location2 == "rotenburg" &
          state == "Schleswig-Holstein" ~ "flensburg",
        TRUE ~ location2
      ),
      state = case_when(
        location2 == "berlin" & state == "Brandenburg" ~ "Berlin",
        location2 == "halle" &
          state == "Sachsen" ~ "Sachsen-Anhalt",
        location2 == "köln" &
          state == "Hessen" ~ "Nordrhein-Westfalen",
        location2 == "norderstedt" &
          state == "Niedersachsen" ~ "Schleswig-Holstein",
        location2 == "lengede" &
          state == "Sachsen-Anhalt" ~ "Niedersachsen",
        location2 == "schwandorf" &
          state == "Brandenburg" ~ "Bayern",
        TRUE ~ state
      )
    )
  
  # Order data
  data_scraped <- data_scraped %>%
    arrange(date, location2, state) %>%
    group_by(date, location2, state) %>%
    mutate(attack_nr = row_number())
  
  # exact anti-join
  data_shelt <- anti_join(data_scraped,
                          data_noshelt,
                          by = c("date", "location2", "state", "attack_nr"))
  
  data_left <- anti_join(data_noshelt,
                         data_scraped,
                         by = c("date", "location2", "state", "attack_nr"))
  
  # check how many attack rows were deleted
  print(paste("Full data:", nrow(data_scraped)))
  print(paste("Attacks outside shelter:", nrow(data_noshelt)))
  print(paste("Attacks removed:", nrow(data_scraped) - nrow(data_shelt)))
  print(paste("Attacks not found:", round(nrow(data_left), 2)))
  print(paste("% of attacks not found:", nrow(data_left) / nrow(data_noshelt)))
  
  # change source_type
  data_shelt <- data_shelt %>%
    mutate(
      data_type = "scraped_shelt",
      source_type = ifelse(
        source_type == "official_unclear",
        "official_shelter",
        source_type
      )
    )
  
  return(data_shelt)
}



#' Function to count attacks per district date
count_attacks_per_district_date <- function(data) {
  
  # scraped attacks
  data_scraped <- data %>%
    filter(data_type == "scraped") %>%
    group_by(date, key) %>%
    count(name = "attacks_s")
  
  # scraped attacks, non-Governmental sources
  data_scraped_other <- data %>%
    filter(data_type == "scraped" & source_type == "other") %>%
    group_by(date, key) %>%
    count(name = "attacks_s_nogov")
  
  #  scraped attacks, excluding attacks outside of shelter
  data_scraped_shelt <- data %>%
    filter(data_type == "scraped_shelt") %>%
    group_by(date, key) %>%
    count(name = "attacks_s_shelt")
  

  data <- data_scraped %>%
    full_join(data_scraped_other) %>%
    full_join(data_scraped_shelt) 
  
  # add binary vars
  data <- data %>%
    ungroup() %>%
    mutate_at(vars(c(-date, -key)), list(bin = ~ ifelse(. > 0, 1, 0)))
  
  return(data)
}


gen_hostile_districts <- function(data,
                                  years,
                                  vars,
                                  cutoff) {
  data <- data %>%
    filter(year %in% years) %>%
    group_by(key) %>%
    summarise_at(all_of(vars), sum) %>%
    mutate_at(all_of(vars), function(x)
      factor(ifelse(x >= quantile(x, cutoff), 1, 0))) %>%
    rename_at(all_of(vars), function(x)
      paste0(x, "_p", as.integer(cutoff * 100)))
  
  return(data)
}


gen_diffusion_vars <- function(data,
                               diffusion_vars,
                               diffusion_lag_start,
                               diffusion_lag_end) {
  for (v in diffusion_vars) {
    # lag function
    fn <- paste(paste0(
      "lag(",
      v,
      ", ",
      (diffusion_lag_start:diffusion_lag_end),
      ")"
    ),
    collapse = " + ")
    
    # count of all attacks in prior X period
    var_all <- paste0('tdif_all_', v)
    
    data_all <- data %>%
      group_by(date) %>%
      summarise(!!v := eval(parse(text = paste0("sum(", v, ")")))) %>%
      mutate(!!var_all := eval(parse(text = fn))) %>%
      select(date, all_of(var_all))
    
    # count of attacks in same district in prior X period
    var_key <- paste0('tdif_key_', v)
    # count of all attacks in other districts in prior X period
    var_other <- paste0('tdif_other_', v)
    
    # generate variables
    data <- data %>%
      full_join(data_all, by = "date") %>%
      arrange(key, date) %>%
      group_by(key) %>%
      # generate var for previous attacks in district
      mutate(!!var_key := eval(parse(text = fn)),!!var_other := eval(parse(text = paste0(
        var_all, " - ", var_key
      )))) %>%
      select(-starts_with("tdif_all_"))
  }
  return(data)
}


process_district_attack_date_data <- function(attacks,
                                              district_date,
                                              config) {
  # count attacks per district-date
  attacks_nr <- count_attacks_per_district_date(attacks)
  # combine with district-date data
  attacks_district <- full_join(district_date, attacks_nr)
  # replace attacks with na values with 0s
  attacks_district[is.na(attacks_district)] <- 0
  
  # add attacks from original data
  attacks_paper <- read_csv('data/raw/scraped/arvig_original_paper.csv')
  attacks_district <- full_join(attacks_district, attacks_paper)
  
  # generate diffusion variables
  data <- gen_diffusion_vars(
    attacks_district,
    diffusion_vars = config$vars$dvs,
    diffusion_lag_start = config$data_config$district_attack_date$diffusion$diffusion_lag_start,
    diffusion_lag_end = config$data_config$district_attack_date$diffusion$diffusion_lag_end
  )
  
  # add hostile districts
  
  data <- left_join(
    data,
    gen_hostile_districts(
      data,
      years = config$data_config$district_attack_date$hostility$years,
      vars = config$vars$dvs,
      cutoff = config$data_config$district_attack_date$hostility$cutoff
    ),
    by = "key"
  )
  
  return(data)
}
