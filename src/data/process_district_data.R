# Functions to process district data
library(tidyverse)
library(yaml)
library(lubridate)


# Function to load data from disk
load_district_data <- function(file) {
  data <- read_csv2(
    paste0(file),
    na = c(".", "...", "-", "x"),
    col_types = cols(
      .default = "n",
      key = "c",
      kreis_type = "c",
      name = "c"
    ),
    show_col_types = FALSE
  )
  
  data <- data %>%
    mutate(key = case_when(key == "11" ~ "11000",
                           key %in% c("02", "2") ~ "02000",
                           TRUE ~ key))
  
  return(data)
}


# function to combine data into single dataframe
combine_district_data <- function(path) {
  data <- tibble(key = character())
  for (f in list.files(path)) {
    tmp <- load_district_data(paste0(path, f))
    if (f == "epw2014.csv") {
      tmp <- tmp %>%
        mutate(key = ifelse((nchar(key) < 5 &
                               key != 11), paste(0, key, sep = ""), key))
    }
    if ("year" %in% names(tmp)) {
      tmp <- tmp %>%
        filter(year == "2015") %>%
        select(-year)
    }
    
    data <- full_join(data, tmp, by = "key") %>%
      select(-starts_with("name"))
  }
  return(data)
}


# function to impute missing values at district level from available state data
impute_from_state_data <- function(data,
                                   vars_to_impute,
                                   districts_to_impute,
                                   state_to_impute_from,
                                   years_to_impute = NULL,
                                   regex = TRUE) {
  # optional selection of variables using regular expressions
  if (regex) {
    vars_to_impute <- grep(vars_to_impute,
                           names(data),
                           value = TRUE)
  }
  
  # if year is given, impute from state value in given year
  if (!is.null(years_to_impute)) {
    for (v in vars_to_impute) {
      for (k in districts_to_impute) {
        for (y in years_to_impute) {
          share_pop <-
            unique(data$pop[data$key == k] / data$pop[data$key == state_to_impute_from])
          data[(data$key == k) & (data$year == y), v] <-
            data[(data$key == state_to_impute_from) &
                   (data$year == y), v] * share_pop
        }
      }
    }
    # else impute from state value
  } else {
    for (v in vars_to_impute) {
      for (k in districts_to_impute) {
        share_pop <-
          unique(data$pop[data$key == k] / data$pop[data$key == state_to_impute_from])
        data[data$key == k, v] <-
          data[data$key == state_to_impute_from, v] * share_pop
      }
    }
  }
  return(data)
}


# function to merge values from two districts into one
merge_districts <- function(data, 
                            districts_to_group, 
                            by_year = FALSE) {
    for (g in districts_to_group) {
      data <- data %>%
        mutate(key = case_when(key == g[1] ~ g[2],
                               TRUE ~ key))
    }
    
    # if year is given, summarise by district-year
    if (by_year) {
      data <- data %>%
        group_by(key, year)
      # else summarise by district only
    } else {
      data <- data %>%
        group_by(key)
    }
    data <- data %>%
      summarise_each(funs(if (is.numeric(.))
        sum(., na.rm = TRUE)
        else
          last(.)))
    
    return(data)
  }


# function to delete old districts
delete_old_districts <- function(data, districts_to_drop) {
  data <- data %>%
    filter(!(key %in% districts_to_drop))
  
  return(data)
}


# function to restrict data to districts only
restrict_to_districts <- function(data) {
  data <- data %>%
    filter(nchar(key) == 5)
}


# function to execute all processing steps of district data
process_district_data <- function(config) {
  # process data
  data <-
    combine_district_data(path = config$data_config$district$input) %>%
    impute_from_state_data(
      vars_to_impute = config$data_config$district$impute$vars,
      districts_to_impute = config$data_config$district$impute$districts,
      state_to_impute_from = config$data_config$district$impute$state
    ) %>%
    merge_districts(config$data_config$district$districts_to_group) %>%
    delete_old_districts(config$data_config$district$districts_to_drop) %>%
    restrict_to_districts()
  
  # generate variables
  data <- data %>%
    mutate(
      east_bin = ifelse(as.numeric(substr(key, 1, 2)) >= 11, TRUE, FALSE),
      unemp_r_n = (unemp - unemp_f) / pop * 100,
      nonEU_p = (for_notEU / pop) * 100,
      turnout14 = (wähler_14 / wahlberechtigte) * 100,
      npd14 = (npd_14 / gültige_14) * 100,
      afd14 = (afd_14 / gültige_14) * 100,
      logpop = log(pop / 1000),
      city = case_when(
        kreis_type %in% c("KfS", "SK") ~ 1,
        kreis_type %in% c("K", "RV", "LK") ~ 0
      ),
      bip_pp = log(bip_1000s / pop),
      mf_ratio = pop_m / pop_f,
      hom_r = hom / (pop / 100000)
    )
  
  data <- data %>%
    select(
      key,
      east_bin,
      unemp_r_n,
      nonEU_p,
      turnout14,
      npd14,
      afd14,
      pop,
      logpop,
      city,
      bip_pp,
      mf_ratio,
      hom_r
    )
  
  return(data)
}


# function to expand district data by date
expand_by_date <- function(data,
                           by_year = TRUE,
                           date_start = NULL,
                           date_end = NULL) {
  # if year is given, pull date from min and max year directly
    if (by_year) {
      dates <- data %>%
        group_by(key, year) %>%
        transmute(date = list(seq(
          as.Date(paste0(min(year), "-01-01")),
          as.Date(paste0(max(year), "-12-31")), by = "days"
        ))) %>%
        unnest(date)
  # else use date_start and date_end to specify date range
    } else {
      dates <- data %>%
        group_by(key) %>%
        transmute(date = list(seq(
          as.Date(date_start), as.Date(date_end), by = "days"
        ))) %>%
        unnest(date)
    }
    
    dates <- dates %>%
      mutate(year = year(date),
             month = month(date),
             week = week(date),
             day = wday(date, label = TRUE),
             count_day = as.numeric(date - as.Date('2013-12-31')))
    
    if (by_year) {
      data <- left_join(dates, data, by = c("key", "year"))
    } else {
      data <- left_join(dates, data, by = c("key"))
    }
    return(data)
  }


# function to linearly interpolate data 
interpolate <- function(data, var, group = NULL) {
  if (!is.null(group)) {
    data <- data %>%
      group_by(get(group))
  }
  data <- data %>%
    arrange(date) %>%
    mutate(!!var := zoo::na.approx(get(var), na.rm = FALSE))
  
  return(data)
}


# function to generate interpolated district refugee data
gen_district_date_refugee_data <- function(config) {
  
  # variable to interpolate by date
  var <- config$data_config$district_date$interpolate$district$var
    
  # load population data to distribute refugees by district population share
    pop <-
      load_district_data(paste0(
        config$data_config$district$input, 
        'population.csv'
        )) %>%
      group_by(key, pop) %>%
      expand(year = 2013:2016)
    
    data <- load_district_data(
      config$data_config$district_date$interpolate$district$input)
    
    data <- left_join(pop, data)
    
    # impute missing refugee data
    data <- impute_from_state_data(
      data,
      vars_to_impute = config$data_config$district$impute$vars,
      state_to_impute_from = config$data_config$district$impute$state,
      districts_to_impute = config$data_config$district$impute$districts,
      years_to_impute = c(2013, 2014, 2015, 2016)
    )
    
    # merge districts in refugee data
    data <- merge_districts(data,
                            config$data_config$district$districts_to_group,
                            by_year = TRUE)
    
    # delete old districts
    data <- delete_old_districts(data, 
                                 config$data_config$district$districts_to_drop)
    
    # restrict to districts
    data <- restrict_to_districts(data)
    
    # expand refugee data by date
    data <- expand_by_date(data, by_year = TRUE)
    
    # only keep observations for last day of year, and interpolate rest
    data <- data %>%
      mutate(!!var := ifelse(
        date %in% c(
          as.Date("2013-12-31"),
          as.Date("2014-12-31"),
          as.Date("2015-12-31"),
          as.Date("2016-12-31")
        ),
        get(var),
        NA
      ))
    
    data <- interpolate(data, var, group = "key")
    
    # add_percentage of variable
    var_p <- paste0(var, "_p")
    
    data <- data %>%
      mutate(!!var_p := eval(parse(text = paste0("(", var, "/pop)*100"))))
    
    # drop values before 2014
    data <- data %>%
      ungroup() %>%
      filter(date >= as.Date("2014-01-01")) %>%
      select(key, date, var, var_p)
    
    return(data)
  }


# function to generate interpolated country refugee inflow data
gen_country_date_refugee_data <- function(config) {
  
  # variable to interpolate by date
  var <- config$data_config$district_date$interpolate$country$var
  
  data <- read_csv2(
    config$data_config$district_date$interpolate$country$input,
    show_col_types = FALSE) %>%
    rename(date = month) 
  
  range <- tibble(date = seq(min(data$date), 
                             max(data$date), 
                             by = "days"))
  
  data <- full_join(data, range) %>%
    arrange(date) %>%
    mutate(!!var := zoo::na.approx(.[[var]], na.rm = FALSE)) %>%
    filter(date != "2017-01-01") %>%
    select(date, all_of(var))
  
  return(data)
}



process_district_date_data <- function(data, config) {
  
  data <- expand_by_date(data, 
                         by_year = FALSE, 
                         date_start = "2014-01-01",
                         date_end = "2016-12-31")
  
  refugee <- gen_district_date_refugee_data(config)
  easy <- gen_country_date_refugee_data(config)
   
  data <- data %>%
    left_join(refugee) %>%
    left_join(easy)
  
  return(data)
}
