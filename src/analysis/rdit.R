library(tidyverse)
library(lubridate)
library(yaml)
library(xtable)

# load config
config <- read_yaml("config.yaml")

# load data
data <- read_csv(config$data_config$district_attack_date$output)

# specify dvs
dvs <- c("attacks_s", "attacks_s_shelt", "attacks_s_nogov")

# specify bandwidth
bandwidth <- c(21, 28, 35)

# specify events
events <- c("2015-01-01", "2016-01-01")

standardize_coef <- TRUE

# aggregate attacks to daily data
data_day <- data %>%
  group_by(date) %>%
  summarise_at(all_of(dvs), sum) %>%
  mutate(day = wday(date, label = T))

# add refugee inflow
data_day <- data_day %>%
  left_join(data %>%
              group_by(date) %>%
              summarise(z_easy_registration = mean(z_easy_registration)))


data_day <- data_day %>%
  # calculate sd of control group
  ungroup() %>%
  mutate(across(
    starts_with("attacks"),
    .fns = ~ (. - mean(., na.rm = T)) / (2 * sd(., na.rm = T)),
    .names = "z_{col}"
  ))

if (standardize_coef) {
  dvs <- paste0("z_", dvs)
}

# generate residuals from global model
for (y in dvs) {
  m0 <-
    lm(formula(paste(y, "~ day + z_easy_registration")), data = data_day)
  data_day[paste0("res_", y)] <- resid(m0)
}

# generate rdits
models <- c()
for (e in events) {
  for (t in bandwidth) {
    event <- as.Date(e)
    
    temp <- data_day %>%
      filter(date >= (event - t) &
               date <= (event + t)) %>%
      mutate(time = as.numeric(date - event),
             nye = ifelse(date >= event, 1, 0))
    
    for (y in dvs) {
      m1 <- lm(formula(paste0("res_", y, " ~ nye*time")), data = temp)
      
      model <- broom::tidy(m1) %>%
        mutate(event = event,
               bandwidth = t,
               dv = y)
      
      models <- rbind(models, model)
      
    }
  }
}


# make table
models <- models %>%
  filter(term == "nye") %>%
  mutate(
    estimate = sprintf("%.2f", round(estimate, 2)),
    estimate = case_when(
      p.value < 0.001 ~ paste0(estimate, "***"),
      p.value < 0.01 ~ paste0(estimate, "**"),
      p.value < 0.05 ~ paste0(estimate, "*"),
      p.value < 0.01 ~ paste0(estimate, "+"),
      TRUE ~ estimate),
    std.error = paste0("(", sprintf("%.2f", round(std.error, 2)), ")"),
    dv = ifelse(
      dv == "z_attacks_s",
      "Full Data",
      ifelse(
        dv == "z_attacks_s_shelt",
        "Approach 1",
        ifelse(dv == "z_attacks_s_nogov",
               "Approach 2", dv)
      )
    ),
    event = format(event, "%b%e, %Y"),
    event = gsub("\\b(\\d{1})\\b", "\\1st", event),
    bandwidth = as.integer(bandwidth)
  ) %>%
  select(event, bandwidth, dv, estimate, std.error)

ests <- models %>% select(-std.error) %>%
  pivot_wider(names_from = dv, values_from = estimate)

ses <- models %>% select(-estimate) %>%
  pivot_wider(names_from = dv, values_from = std.error)

tab <- full_join(ests, ses) %>%
  arrange(bandwidth, event)


for (y in c("2015", "2016")) {
  path <- paste0(config$output$tables, 'rdit_', y, ".tex")
  output <- xtable(
    tab %>% filter(event == paste("Jan 1st,", y)),
    booktabs = TRUE,
    align = c("llcccc"),
    label = paste("rdit", y, sep = "_"),
    caption =
      paste(
        "OLS estimates of the immediate impact of the",
        y,
        "New Year's Eve on the number of anti-refugee attacks across Germany at different bandwidths."
      )
  )
  print(output, file = path)
}
