library(tidyverse)
library(lubridate)
library(patchwork)
library(sf)
library(yaml)

theme_set(theme_bw(base_size = 12))

# load config
config <- read_yaml("config.yaml")

# load data
data <- read_csv(config$data_config$district_attack_date$output)
attacks_raw <- read_csv(config$data_config$attacks$output)
refugee_inflow <- read_csv2('data/raw/country/easy.csv') %>%
  rename(date = month,
         'Registered arrivals' = easy_registration) %>%
  pivot_longer(-date)


# Descriptive tables ------------------------------------------------------

data %>% 
  select(year, starts_with("attacks_"), -(ends_with(c("bin", "p90", "nd")))) %>% 
  group_by(year) %>% 
  pivot_longer(-year) %>% 
  group_by(year, name) %>% 
  summarise(n = sum(value)) %>% 
  mutate(name = case_when(
    name == "attacks_s" ~ "Attacks (all)", 
    name == "attacks_s_shelt" ~ "Approach 1 (at shelter)", 
    name == "attacks_s_nogov" ~ "Approach 2 (non-gov.)",
    TRUE ~ name)) %>%
  pivot_wider(names_from = year, values_from = n) %>% 
  xtable::xtable(booktabs = TRUE)

data %>% 
  select(year, starts_with("attacks_") & ends_with("bin"), -ends_with("nd_bin")) %>% 
  group_by(year) %>% 
  pivot_longer(-year) %>% 
  group_by(year, name) %>% 
  summarise(n = sum(value)) %>% 
  mutate(name = case_when(
    name == "attacks_s" ~ "Attacks (all)", 
    name == "attacks_s_shelt" ~ "Approach 1 (at shelter)", 
    name == "attacks_s_nogov" ~ "Approach 2 (non-gov.)",
    TRUE ~ name)) %>%
  pivot_wider(names_from = year, values_from = n)


# Attacks over time  ------------------------------------------------------

# subset relevant attacks
attacks <- attacks_raw %>%
  filter(((data_type == "scraped") & source_type == "other") |
           (data_type == "scraped_shelt")) %>%
  mutate(
    source_type =
      case_when((data_type == "scraped_shelt") ~ "Attacks (at shelter)",
                ((data_type == "scraped") &
                   source_type == "other")  ~ "Attacks (non-government sources)",
                TRUE ~ source_type
      )
  )

dates <- data.frame(date = seq(min(attacks$date), max(attacks$date), by = "1 day")) %>%
  mutate(week = week(date),
         year = year(date))

# reshape
attacks <- attacks %>%
  arrange(date) %>%
  mutate(week = week(date),
         year = year(date)) %>%
  group_by(year, week, source_type) %>%
  summarise(n = n()) %>%
  filter(week <= 52) %>%
  pivot_longer(-c(year, week, source_type))


attacks <- left_join(dates, attacks)

# plots
p1 <- attacks %>%
  filter(source_type == "Attacks (at shelter)") %>%
  filter(!is.na(value)) %>%
  ggplot(aes(x = date, y = value)) +
  geom_vline(
    xintercept = as.Date("2015-12-31"),
    linetype = 2,
    size = 0.8,
    col = 'red'
  ) +
  geom_area(
    size = 0.6,
    col = 'black',
    fill = 'grey',
    alpha = 1 / 2
  ) +
  ylim(c(0, max(attacks$value, na.rm = T))) +
  ylab("Attacks/week") +
  theme(axis.title.x = element_blank()) +
  labs(title = bquote(
    bold("Approach 1:") ~
      "Attacks after excluding those that occurred away from refugee shelters"
  ))

p2 <- attacks %>%
  filter(source_type == "Attacks (non-government sources)") %>%
  filter(!is.na(value)) %>%
  ggplot(aes(x = date, y = value)) +
  geom_vline(
    xintercept = as.Date("2015-12-31"),
    linetype = 2,
    size = 0.8,
    col = 'red'
  ) +
  geom_area(
    size = 0.6,
    col = 'black',
    fill = 'grey',
    alpha = 1 / 2
  ) +
  ylab("Attacks/week") +
  ggtitle("Approach 2: Attacks after selecting those from non-government sources") +
  theme(axis.title.x = element_blank()) +
  labs(title = bquote(
    bold("Approach 2:") ~
      "Attacks after selecting those from non-government sources"
  ))

p3 <- refugee_inflow %>%
  filter(name == "Registered arrivals") %>%
  ggplot(aes(x = date, y = value)) +
  geom_bar(
    stat = "identity",
    col = 'black',
    fill = 'grey',
    alpha = 1 / 2
  ) +
  ylab("Arrivals/month") +
  ggtitle("Number of asylum-seekers arrivals to Germany per month") +
  theme(axis.title.x = element_blank())

p <- p1 / p2 / p3
p
path <- paste0(config$output$plots, 'attacks_over_time.jpg')
ggsave(path,
       p,
       width = 10,
       height = 7,
       dpi = 600)
path <- paste0(config$output$plots, 'attacks_over_time.pdf')
ggsave(path,
       p,
       width = 10,
       height = 7,
       dpi = 600)




# Arson attacks -----------------------------------------------------------

arsons <- attacks_raw %>% 
  filter(data_type == "scraped" & category_en == "arson")  %>% 
  group_by(date, location, state) %>%
  mutate(n = 1, 
         rowid = 1:n())

arsons_shelt <- attacks_raw %>% 
  filter(data_type == "scraped_shelt" & category_en == "arson") %>% 
  group_by(date, location, state) %>%
  mutate(rowid = 1:n())

# identify arsons that were ecxluded in approach 1
arsons_notshelt <- anti_join(arsons, 
                             arsons_shelt %>% 
                               select(date, location, state, rowid), 
                             by = c("date", "location", "state", "rowid"))

arsons <- arsons %>% 
  full_join(arsons_notshelt %>% mutate(not_shelt = TRUE)) %>%
  mutate(not_shelt = ifelse(is.na(not_shelt), FALSE, not_shelt))

# identify arson attacks on uninhabited refugee shelters
arson_descriptions <- unique(trimws(arsons$description))

uninhabited <- c(8, 15, 23, 28, 30, 32, 35, 37, 42,43, 44, 45, 49, 52, 53, 54, 
                 57, 59, 60, 61, 63, 64, 65, 66, 69, 70, 71, 72, 73, 75, 76, 
                 79, 80, 82, 83, 85, 86, 87, 88, 91, 92, 93, 96, 97, 98, 100, 
                 101, 103, 104, 105, 107, 108, 109, 111, 116, 117, 120, 128, 
                 132, 133, 134, 138, 140, 142, 143, 150, 158, 159, 160, 163, 
                 166, 179, 184, 191, 204, 208, 209, 212, 213, 218, 219, 223, 
                 224, 225, 228, 229, 231, 232, 233, 234, 235, 237, 240, 241, 
                 243, 247, 249)

uninhabited_descriptions <- arson_descriptions[uninhabited]

# share of all arson attacks that targetted uninhabited shelters with any description
length_all_desc <- length(arson_descriptions[!str_detect(arson_descriptions, "^Aus der Antwort")])
length_uninhabited <- length(uninhabited_descriptions)

cat("Number of arson attacks with any event description:", length_all_desc)
cat("Number of arson attacks on unoccupied location:", length_uninhabited)
cat("% of attacks with description that were unoccupied:", 
    round(length_uninhabited / length_all_desc, 2))

# add information on occupancy status
arsons <- arsons %>% 
  mutate(uninhabited = ifelse(description %in% uninhabited_descriptions, 
                              TRUE, FALSE),
         inhabited_cat = case_when(
           uninhabited ~ "Unoccupied", 
           not_shelt ~ "Not at shelter",
           TRUE ~ "Occupied/unclear"
         ))

# occupancy over year
arsons %>% 
  mutate(year = year(date)) %>% 
  group_by(year, inhabited_cat) %>% 
  count() %>%
  pivot_wider(names_from = year, values_from = n) %>%
  print()
  
# arsons per week
arsons %>% 
  mutate(week = week(date), 
         year = year(date)) %>% 
  group_by(year, week) %>% 
  count()

# plot arsons over time
dates <- bind_rows(
  tibble(
  date = seq(
    from = as.Date("2014-01-01"),
    to =  as.Date("2016-12-31"),
    by = "1 day"), 
  inhabited_cat = "Unoccupied"), 
  tibble(
    date = seq(
      from = as.Date("2014-01-01"),
      to =  as.Date("2016-12-31"),
      by = "1 day"), 
    inhabited_cat = "Occupied/unclear"),
  tibble(
    date = seq(
      from = as.Date("2014-01-01"),
      to =  as.Date("2016-12-31"),
      by = "1 day"), 
    inhabited_cat = "Not at shelter")
  )
  
arsons <- arsons %>%
  ungroup() %>%
  select(date, inhabited_cat, n) %>% 
  full_join(dates, by = c("date", "inhabited_cat")) %>%
  mutate(n = ifelse(is.na(n), 0, n)) %>%
  arrange(date) %>%
  mutate(week = week(date),
         year = year(date)) %>%
  filter(week <= 52)

# arsons over time, total
p1 <-  arsons %>%
    group_by(year, week) %>%
    summarise(date = as.Date(floor_date(date)),
              n = sum(n)) %>%
    ggplot(aes(x = date, y = n)) +
    geom_area(
      size = 0.6,
      col = 'black',
      fill = 'grey',
      alpha = 1 / 2
    ) +
    geom_vline(
      xintercept = as.Date("2015-12-31"),
      linetype = 2,
      size = 0.8,
      col = 'red'
    ) +
    ylab("Attacks/week") +
    scale_y_continuous(breaks = c(2, 4, 6, 8, 10)) +
    ggtitle("Arson attacks per week") +
    theme(axis.title.x = element_blank())
  
# arsons over time by uninhabited
p2 <-  arsons %>%
  group_by(year, week, inhabited_cat) %>%
  summarise(date = as.Date(floor_date(date)),
            n = sum(n)) %>%
  mutate(inhabited_cat = factor(inhabited_cat, 
                                levels = c("Occupied/unclear", "Unoccupied", "Not at shelter"))) %>% 
  ggplot(aes(x = date, y = n)) +
  geom_area(
    aes(fill = inhabited_cat),
    size = 0.6,
  ) +
  geom_vline(
    xintercept = as.Date("2015-12-31"),
    linetype = 2,
    size = 0.8,
    col = 'red'
  ) +
  geom_line(data = arsons %>% 
              group_by(year, week) %>% 
              summarise(date = as.Date(floor_date(date)), 
                        n = sum(n)), aes(x = date, y = n), col = "black", size = 0.6) + 
  ylab("Attacks/week") +
  scale_y_continuous(breaks = c(2, 4, 6, 8, 10)) +
  scale_fill_manual(name = "Occupancy status",
                    values = c("grey", "black", "red")) + 
  ggtitle("Arson attacks per week by occupancy status and location") +
  theme(axis.title.x = element_blank(), 
        legend.position = "bottom", 
        legend.margin=margin(t = 0, b = 0, unit='cm'))

p <- p1 / p2 / p3
path <- paste0(config$output$plots, 'arson_over_time_uninhabited.jpg')
ggsave(path,
       p,
       width = 10,
       height = 7,
       dpi = 600)
path <- paste0(config$output$plots, 'arson_over_time_uninhabited.pdf')
ggsave(path,
       p,
       width = 10,
       height = 7,
       dpi = 600)



