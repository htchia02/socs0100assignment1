#setup 
if (!require("pacman")) {
  install.packages("pacman")
}

pacman::p_load(
  tidyverse, # tidyverse packages
  glue, #combining strings and objects
  ggplot2,
  purrr,
  skimr) #data visualisation

#import data
data <- read.csv("https://github.com/owid/covid-19-data/raw/refs/heads/master/public/data/cases_deaths/full_data.csv", header = TRUE)

skim(data)

# Data wrangling - Number of Cases by Continent
continents <- c("Africa","Asia","Europe",
                "North America","Oceania","South America")

top_cases_continents <- data %>% # Create new data frame to the existing data
  filter(location %in% continents) %>% # Filter out the data for the continents
  group_by(location)%>% # Group by location
  filter(date == max(date))%>% 
  select(total_cases)%>%
  arrange(desc(total_cases))%>%
  head(10)

top_cases_continents

# Data wrangling - Proportion of cases by country income

country_income <- c("High-income countries", "Low-income countries", 
                  "Lower-middle-income countries","Upper-middle-income countries")

proportion_cases_income <- data %>%
  filter(location %in% country_income) %>%
  group_by(date, location) %>%
  summarise(total_cases_by_income = sum(total_cases, na.rm = TRUE))

world_cases <- data %>%
  filter(location == "World") %>%
  select(date, world_total_cases = total_cases)

proportion_cases <- proportion_cases_income %>%
  left_join(world_cases, by = "date") %>%
  mutate(proportion = total_cases_by_income / world_total_cases)

proportion_cases

# Data wrangling - Rate of weekly case and death increase
data <- data %>%
  arrange(location,date)%>%
  group_by(location)%>%
  mutate(weekly_case_increase = ((weekly_cases - lag(weekly_cases)) / lag(weekly_cases)) *100,
         weekly_death_increase = ((weekly_deaths - lag(weekly_deaths)) / lag(weekly_deaths)) *100)

data %>%
  select(date,weekly_case_increase,weekly_death_increase)


# Data wrangling - Case Fatality Rate (CFR)
data <- data %>%
  arrange(date)%>%
  group_by(location)%>%
  mutate(cfr = (total_deaths/total_cases) * 100)

data %>%
  select(date,total_deaths,total_cases,cfr)

# Function to list countries above x% case fatality rate (CFR)
countries_cfr_above_threshold <- function(data, cfr_threshold) {
  # Obtain the most recent CFR for each country
  data %>%
    group_by(location) %>%
    filter(!is.na(total_cases) & !is.na(total_deaths)) %>%
    slice_tail(n = 1) %>%  # Get the most recent CFR by taking the last row for each country
    mutate(cfr = (total_deaths / total_cases) * 100) %>%
    filter(cfr > cfr_threshold) %>%   # Filter countries with CFR above the specified threshold
    select(location, total_cases, total_deaths, date,cfr) %>% # Select variables to be displayed in output
    arrange(desc(cfr)) # Arrange output in descending order (highest CFR first)
}

# List countries with CFR above x% - change x by changing the number below
countries_cfr_above_x <- countries_cfr_above_threshold(data, 2)

# Result
countries_cfr_above_x


# Data Visualisation 1 - case fatality rate over time in low-income countries versus high-income countries
cfr_over_time <- data %>%
  filter(location %in% c("Low-income countries", "High-income countries"))%>%
  group_by(location, date)
  
cfr_over_time %>%
  ggplot(aes(x = as.Date(date), y = cfr, color = location)) +
  geom_point(alpha=0.1) +
  geom_smooth(method = 'lm', aes(color=location), size = 0.7)+
  labs(
    title = "Case Fatality Rate (CFR) in Low-Income vs High-Income Countries",
    x = "Year",
    y = "Case Fatality Rate (%)",
    color = "Country Income Group"
  ) 


# Data Visualisation 2 - Weekly Case Increase Rate in the UK
weekly_case_increase_we <- data %>%
  filter(location %in% c("United Kingdom")) %>%
  group_by(location,date)

weekly_case_increase_we %>%
  ggplot(aes(x=as.Date(date), y=weekly_case_increase, color=location)) +
  geom_line()+
  labs(
    title = "Weekly Case Increase Rate in the United Kingdom",
    x = "Year",
    y = "Weekly Case Increase Rate (%)"
  ) 


# Data Visualisation 3 - Number of Cases across various continents
total_cases_continents <- data %>%
  filter(location %in% continents) %>%
  group_by(location,date)

total_cases_continents %>%
  ggplot(aes(x=as.Date(date), y=total_cases, color=location)) +
  geom_line()+
  labs(
    title = "Number of Covid-19 cases across time in various continents",
    x = "Year",
    y = "Number of Covid-19 cases",
    color = "Continent"
  ) +
  scale_y_continuous(labels = scales::comma) # Changes scentific mathematical numbers in the y-axis to readable numeric format

# Function - Data visualisations for weekly case increase rate in each continent from June 2020 onwards

filtered_df <- data %>% filter(location %in% c("Africa", "Asia","Europe","North America",
                                               "Oceania","South America","World"))
start_date <- as.Date("2020-06-01")

create_point_plot <- function(location) {
  filtered_df %>%
    filter(location == location, date >= start_date)%>%
    ggplot(aes(x = as.Date(date), y = weekly_case_increase)) +
    geom_line() +
    geom_smooth(method = "lm", color = "blue") +  # Add trend line
    labs(
        title = glue("Weekly Case Increase Trend in {location}"),
        x = "Year",
        y ="Weekly Case Increase (%)"
    )
}

plots_list <- map(unique(filtered_df$location), create_point_plot) # Generates one plot per continent
plots_grid <- gridExtra::grid.arrange(grobs = plots_list, ncol = 2) # Displays the graphs in two columns
