#setup 
if (!require("pacman")) {
  install.packages("pacman")
}

pacman::p_load(
  tidyverse, # tidyverse packages
  kableExtra,#table
  flextable, #table
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

# Function to list countries above a specified case fatality rate
countries_cfr_above_threshold <- function(data, cfr_threshold) {
  data %>%
    group_by(location)%>%
    filter(!is.na(total_cases) & is.na(total_deaths)) %>%
    slice_tail(n=1)%>%
    mutate(cfr = (total_deaths/total_cases) * 100) %>%
    filter(cfr > cfr_threshold)%>%
    select(location,total_cases,total_deaths,cfr)%>%
    arrange(desc(cfr))
}

# List countries with CFR above 3%
countries_cfr_above_x <- countries_cfr_above_threshold(covid_data, 1.5)

# View the result
print(countries_above_x)



