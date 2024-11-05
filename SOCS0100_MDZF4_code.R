# Setup 
rm(list = ls()) # Clears environment
if (!require("pacman")) { # If pacman is not yet installed, it will be installed
  install.packages("pacman")
}

# Load packages
pacman::p_load(
  tidyverse, # tidyverse packages
  glue, # to combine strings and objects
  ggplot2, # used in data visualisation
  skimr) # skim data

# Import data
data <- read.csv("https://github.com/owid/covid-19-data/raw/refs/heads/master/public/data/cases_deaths/full_data.csv", header = TRUE)

# Skim data to find descriptive statistics 
skim(data)

# Print data from new_cases and new_deaths variables
selected_data <- data %>% 
  select(date,location,new_cases,new_deaths)%>% # Select relavant variables
  slice(411374:411386) # Choose only a few rows to be used in the example

print(selected_data) # Print output

# Data-wrangling 1 - Number of Cases by Continent
# Define the continents
continents <- c("Africa","Asia","Europe",
                "North America","Oceania","South America")

# Data-wrangling
top_cases_continents <- data %>% # Create new data frame to the existing data
  filter(location %in% continents) %>% # Filter out the data for the continents
  group_by(location)%>% # Group by location
  filter(date == max(date))%>% # Filter for the most recent data available
  select(total_cases)%>% # Select the variable for total cases in each continent
  arrange(desc(total_cases)) # Arrange the output in descending order

# Print result
top_cases_continents


# Data wrangling 2 - Proportion of cases by country income
# Proportion of cases by country income
country_income <- c("High-income countries", "Low-income countries", "Lower-middle-income countries","Upper-middle-income countries") # Define the countries by income level

proportion_cases_income <- data %>% # Create new dataframe to existing data for the proportion of cases by country income
  filter(location %in% country_income) %>% # Filter for the rows with country income groups
  filter(date == max(date))%>% # Filter for the most recent data available 
  group_by(date, location) %>% # Group by date and country income groups
  summarise(total_cases_by_income = sum(total_cases, na.rm = TRUE)) # Create summary statistics of total cases by country income group, removing NA values

world_cases <- data %>% # Create new dataframe to existing data for the number of cases in the world
  filter(location == "World") %>% # Filter for entries with "World" as the location
  select(date, world_total_cases = total_cases) # Select the date and rename the total number of cases in the world variable to be included in the output

proportion_cases <- proportion_cases_income %>% # Create new dataframe to calculate the proportion of cases by income group against the number of global cases
  left_join(world_cases, by = "date") %>% # Join the data for income group cases with total cases in the world by date
  mutate(proportion = total_cases_by_income / world_total_cases) # Calculate proportion of cases by income group


proportion_deaths_income <- data %>% # Create new dataframe to existing data for the proportion of deaths by country income
  filter(location %in% country_income) %>% # Filter for the rows with country income groups
  filter(date == max(date))%>% # Filter for the most recent data available
  group_by(date, location) %>% # Group by date and country income groups
  summarise(total_deaths_by_income = sum(total_deaths, na.rm = TRUE)) # Create summary statistics of total deaths by country income group, removing NA values

world_deaths <- data %>% # Create new dataframe to existing data for the number of Covid-19 deaths in the world
  filter(location == "World") %>% # Filter for entries with "World" as the location
  select(date, world_total_deaths = total_deaths) # Select the date and rename the total number of Covid-19 deaths in the world variable to be included in the output

proportion_deaths <- proportion_deaths_income %>% # Create new dataframe to calculate the proportion of Covid-19 deaths by income group against the number of global Covid-19 deaths
  left_join(world_deaths, by = "date") %>% # Join the data for income group deaths with total Covid-19 deaths in the world by date
  mutate(proportion = total_deaths_by_income / world_total_deaths) # Calculate proportion of Covid-19 deaths by income group


# Print results
proportion_cases
proportion_deaths


# Data wrangling 3 - Rate of weekly case and death increase
data <- data %>% # Store the results of these data-wrangling in the original dataframe
  arrange(location,date)%>% # Arrange the data by location and date
  group_by(location)%>% # Group data by location
  mutate(weekly_case_increase = ((weekly_cases - lag(weekly_cases)) / lag(weekly_cases)) *100, # Calculate the weekly case increase rate of all locations as a percentage
         weekly_death_increase = ((weekly_deaths - lag(weekly_deaths)) / lag(weekly_deaths)) *100) # Calculate the weekly death increase rate of all locations as a percentage

data %>%
  select(date,weekly_case_increase,weekly_death_increase) # Select the date, weekly case increase rate and weekly death increase rate variables to be displayed in the output 


# Data wrangling 4 - Case Fatality Rate (CFR)
data <- data %>% # Store the results of these data-wrangling in the original dataframe
  arrange(date)%>% # Arrange the existing data by date
  group_by(location)%>% # Group by location, such that countries would be in alphabetical order
  mutate(cfr = (total_deaths/total_cases) * 100) # Calculate the CFR as a percentage

data %>% 
  select(date,total_deaths,total_cases,cfr) # Select the date, total deaths, total cases and CFR variables to be displayed in the output

# Function to generate a list of countries above a given case fatality rate (CFR)
countries_cfr_above_threshold <- function(data, cfr_threshold) {
  # Obtain the most recent CFR for each country
  data %>%
    group_by(location) %>% # Group data by location, in alphabetical order
    filter(!is.na(total_cases) & !is.na(total_deaths)) %>% # Filter for total cases and total deaths, removing for NA values
    slice_tail(n = 1) %>%  # Get the most recent CFR by taking the last row for each country
    mutate(cfr = (total_deaths / total_cases) * 100) %>% # Calculate CFR as a percentage
    filter(cfr > cfr_threshold) %>%   # Filter countries with CFR above the specified threshold
    select(location, total_cases, total_deaths, date,cfr) %>% # Select variables to be displayed in output
    arrange(desc(cfr)) # Arrange output in descending order (highest CFR first)
}

# List countries with CFR above x% - change x by changing the number below (here, the percentage is 2%)
countries_cfr_above_x <- countries_cfr_above_threshold(data, 2)

# Print result
countries_cfr_above_x


# Data Visualisation 1 - Case fatality rate over time in low-income countries versus high-income countries
cfr_over_time <- data %>% # Store the results of the data-wrangling in the original dataframe
  filter(location %in% c("Low-income countries", "High-income countries"))%>% # Filter out the data points for low-income countries and high-income countries
  group_by(location, date) # Group these data points by location (low or high-income countries) and date

cfr_over_time %>%
  ggplot(aes(x = as.Date(date), y = cfr, color = location)) + # Graph with x-variable being date (time) and y-variable being the case fatality rate. The countries' income group is differentiated by two colours in the graph.
  geom_point(alpha=0.1) + # Point plot, with transparency being 0.1 so that the line of best fit can be better seen.
  geom_smooth( aes(color=location), size = 0.7)+ # Curve of best fit, made thinner to see the distinctions between both lines in the graph
  labs( # Labels for graph
    title = "Case Fatality Rate (CFR) in Low-Income vs High-Income Countries",
    x = "Year",
    y = "Case Fatality Rate (%)",
    color = "Country Income Group"
  ) 


# Data Visualisation 2 - Weekly Case Increase Rate in the UK
weekly_case_increase_we <- data %>% # Store the results of the data-wrangling in the original dataframe
  filter(location %in% c("United Kingdom")) %>% # Filter for a specific country, in this example I used the UK
  group_by(location,date) # Group data by location (the UK) and date, in ascending order

weekly_case_increase_we %>%
  ggplot(aes(x=as.Date(date), y=weekly_case_increase)) + # Graph with x-variable being date (time), y-variable being weekly case increase. 
  geom_line()+ # Line graph to illustrate the fluctuations in case increase/decrease over time.
  labs( # Labels for graph
    title = "Weekly Case Increase Rate in the United Kingdom",
    x = "Year",
    y = "Weekly Case Increase Rate (%)"
  ) 

# Data Visualisation 3 - Number of Cases across various continents
total_cases_continents <- data %>% # Store the results of the data-wrangling in the original dataframe
  filter(location %in% continents) %>% # Filter for continents, as used above
  group_by(location,date) # Group data by the continents, and date (time)

total_cases_continents %>%
  ggplot(aes(x=as.Date(date), y=total_cases, color=location)) + # Graph with x-variable being date (time), y-variable being the total cases in each continent. The colour of each line will differentiate the continents.
  geom_line()+ # Line graph so that the trajectory of cases is clear
  labs( # Labels for graph
    title = "Number of Covid-19 cases across time in various continents",
    x = "Year",
    y = "Number of Covid-19 cases",
    color = "Continent"
  ) +
  scale_y_continuous(labels = scales::comma) # Changes scentific mathematical numbers in the y-axis to readable numeric format


# Function - Data visualisations for weekly case increase rate in each continent from June 2020 onwards
filtered_df <- data %>% 
  filter(location %in% c("Africa", "Asia","Europe","North America",
                         "Oceania","South America","World")) # Dataframe to filter the continents and World statistics
start_date <- as.Date("2020-06-01") # Define the start date as 1 June 2020

create_point_plot <- function(location) { # Define function that creates a point plot for each continent's weekly case increase rate
  filtered_df %>%
    filter(location == location, date >= start_date)%>% # Filter data for the locations listed above and from the start date listed above
    ggplot(aes(x = as.Date(date), y = weekly_case_increase)) + # Graph with x-variable as the date (time, from 1 June 2020 onwards), and y-variable as the rate of weekly case increase
    geom_line() + # Creates a line graph
    geom_smooth(color = "blue") +  # Add curve of best fit to determine the trend
    labs(
      title = glue("Weekly Case Increase Trend in {location}"),
      x = "Year",
      y ="Weekly Case Increase (%)"
    )
}

plots_list <- map(unique(filtered_df$location), create_point_plot) # Maps the function to each continent, generating separate plots for each continent
plots_grid <- gridExtra::grid.arrange(grobs = plots_list, ncol = 2) # Displays the graphs in two columns, making them easier to compare

