# Coronavirus Cases and Deaths: Looking at Covid-19 Data with R

The coronavirus pandemic severely impacted the lives of citizens around the globe. Made as part of the SOCS0100: Computational Social Science course, this project seeks to explore the extent to which quantitative data attests to that statement, through data-wrangling processes that culminate in a series of visualisations to make sense of the data (Wickham et al. 2019). In this project, I conducted data-wrangling and data visualisation processes to elucidate meaning out of a given Covid-19 dataset.

## Getting Started

To set up the project locally, simply use the code in the project and the required packages will be downloaded if you don't already have them. These are the packages used in the project:

-   pacman
-   tidyverse
-   glue
-   ggplot2
-   skimr

## Usage/Examples

The following are some examples of data-wrangling and data visualisation processes done with the dataset provided:

**Example 1: Data-wrangling - Number of cases by continent**

```{r}
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
```

**Example 2: Data visualisation - Number of cases across continents**

```{r}
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
  scale_y_continuous(labels = scales::comma)

```

## Authors

To contact the author for any questions, please use the following details:

-   John Doe (email: [example\@email.com](mailto:example@email.com){.email})
-   Project Link: <https://github.com/example_username/example_name>
