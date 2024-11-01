#setup 
if (!require("pacman")) {
  install.packages("pacman")
}

pacman::p_load(
  tidyverse, # tidyverse pkgs including purrr
  kableExtra,#table
  flextable, #table
  glue, #combining strings and objects
  ggplot2) #dataviz

#import data
data <- read.csv("https://github.com/owid/covid-19-data/raw/refs/heads/master/public/data/cases_deaths/full_data.csv", header = TRUE)

data
