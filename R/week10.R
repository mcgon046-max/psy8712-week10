# Script Settings and Resources 
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
library(haven) 
library(tidyverse) 
library(caret) 

# Data Import and Cleaning 
gss_tbl <- read_sav("../data/GSS2016.sav") |> # read_sav turns dataset into tibble (checked with is_tibble()) missing data is read as "NA", appears to be correct
  drop_na(mosthrs) |># Dropping missing values here as per assignment 
  mutate(mosthrs = as.integer(mosthrs)) |> # Set these to integer values instead of doubles 
  select(-hrs1, -hrs2) |> # Getting rid of these variables as per assignment 
  select(where(~ mean(is.na(.)) < 0.75)) # This call takes the piped data set, calcs the proportion of missingness (mean) and makes sure it is less than 0.75

# Visualization 
gss_tbl |> 
  ggplot(aes(x = mosthrs)) + 
  geom_histogram(binwidth = 5, fill = "steelblue", color = "white") +
  labs(
    title = "Distribution of Maximum Hours Worked Last Week",
    subtitle = "GSS 2016 Data",
    x = "Work Hours",
    y = "Number of Respondents"
  ) +
  theme_minimal()

# Analysis 
