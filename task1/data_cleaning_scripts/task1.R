library(tidyverse)
library(dplyr)
library(janitor)
library(here)
library(readr)

# check working folder path
here()

# read raw data from RDS file format as df "decathlon"
decathlon <- read_rds(here("raw_data/decathlon.rds"))
decathlon

# get information about the df and data
glimpse(decathlon)
view(decathlon)

# change the row names to a new column called "name"
decathlon <- rownames_to_column(decathlon, "name")

# clean the column names (lower case, change periods to underscores)
decathlon_clean <- clean_names(decathlon)

# change wide data format to long format, under new columns "event" and "result"
decathlon_clean <- decathlon_clean %>%
  pivot_longer(cols = "x100m":"x1500m",
               names_to = "event",
               values_to = "result")

# write cleaned data to CSV file format for analysis  
write_csv(decathlon_clean, "clean_data/decathlon_clean.csv")