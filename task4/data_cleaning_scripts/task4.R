# load common libraries
library(tidyverse)
library(dplyr)
library(janitor)
library(here)
library(readr)
library(readxl)

# read raw data files
candy_2015 <- read_excel("../raw_data/boing-boing-candy-2015.xlsx")
candy_2016 <- read_excel("../raw_data/boing-boing-candy-2016.xlsx")
candy_2017 <- read_excel("../raw_data/boing-boing-candy-2017.xlsx")

# select required columns by position (column numbers)
candy_2015_clean <- candy_2015 %>%
  select(c(2:96))

candy_2016_clean <- candy_2016 %>%
  select(c(2:5,7:106))

candy_2017_clean <- candy_2017 %>%
  select(c(2:5,7:109))

# clean the column names
candy_2015_clean <- clean_names(candy_2015_clean)
candy_2016_clean <- clean_names(candy_2016_clean)
candy_2017_clean <- clean_names(candy_2017_clean)

# add new column called year and insert value for each row
candy_2015_clean$year = "2015"
candy_2016_clean$year = "2016"
candy_2017_clean$year = "2017"

# add missing columns to 2015, with no values
candy_2015_clean$country = ''
candy_2015_clean$gender = ''

# reorder 2015 columns to match column order of 2016/2017
candy_2015_clean_reordered <- candy_2015_clean[, c(2, 98, 1, 97, 3:96)]

# pivot each data set from wide to long format
candy_2015_clean_vertical <- pivot_longer(data = candy_2015_clean_reordered[,1:98],
                                          cols = 5:97, # exclude columns 1:4,98
                                          names_to = "candy",
                                          values_to = "rating")

candy_2016_clean_vertical <- pivot_longer(data = candy_2016_clean[,1:105],
                                          cols = 5:104, # exclude columns 1:4,105
                                          names_to = "candy",
                                          values_to = "rating")

candy_2017_clean_vertical <- pivot_longer(data = candy_2017_clean[,1:108],
                                          cols = 5:107, # exclude columns 1:4,108
                                          names_to = "candy",
                                          values_to = "rating")

# rename columns in each data set to match prior to binding rows 
colnames(candy_2015_clean_vertical)[1] <- "trick_or_treating"
colnames(candy_2015_clean_vertical)[2] <- "gender"
colnames(candy_2015_clean_vertical)[3] <- "age"
colnames(candy_2015_clean_vertical)[4] <- "country"
colnames(candy_2015_clean_vertical)[5] <- "year"

colnames(candy_2016_clean_vertical)[1] <- "trick_or_treating"
colnames(candy_2016_clean_vertical)[2] <- "gender"
colnames(candy_2016_clean_vertical)[3] <- "age"
colnames(candy_2016_clean_vertical)[4] <- "country"
colnames(candy_2016_clean_vertical)[5] <- "year"

colnames(candy_2017_clean_vertical)[1] <- "trick_or_treating"
colnames(candy_2017_clean_vertical)[2] <- "gender"
colnames(candy_2017_clean_vertical)[3] <- "age"
colnames(candy_2017_clean_vertical)[4] <- "country"
colnames(candy_2017_clean_vertical)[5] <- "year"

# row bind all three data sets
# all data sets have matching column count and column names
candy_all_years_clean <- rbind(candy_2015_clean_vertical,
                               candy_2016_clean_vertical,
                               candy_2017_clean_vertical)

# lowercase all chr values
candy_all_years_clean <- candy_all_years_clean %>%
  mutate(trick_or_treating = tolower(trick_or_treating)) %>%
  mutate(gender = tolower(gender)) %>%
  mutate(country = tolower(country)) %>%
  mutate(candy = tolower(candy)) %>%
  mutate(rating = tolower(rating))

# remove invalid ages from age column
# assumption: people visiting the website will be aged 10-99
age_pattern <- "[1-9][0-9]" # valid ages 10-99 only

candy_all_years_clean$age <- candy_all_years_clean$age %>%
  str_extract(age_pattern)

# convert age column to numeric data type
candy_all_years_clean$age <- as.numeric(candy_all_years_clean$age)

# write clean data set to CSV
write_csv(candy_all_years_clean, "../clean_data/boing_boing_candy_clean.csv")