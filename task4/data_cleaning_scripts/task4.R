# load common libraries
library(tidyverse)
library(dplyr)
library(janitor)
library(here)
library(readr)
library(readxl)

here()

# read raw data files
candy_2015 <- read_excel(here("/raw_data/boing-boing-candy-2015.xlsx"))
candy_2016 <- read_excel(here("/raw_data/boing-boing-candy-2016.xlsx"))
candy_2017 <- read_excel(here("/raw_data/boing-boing-candy-2017.xlsx"))

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

# add new columns to give each rater a unique id
candy_2015_clean$id = seq(1, 5630, by = 1)
candy_2016_clean$id = seq(5631, 6889, by = 1)
candy_2017_clean$id = seq(6890, 9349, by = 1)

# add missing columns to 2015, with no values
candy_2015_clean$country = ''
candy_2015_clean$gender = ''

# reorder 2015 columns to match column order of 2016/2017
candy_2015_clean_reordered <- candy_2015_clean[, c(2, 99, 1, 98, 3:97)]

# pivot each data set from wide to long format
candy_2015_clean_vertical <- pivot_longer(data = candy_2015_clean_reordered[,1:99],
                                          cols = 5:97, # exclude 1:4, 98, 99
                                          names_to = "candy",
                                          values_to = "rating")

candy_2016_clean_vertical <- pivot_longer(data = candy_2016_clean[,1:106],
                                          cols = 5:104, # exclude 1:4, 105, 106
                                          names_to = "candy",
                                          values_to = "rating")

candy_2017_clean_vertical <- pivot_longer(data = candy_2017_clean[,1:109],
                                          cols = 5:107, # exclude 1:4, 108, 109
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

# clean country column
invalid_canada <- c("canada`", "can")
invalid_usa <- c("united states of america", "united states", "ussa", "u.s.a", "murica", "usa!",
                 "usa (i think but it's an election year so who can really tell)", "u.s.",
                 "america", "units states", "usa usa usa", "usa! usa! usa!", "the best one - usa",
                 "the yoo ess of aaayyyyyy", "usa!!!!!!", "usa! usa!", "united sates",
                 "sub-canadian north america... 'merica", "trumpistan", "merica", "united stetes",
                 "usa usa usa usa", "united  states of america", "united state", "united staes",
                 "usausausa", "unhinged states", "us of a", "unites states", "the united states",
                 "north carolina", "unied states", "u s", "the united states of america",
                 "unite states", "insanity lately", "usa? hard to tell anymore..", "'merica",
                 "usas", "pittsburgh", "new york", "california",
                 "i pretend to be from canada, but i am really from the united states.",
                 "united stated", "ahem....amerca", "ud", "new jersey", "united ststes",
                 "united statss", "murrika", "usaa", "alaska", "n. america", "u s a",
                 "united statea", "usa usa usa!!!!", "u.s.a.", "us")
invalid_uk <- c("england", "united kingdom", "united kindom", "u.k.", "scotland", "endland")

candy_all_years_clean <- candy_all_years_clean %>%
  mutate(
    country = ifelse(country %in% invalid_canada, "canada", country),
    country = ifelse(country %in% invalid_usa, "usa", country),
    country = ifelse(country %in% invalid_uk, "uk", country)
  )

# remove extra characters to prevent duplicate candies
candy_pattern1 <- "[q][6][_]"
candy_pattern2 <- "_a_k_a_mary_janes"

candy_all_years_clean$candy <- candy_all_years_clean$candy %>%
  str_remove_all(candy_pattern1)

candy_all_years_clean$candy <- candy_all_years_clean$candy %>%
  str_remove_all(candy_pattern2)

invalid_candy <- c("x100_grand_bar")

candy_all_years_clean <- candy_all_years_clean %>%
  mutate(candy = ifelse(candy %in% invalid_candy, "100_grand_bar", candy))

# remove non-candy rows
candy_all_years_clean <- candy_all_years_clean[!grepl(
  "person_of_interest_season_3_dvd_box_set_not_including_disc_4_with_hilarious_outtakes", candy_all_years_clean$candy),]

candy_all_years_clean <- candy_all_years_clean[!grepl(
  "real_housewives_of_orange_county_season_9_blue_ray", candy_all_years_clean$candy),]

# write clean data set to CSV
write_csv(candy_all_years_clean, "../clean_data/boing_boing_candy_clean.csv")