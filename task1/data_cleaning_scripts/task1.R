library(tidyverse)
library(dplyr)
library(janitor)
library(here)
library(readr)

here()

decathlon <- read_rds(here("raw_data/decathlon.rds"))
decathlon

glimpse(decathlon)
view(decathlon)

decathlon <- rownames_to_column(decathlon, "name")

decathlon_clean <- clean_names(decathlon)

decathlon_clean <- decathlon_clean %>%
  pivot_longer(cols = "x100m":"x1500m",
               names_to = "event",
               values_to = "result")

write_csv(decathlon_clean, "clean_data/decathlon_clean.csv")