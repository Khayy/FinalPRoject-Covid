library(tidyverse)
library(lubridate)


age <- read_csv("data-raw/age&gender.csv")

# remove January data
age <- age %>%
  select(-`Data as of`, - Footnote)

# group by state
age <- age %>%
  group_by(State, Sex, `Age group`) %>%
  summarise(Deaths = sum(`COVID-19 Deaths`))


# filder the data
age <- age %>%
  filter(Sex != "All Sexes") %>%
  filter(`Age group` != "All Ages") %>%
  filter(`Age group` != "Under 1 year") %>%
  filter(`Age group` != "0-17 years") %>%
  filter(`Age group` != "1-4 years") %>%
  filter(`Age group` != "15-24 years") %>%
  filter(`Age group` != "5-14 years") %>%
  filter(Sex != "Unknown")

# Export to rds file
write_rds(age, path = "./data/age_gender.rds")

  
  
  
  
  