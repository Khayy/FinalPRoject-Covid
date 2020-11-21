library(tidyverse)
library(lubridate)


ctracking <- read_csv("data-raw/all-states-history (2).csv")

# remove January data
ctracking <- ctracking %>%
  filter(date >= "2020-02-01")

#remove unnecessary data
ctracking <- ctracking %>%
  select(-dataQualityGrade)

# group data by month
ctracking <- ctracking %>%
  mutate(Month = month(ymd(date))) %>%
  select(Month, everything()) %>% 
  select(-date) %>%
  group_by(Month, state) %>%
  summarise_all(sum, na.rm = TRUE) %>% 
  arrange(Month, state)



# Export to rds file
write_rds(ctracking, path = "./data/ctracking.rds")
