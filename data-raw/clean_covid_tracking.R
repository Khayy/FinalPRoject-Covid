library(tidyverse)
library(lubridate)


ctracking <- read_csv("data-raw/all-states-history (2).csv")

# remove January data
ctracking <- ctracking %>%
  filter(date >= "2020-02-01")
