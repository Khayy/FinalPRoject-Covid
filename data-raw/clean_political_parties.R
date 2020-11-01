library(tidyverse)

# read in party data 
parties <- read_csv("data-raw/state_parties.csv")

# rename Party of Governor column
parties <- parties %>%
  rename(Party = `Party of Govoner`, Name = State, state = Abbreviation)

# Export to rds file
write_rds(parties, path = "./data/political_parties_of_states.rds")
