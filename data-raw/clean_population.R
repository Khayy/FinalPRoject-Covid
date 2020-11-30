library(tidyverse)

pop <- read_csv("./data-raw/nst-est2019-alldata (1).csv")

pop <- pop %>%
  select(NAME, POPESTIMATE2019)

pop <- pop %>%
  filter(NAME != "United States" & NAME != "Northeast Region" & NAME != "Midwest Region"
         & NAME != "South Region" & NAME != "West Region" & NAME != "Puerto Rico")

write_rds(pop, path = "./data/pop.rds")
