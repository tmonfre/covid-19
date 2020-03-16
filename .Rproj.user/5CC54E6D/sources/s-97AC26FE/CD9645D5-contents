## Thomas Monfre
## Time-series data visualization of the global COVID-19 pandemic
## Data acquired from: https://data.humdata.org/dataset/novel-coronavirus-2019-ncov-cases

library(tidyverse)
library(maps)

# Growth Rate -------------------------------------------------------------

# import data on confirmed cases
confirmed <- read.csv("./data/time_series-ncov-Confirmed.csv", stringsAsFactors = FALSE) %>%
  slice(2:n()) %>%
  rename(country = Country.Region, state = Province.State, confirmed = Value, date = Date) %>%
  select(country, state, confirmed, date) %>%
  mutate(confirmed = as.numeric(confirmed)) %>%
  group_by(country, date) %>%
  summarise(confirmed = sum(confirmed)) %>%
  filter(country == "China")

ggplot(data = confirmed,
       mapping = aes(x = date, y = confirmed)) +
  geom_col()


