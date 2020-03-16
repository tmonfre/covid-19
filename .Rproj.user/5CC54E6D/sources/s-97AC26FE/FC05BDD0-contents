## Thomas Monfre
## All-time data visualization of the global COVID-19 pandemic
## Data acquired from: https://data.humdata.org/dataset/novel-coronavirus-2019-ncov-cases

library(tidyverse)
library(maps)

# Prepare Data ------------------------------------------------------------

# import data on confirmed cases
confirmed <- read.csv("./data/time_series-ncov-Confirmed.csv", stringsAsFactors = FALSE) %>% 
  slice(2:n()) %>% 
  rename(country = Country.Region, state = Province.State, confirmed = Value) %>% 
  select(country, state, confirmed) %>% 
  mutate(confirmed = as.numeric(confirmed)) %>% 
  group_by(country) %>% 
  summarise(confirmed = sum(confirmed))

# import data on deaths
deaths <- read.csv("./data/time_series-ncov-Deaths.csv", stringsAsFactors = FALSE) %>% 
  slice(2:n()) %>% 
  rename(country = Country.Region, state = Province.State, deaths = Value) %>% 
  select(country, state, deaths) %>% 
  mutate(deaths = as.numeric(deaths)) %>% 
  group_by(country) %>% 
  summarise(deaths = sum(deaths))

# import data on recovered cases
recovered <- read.csv("./data/time_series-ncov-Recovered.csv", stringsAsFactors = FALSE) %>% 
  slice(2:n()) %>% 
  rename(country = Country.Region, state = Province.State, recovered = Value) %>% 
  select(country, state, recovered) %>% 
  mutate(recovered = as.numeric(recovered)) %>% 
  group_by(country) %>% 
  summarise(recovered = sum(recovered))

# combine to one data frame
data <- left_join(confirmed, deaths, c("country")) %>% 
  left_join(recovered, c("country"))

# import data on country codes
country_codes <- read.csv("./data/country-codes.csv", stringsAsFactors = FALSE) %>%
  select(ISO3166.1.Alpha.3, official_name_en, Region.Name, Sub.region.Name, Continent,
         Region.Code, Sub.region.Code) %>%
  rename(id = ISO3166.1.Alpha.3, country = official_name_en, region_name = Region.Name,subregion_name = Sub.region.Name,
         continent = Continent, region_code = Region.Code, subregion_code = Sub.region.Code)

# fix known bad matches
data$country[data$country == "US"] <- "United States of America"
data$country[data$country == "Vietnam"] <- "Viet Nam"
data$country[data$country == "Congo (Kinshasa)"] <- "Congo"
data$country[data$country == "Cote d'Ivoire"] <- "Côte d'Ivoire"
data$country[data$country == "Curacao"] <- "Curaçao"
data$country[data$country == "Korea, South"] <- "Republic of Korea"
data$country[data$country == "North Macedonia"] <- "The former Yugoslav Republic of Macedonia"

# find unknown bad matches
missing_countries <- anti_join(data, country_codes, by = "country")

# search for easy replacements
for (country in missing_countries$country) {
  match <- grep(country, country_codes$country)

  if (length(match) > 0) {
    data$country[data$country == country] <- country_codes$country[match[1]]
  }
}

# output missing names
anti_join(data, country_codes, by = "country")

# join region code data together
data <- left_join(data, country_codes, by = "country")

worldmap_covid19 <- read.csv("./data/HiResWorldMapWithISO3.csv", stringsAsFactors = FALSE) %>%
  left_join(data, by = "id")

# Global Visualization -----------------------------------------------------------

world_plot <- ggplot(data = subset(worldmap_covid19, country != "China" | is.na(x = country))) +
  # all countries but China
  geom_polygon(mapping = aes(
    x = long,
    y = lat,
    group = group,
    fill = confirmed),
    color = "grey80") +
  # China
  geom_polygon(data = subset(worldmap_covid19, country == "China"),
               mapping = aes(
                 x = long,
                 y = lat,
                 group = group), 
               fill = "#4d0000",
               color = "grey80") +
  scale_fill_gradient(limits = c(0,120000), low = "#fed976", high = "#800026", na.value = "grey90") +
  coord_equal() +
  theme_minimal() +
  labs(title = "COVID-19 Confirmed Cases By Country",
       x = "",
       y = "",
       fill = "Cases")

world_plot
ggsave(filename = "./figures/all-time/world.png", plot = world_plot)


# US Visualization --------------------------------------------------------

# import data on confirmed cases
confirmed <- read.csv("./data/time_series-ncov-Confirmed.csv", stringsAsFactors = FALSE) %>% 
  slice(2:n()) %>% 
  rename(country = Country.Region, region = Province.State, confirmed = Value) %>% 
  select(country, region, confirmed) %>% 
  mutate(confirmed = as.numeric(confirmed)) %>% 
  filter(country == "US") %>% 
  group_by(region) %>% 
  summarise(confirmed = sum(confirmed)) %>% 
  mutate(region = toupper(region))

states <- map_data("state") %>% 
  mutate(region = toupper(region))

us_states_covid19 <- left_join(states, confirmed, by = "region")

us_plot <- ggplot(data = us_states_covid19, mapping = 
         aes(
           x = long, 
           y = lat, 
           group = group,
           fill = confirmed)) +
  geom_polygon(color = "white") +
  scale_fill_gradient(low = "#fed976", high = "#800026", na.value = "grey90") +
  coord_map(projection = "polyconic", xlim = c(min(states$long), max(states$long))) +
  theme_minimal() +
  labs(title = "COVID-19 Confirmed Cases By US State",
       x = "Longitude",
       y = "Latitude")

us_plot
ggsave(filename = "./figures/all-time/us.png", plot = us_plot)
