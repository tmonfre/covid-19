## Thomas Monfre
## Time-series data visualization of the global COVID-19 pandemic
## Data acquired from: https://data.humdata.org/dataset/novel-coronavirus-2019-ncov-cases

library(tidyverse)
library(animation)
library(maps)

# Prepare Data ------------------------------------------------------------

# import data on confirmed cases
confirmed <- read.csv("./data/time_series-ncov-Confirmed.csv", stringsAsFactors = FALSE) %>% 
  slice(2:n()) %>% 
  rename(country = Country.Region, state = Province.State, date = Date, confirmed = Value) %>% 
  select(country, state, date, confirmed) %>% 
  mutate(confirmed = as.numeric(confirmed)) %>% 
  group_by(country, date) %>% 
  summarise(confirmed = sum(confirmed))

# import data on deaths
deaths <- read.csv("./data/time_series-ncov-Deaths.csv", stringsAsFactors = FALSE) %>% 
  slice(2:n()) %>% 
  rename(country = Country.Region, state = Province.State, date = Date, deaths = Value) %>% 
  select(country, state, date, deaths) %>% 
  mutate(deaths = as.numeric(deaths)) %>% 
  group_by(country, date) %>% 
  summarise(deaths = sum(deaths))

# import data on recovered cases
recovered <- read.csv("./data/time_series-ncov-Recovered.csv", stringsAsFactors = FALSE) %>% 
  slice(2:n()) %>% 
  rename(country = Country.Region, state = Province.State, date = Date, recovered = Value) %>% 
  select(country, state, date, recovered) %>% 
  mutate(recovered = as.numeric(recovered)) %>% 
  group_by(country, date) %>% 
  summarise(recovered = sum(recovered))

# combine to one data frame
data <- left_join(confirmed, deaths, c("country", "date")) %>% 
  left_join(recovered, c("country", "date"))

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
data <- left_join(data, country_codes, by = "country") %>% 
  filter(!is.na(date))

worldmap_covid19 <- read.csv("./data/HiResWorldMapWithISO3.csv", stringsAsFactors = FALSE) %>%
  left_join(data, by = "id")

# Generate Visualizations -----------------------------------------------------------

# delete all previously generated files
unlink("./figures/time-series/individual/*.png")

for (unique_date in unique(data$date)) {
  world_plot <- ggplot(data = subset(worldmap_covid19, date == unique_date & !is.na(x = country))) +
    geom_polygon(mapping = aes(
      x = long,
      y = lat,
      group = group,
      fill = confirmed),
      color = "grey80") +
    geom_polygon(data = subset(worldmap_covid19, is.na(x = country)),
                 mapping = aes(
                   x = long,
                   y = lat,
                   group = group),
                 fill = "grey90",
                 color = "grey80") +
    scale_fill_gradient(limits = c(0, 10000), low = "#fed976", high = "#800026", na.value = "grey90") +
    coord_equal() +
    theme_minimal() +
    labs(title = paste("COVID-19 Confirmed Cases By Country:", unique_date),
         x = "",
         y = "",
         fill = "Cases")
  
  ggsave(filename = paste0("./figures/time-series/individual/", unique_date, ".png"), plot = world_plot)
}

# Create GIF --------------------------------------------------------------

filenames <- paste0("./figures/time-series/individual/", unique(data$date), ".png")

im.convert(filenames, output = "./output/time-series.gif", convert = c("magick", "convert", "gm convert"),
           cmd.fun = if (.Platform$OS.type == "windows") shell else system,
           extra.opts = "", clean = FALSE)
