# Assignment #3 - Incarceration

library(dplyr)
library(stringr)
library(ggplot2)
library(maps)
library(tidyverse)
library(scales)
library(ggmap)
library(tmap)
library("sf")
library(mapproj)

setwd("~/INFO-201code/a3-starter-sbains2/a3-sbains2")

it <- file.choose()

read_it <- read_csv("/Users/sahilbains/INFO-201code/a3-starter-sbains2/a3-sbains2/docs/incarceration_trends.csv")
# Summary Information of the 'incarcerations.csv' file


# 1) Comparing 1970s Total Population with 2018 Total Population = 10105518 (and finding the difference between the two)

population_1970 <- read_it %>% 
  filter(year == "1970") %>% 
  filter(total_pop == max(total_pop)) %>% 
  pull(total_pop)

population_2018 <- read_it %>%
  filter(year == "2018") %>%
  filter(total_pop == max(total_pop)) %>%
  pull(total_pop)


popdiff_1970_2018 <- population_2018 - population_1970 # The population difference between 1970 and 2018 was 2208470

# 2) Organizing the incarceration rates of Texas and Washington State per year and finding the ratio of blacks/whites incarcerated

incarceration_rates_TX <- read_it %>%
  select(black_jail_pop, total_jail_pop, state, year, white_jail_pop) %>%
  filter(state == "TX", black_jail_pop > 0) %>%
  group_by(year)

incarceration_rates_WA <- read_it %>%
  select(black_jail_pop, total_jail_pop, state, year, white_jail_pop) %>%
  filter(state == "WA", black_jail_pop > 0) %>%
  group_by(year)

# Ratios of whites/blacks incarcerated in Texas in the most recent year (2018)

incarceration_race_tx <- incarceration_rates_TX %>% 
  select(black_jail_pop, white_jail_pop) %>% 
  filter(year == "2018", black_jail_pop == max(black_jail_pop), white_jail_pop == max(white_jail_pop))

ratio_incarceration_race_tx <- incarceration_race_tx$black_jail_pop/incarceration_race_tx$white_jail_pop

# The ratio of incarceration rates of black individuals to white individuals in Texas was ~ 1.87


#3) Calculating the year when prison populations for men were the highest

# Compiling a data frame of all incarcerations for men - prisons and jails

men_incarceration_years <- read_it %>%
  group_by(year) %>%
  summarize(male_prison_pop = sum(male_prison_pop, na.rm = TRUE),
            male_jail_pop = sum(male_jail_pop, na.rm = TRUE)) %>%
  subset(male_prison_pop > 0)

# Finding the year the max population for men 

men_incarceration_year_max <- men_incarceration_years %>%
  filter(male_prison_pop == max(male_prison_pop)) %>%
  pull(year)

# The year where the maximum population of men were incarcerated was 2013

#4) Creating a table of the average maximum incarceration populations per year for black men vs. white men from 1970 - 2016.

men_prison_max_race<- read_it %>%
  group_by(year) %>%
  summarize(white_male_prison_pop = sum(white_male_prison_pop, na.rm = TRUE),
            black_male_prison_pop = sum(black_male_prison_pop, na.rm = TRUE)) %>% 
  subset(black_male_prison_pop > 0)

#5) Calculating the state with the maximum/minimum amount of male prison incarcerations 

men_prison_pop <- read_it %>%
  group_by(state) %>%
  summarize(male_prison_pop = max(male_prison_pop, na.rm = TRUE)) 

men_prison_pop_max <- men_prison_pop %>%
  filter(male_prison_pop == max(male_prison_pop, na.rm = TRUE)) %>%
  pull(state)

men_prison_pop_min <- men_prison_pop %>%
  filter(male_prison_pop == min(male_prison_pop, na.rm = TRUE)) %>%
  pull(state)

#The state with the maximum amount of male prison incarcerations was California, 
# while the state with the minimum amount of male prison incarcerations was Maine

#6 Comparing male juvenile and adult incarceration rates

male_juvenile_adult_incarceration <- read_it %>%
  group_by(year) %>%
  summarize(male_adult_jail_pop = sum(male_prison_pop, na.rm = TRUE),
            male_juvenile_jail_pop = sum(male_jail_pop, na.rm = TRUE)) %>%
  subset(male_juvenile_jail_pop > 0,
         male_adult_jail_pop > 0)

View(male_juvenile_adult_incarceration_2018)

## Graph #1: Time Trend Graph


racial_incarceration_disparities_timespan <- ggplot(men_prison_max_race, aes(x = year)) +
  geom_line(mapping = aes(y = white_male_prison_pop, color = "white_male_prison_pop")) +
  geom_line(mapping = aes(y = black_male_prison_pop, color = "black_male_prison_pop")) +
  xlab('Years') +
  ylab('Total Incarceration Population') +
  labs(title = str_wrap('Total Incarcerations of Black and White Males in the US', 30)) +
  theme(legend.position = c(0.35, 0.85)) +
  scale_y_continuous(labels = comma)

racial_incarceration_disparities_timespan

## Graph #2: Comparing Two Variables

male_juveline_adult_jail_chart <- ggplot(male_juvenile_adult_incarceration, aes(male_juvenile_adult_incarceration)) +
  geom_col(mapping = aes(x = year, 
                         y = male_adult_jail_pop, 
                         fill = 'Adults')) +
  geom_col(mapping = aes(x = year, 
                         y = male_juvenile_jail_pop, 
                         fill = 'Juveniles')) +
  xlab('Years') +
  ylab('Total Population Jailed') +
  scale_y_continuous(labels = comma) +
  labs(title = str_wrap("Male Juvenile vs. Adult Incarcerated", 30)) +
   scale_fill_brewer(palette = "Accent")

## Mapping

## Map #1
## Blacks and whites incarcerated in Texas 2018: 

# Loading the data necessary and mutating a 'counties' column

incarceration_race_tx_2018 <- read_it %>% 
  select(state, year, black_jail_pop, white_jail_pop) %>% 
  filter(state == "TX", year == "2018")


# Configuring the blank canvas for the map

usa_tbl <- map_data("state") %>% as_tibble()

usa_tbl %>%
  ggplot(aes(long, lat, map_id = region)) +
  geom_map(
    map = usa_tbl,
    color = "gray80", fill = "gray30", size = 0.3
  ) +
  coord_map("ortho", orientation = c(39, -98, 0))

# joining the geo-spatial map with the necessary data

texas_incarceration_map <- map_data("county") %>% 
  unite(region, subregion, sep = ',') %>% 
  left_join('region', by = 'state', copy = T) %>% 
  filter(state == "texas")

