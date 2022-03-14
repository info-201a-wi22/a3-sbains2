# Assignment #3 - Incarceration

library(dplyr)
library(stringr)
library(ggplot2)
library(maps)
library(tidyverse)
library(scales)
library(stringr)
library(ggmap)
library(tmap)
library("sf")
library(mapproj)
library(readr)
library(mapdata)
library('reshape2')
setwd("~/INFO-201code/a3-starter-sbains2/a3-sbains2")


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


black_males_incarcerated <- read_it %>% 
  select(year, black_male_prison_pop) %>% 
  filter(year > 1970) %>% 
  filter(year < 2016) %>% 
  group_by(year) %>% 
  summarize("Black men Incarcerated" = sum(black_male_prison_pop, na.rm = T))

white_males_incarcerated <- read_it %>% 
  select(year, white_male_prison_pop) %>% 
  filter(year > 1970) %>% 
  filter(year < 2016) %>% 
  group_by(year) %>% 
  summarize("White men Incarcerated" = sum(white_male_prison_pop, na.rm = T))


black_white_prison_data <- left_join(black_males_incarcerated, white_males_incarcerated, by = 'year')

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

View(male_juvenile_adult_incarceration)

## Graph #1: Time Trend Graph


timespan_data <- black_white_prison_data %>% 
  gather(key = review,
         value = population,
         `White men Incarcerated`,
         `Black men Incarcerated`)

racial_incarceration_disparities_timeline <- plot_ly(
  data = timespan_data,
  x = ~year,
  y = ~population,
  color = ~review,
  type = 'bar'
) %>% layout(
  title = "Total Incarcerations of Black and White men in the US"
)


racial_incarceration_disparities_timeline

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
## Total of Blacks imprisoned vs. Whites in 2018


incarceration_rate_TX <- read_it %>%
  select(black_jail_pop, total_jail_pop, state, year, white_jail_pop, fips) %>%
  filter(state == "TX", black_jail_pop > 0) %>%
  group_by(year)


blank_theme <- theme_bw() +
  theme(
    axis.line = element_blank(),        
    axis.text = element_blank(),        
    axis.ticks = element_blank(),       
    axis.title = element_blank(),       
    plot.background = element_blank(),  
    panel.grid.major = element_blank(), 
    panel.grid.minor = element_blank(), 
    panel.border = element_blank()      
  )


state_shape <- map_data("county") %>% 
  unite(polyname, region, subregion, sep = ",") %>%
  left_join(county.fips, by = "polyname")



map_data_new <- left_join(incarceration_rate_TX, state_shape, sby = "fips", na.rm = T)


map <- ggplot(map_data_new) +
  geom_polygon(
    mapping = aes(x = long, y = lat, group = group, fill = total_jail_pop),
    color = "red",
    size = .2        
  ) +
  blank_theme +
  labs(
    title = "Black Jail Populations in Texas", 
    fill = "black_jail_pop"
  )

# Plot the map
plot(map)



  
