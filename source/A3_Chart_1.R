# Assignment 3 - Chart 1 - Trends over time

library(dplyr)
library(stringr)
library(ggplot2)
library(maps)
library(tidyverse)
setwd("~/INFO-201code/a3-starter-sbains2/a3-sbains2/source")

## Graph #1: Time Trend of Total Incarcerations of Minorities and Whites in the United States from 1970 - 2016

read_it <- read_csv("/Users/sahilbains/INFO-201code/a3-starter-sbains2/a3-sbains2/docs/incarceration_trends.csv")


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


