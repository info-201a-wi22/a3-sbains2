# Assignment 3 - Chart 1 - Trends over time

library(dplyr)
library(stringr)
library(ggplot2)
library(maps)
library(tidyverse)
setwd("~/INFO-201code/a3-starter-sbains2/a3-sbains2/source")

## Graph #1: Time Trend of Total Incarcerations of Minorities and Whites in the United States from 1970 - 2016

read_it <- read_csv("/Users/sahilbains/INFO-201code/a3-starter-sbains2/a3-sbains2/docs/incarceration_trends.csv")

men_prison_max_race<- read_it %>%
  group_by(year) %>%
  summarize(white_male_prison_pop = sum(white_male_prison_pop, na.rm = TRUE),
            black_male_prison_pop = sum(black_male_prison_pop, na.rm = TRUE)) %>% 
  subset(black_male_prison_pop > 0)

racial_incarceration_disparities_timespan <- ggplot(men_prison_max_race, aes(x = year)) +
  geom_line(mapping = aes(y = white_male_prison_pop, color = "white_male_prison_pop")) +
  geom_line(mapping = aes(y = black_male_prison_pop, color = "black_male_prison_pop")) +
  xlab('Years') +
  ylab('Total Incarceration Population') +
  scale_y_continuous(labels = comma) +
  labs(title = str_wrap('Total Incarcerations of Black and White Males in the US', 30)) +
  theme(legend.position = c(0.35, 0.85))

racial_incarceration_disparities_timespan
