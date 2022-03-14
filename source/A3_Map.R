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
