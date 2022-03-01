# Assignment 3 - Chart 2 - Comparison of two variables

read_it <- read_csv("/Users/sahilbains/INFO-201code/a3-starter-sbains2/a3-sbains2/docs/incarceration_trends.csv")

male_juvenile_adult_incarceration <- read_it %>%
  group_by(year) %>%
  summarize(male_adult_jail_pop = sum(male_prison_pop, na.rm = TRUE),
            male_juvenile_jail_pop = sum(male_jail_pop, na.rm = TRUE)) %>%
  subset(male_juvenile_jail_pop > 0,
         male_adult_jail_pop > 0)

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
