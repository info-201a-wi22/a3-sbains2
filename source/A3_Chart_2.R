# Assignment 3 - Chart 2 - Comparison of two variables

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
