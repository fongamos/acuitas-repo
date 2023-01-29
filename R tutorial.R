library(modelr)
library(tidyverse)
library(gapminder)

# plot raw data
gapminder %>% 
  ggplot(aes(year, lifeExp, group = country)) +
  geom_line(alpha = 1/3)

# create nested dataframe
by_country <- gapminder %>% 
  group_by(country, continent) %>% 
  nest()

# create grouped dataframe
by_country_2 <- gapminder %>%
  group_by(country, continent)

# define function for applying model
country_model <- function(df) {
  lm(lifeExp ~ year, data = df)
}

# apply model to all rows in dataframe
models <- map(by_country$data, country_model)

# append models as a column in dataframe
by_country <- by_country %>% 
  mutate(model = map(data, country_model))

# compute residuals for each model
by_country <- by_country %>% 
  mutate(
    resids = map2(data, model, add_residuals)
  )

# unnest dataframe
resids <- unnest(by_country, resids)

# plot residuals
resids %>% 
  ggplot(aes(year, resid)) +
  geom_line(aes(group = country), alpha = 1 / 3) + 
  geom_smooth(se = FALSE)

# plot residuals facetting by continent
resids %>% 
  ggplot(aes(year, resid, group = country)) +
  geom_line(alpha = 1 / 3) + 
  facet_wrap(~continent)

# extract model quality metrics
glance <- by_country %>% 
  mutate(glance = map(model, broom::glance)) %>% 
  unnest(glance, .drop = TRUE)

# look for models that don't fit well
glance %>% 
  arrange(r.squared)

# plot models based on r.squared
glance %>% 
  ggplot(aes(continent, r.squared)) + 
  geom_jitter(width = 0.5)

# plot models with r.squared < 0.25
bad_fit <- filter(glance, r.squared < 0.25)

gapminder %>% 
  semi_join(bad_fit, by = "country") %>% 
  ggplot(aes(year, lifeExp, colour = country)) +
  geom_line()


