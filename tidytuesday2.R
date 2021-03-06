
library(schrute)
library(tidyverse)
library(tidymodels)



tibble(theoffice)

office_ratings <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-03-17/office_ratings.csv')


office_ratings


remove_regex <- "[:punct:]|[:digit:]|parts |part |the |and"



office_ratings %>%
  group_by(season) %>%
  summarise(avg_rating = mean(imdb_rating)) %>%
  ggplot(aes(season, avg_rating)) +
  geom_line() +
  scale_x_continuous(breaks = 1:9)


raw_ratings <- office_ratings %>%
  mutate(
    episode_name = title,
    episode_name = episode_name,
    episode_name = episode_name,
    imdb_rating
  )



office_info <- schrute::theoffice %>%
  mutate(
    season = as.numeric(season),
    episode = as.numeric(episode),
    episode_name = str_to_lower(episode_name),
    episode_name = str_remove_all(episode_name, remove_regex),
    episode_name = str_trim(episode_name)
  ) %>%
  select(season, episode, episode_name, director, writer, character)





characters <- office_info %>%
  count(episode_name, character) %>%
  add_count(character, wt = n, name = "character_count") %>%
  filter(character_count > 800) %>%
  select(-character_count) %>%
  pivot_wider(
    names_from = character,
    values_from = n,
    values_fill = list(n = 0)
  )



creators <- office_info %>%
  distinct(episode_name, director, writer) %>%
  pivot_longer(director:writer, names_to = "role", values_to = "person") %>%
  separate_rows(person, sep = ";") %>%
  add_count(person) %>%
  filter(n > 20) %>%
  distinct(episode_name, person) %>%
  mutate(person_value = 1) %>%
  pivot_wider(
    names_from = person,
    values_from = person_value,
    values_fill = list(person_value = 0)
  )




office <- office_info %>%
  distinct(season, episode, episode_name) %>%
  inner_join(characters) %>%
  inner_join(creators) %>%
  inner_join(raw_ratings %>%
               select(episode_name, imdb_rating))



raw_ratings %>%
  group_by(season) %>%
  summarise(avg_rating = mean(imdb_rating)) %>%
  ggplot(aes(season, avg_rating)) +
  geom_line() +
  scale_x_continuous(breaks = 1:9)



raw_ratings %>%
  arrange(desc(imdb_rating))%>%
  mutate(title = paste0(season,".", episode," ",title),
         title = fct_reorder(title, imdb_rating)) %>%
  head(20)%>%
  ggplot(aes(title, imdb_rating, color = factor(season))) +
  geom_point() + coord_flip()
