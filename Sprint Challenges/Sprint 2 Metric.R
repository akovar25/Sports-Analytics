# Load libraries

library(fuzzyjoin)
library(tidyverse)
library(nflreadr)
library(ggplot2)

pbp <- load_pbp(seasons = most_recent_season())
roster <- load_rosters(most_recent_season())

pbp <- pbp %>%
  mutate(
    is_clutch = (
      (qtr == 4 & abs(score_differential) <= 7) |
        (down %in% c(3, 4) & ydstogo >= 7) |
        (yardline_100 <= 20) |
        (qtr == 4 & game_seconds_remaining <= 120 & score_differential <= 0)
    ),
    is_success = epa > 0
  )

ccr_summary <- pbp %>%
  filter(is_clutch) %>%
  summarise(
    clutch_plays = n(),
    successful_clutch_plays = sum(is_success, na.rm = TRUE),
    CCR = successful_clutch_plays / clutch_plays
  )

ccr_by_qb <- pbp %>%
  filter(is_clutch) %>%
  group_by(passer_player_name, posteam) %>%
  summarise(
    clutch_plays = n(),
    successful_clutch_plays = sum(is_success, na.rm = TRUE),
    CCR = successful_clutch_plays / clutch_plays
  ) %>%
  arrange(desc(CCR))

ccr_by_qb <- ccr_by_qb %>%
  mutate(
  first_initial = str_extract(passer_player_name, "^[A-Z]"),
  last_name = str_extract(passer_player_name, "(?<=\\.)\\s*\\w+")
  )

ccr_qb_filtered <- ccr_by_qb %>%
  left_join(roster, by = c("last_name" = "last_name", "posteam" = "team")) %>%
  filter(position == "QB")

ccr_qb_filtered <- ccr_qb_filtered %>%
  filter(clutch_plays >= 10) #minimum involvement in games

ccr_qb_filtered %>%
  filter(!is.na(passer_player_name)) %>%
  group_by(passer_player_name) %>%
  summarise(
    clutch_plays = sum(clutch_plays, na.rm = TRUE),
    successful_clutch_plays = sum(successful_clutch_plays, na.rm = TRUE),
    CCR = successful_clutch_plays / clutch_plays
  ) %>%
  ungroup() %>%
  arrange(desc(CCR)) %>%
  slice_head(n = 15) %>%                                     # Take top 15 only
  ggplot(aes(x = reorder(passer_player_name, CCR), y = CCR)) +
  geom_col(fill = "darkgreen") +
  coord_flip() +
  labs(
    title = "Top 15 Quarterbacks by Clutch Conversion Rate (CCR)",
    x = "Quarterback",
    y = "CCR"
  ) +
  theme_minimal()

J_Love_CCR <- ccr_qb_filtered %>%
  filter(last_name == "Love")

J_Allen_CCR <- ccr_qb_filtered %>%
  filter(last_name == "Allen")

J_Love_CCR$CCR - J_Allen_CCR$CCR
