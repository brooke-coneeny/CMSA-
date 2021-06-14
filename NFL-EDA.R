library(nflfastR)
library(tidyverse)
library(dplyr)
library(patchwork)
library(extrafont)

# Load all regular season passes from the 2020 regular season:
nfl_passing_plays <- load_pbp(2020) %>%
  filter(play_type == "pass", season_type == "REG", 
         !is.na(epa), !is.na(posteam), posteam != "") %>%
  select(# Player info attempting the pass:
    passer_player_name, passer_player_id, posteam, 
    # Info about the pass:
    complete_pass, interception, yards_gained, touchdown, 
    pass_location, pass_length, air_yards, yards_after_catch, epa, wpa,
    shotgun, no_huddle, qb_dropback, qb_hit, sack,
    # Context about the receiver:
    receiver_player_name, receiver_player_id	,
    # Team context:
    posteam, defteam, posteam_type, 
    # Play and game context:
    play_id, yardline_100, side_of_field, down, qtr, play_clock,
    half_seconds_remaining, game_half, game_id,
    home_team, away_team, home_score, away_score,
    # Description of play
    desc)


##First hypothesis: What teams have the worse offensive lines 

nfl_passing_plays %>%
  group_by(posteam) %>%
  summarize(total_hit = sum(qb_hit) + sum(sack)) %>%
  ungroup() %>%
  mutate(posteam = fct_reorder(posteam, total_hit)) %>%
  ggplot(aes(x = posteam, y = total_hit)) + 
  geom_col(fill = "slategray3") +
  labs(
    x = "Possessing Team",
    y = "QB Hits + Sacks",
    title = "Comparing NFL Team's Offensive Line Strength",
    caption = "Data courtesy of nflfastR"
  ) +
  theme_bw() +
  theme(
    plot.background = element_rect(fill = "grey95"),
    panel.background = element_rect(fill = "grey95"),
    panel.grid.minor = element_blank(),
    text = element_text(family = "Century", size = 12),
    plot.title = element_text(size = 17),
    axis.text.x = element_text(size = 8, angle = 90)
  )


##Second hypothesis: If a QB is hit, are they more likely to throw an interception

nfl_passing_plays %>%   
  filter(sack != 1) %>% #remove sacks because it inflates the value of no hit/interception
  group_by(qb_hit, interception) %>%
  summarize(count = n(), joint_prob = count / nrow(nfl_passing_plays)) %>%
  ungroup() %>%
  mutate(qb_hit_name = case_when(qb_hit == 0 ~ "No hit", TRUE ~ "Hit"), interception_name = case_when(interception == 0 ~ "No interception", TRUE ~ "Interception")) %>%
  ggplot(aes(x=qb_hit_name, y=interception_name)) +
  geom_tile(aes(fill = count), color="white") +
  geom_text(aes(label = round(joint_prob, digits=4)), color = "white", size = 7) +
  labs(
    x = "QB Hit",
    y = "Intercepton",
    title = "Interception Not More Likely When Hit",
    caption = "Data courtesy of nflfastR"
  ) +
  theme(
    plot.caption.position = "plot",
    legend.position = "right", legend.key.width = unit(2,"cm"),
    plot.background = element_rect(fill = "grey95"),
    panel.background = element_rect(fill = "grey95"),
    text = element_text(family = "Century", size = 20),
    plot.title = element_text(size = 25),
    legend.background = element_rect(fill = "grey95"),
    axis.text.x = element_text(size = 15),
    axis.text.y = element_text(size = 15),
    axis.title.y = element_blank(),
    axis.title.x = element_blank(),
    axis.ticks = element_blank()
  )

##Third hypothesis: 

library(patchwork)
nfl_density_compare <- nfl_passing_plays %>%
  ggplot(aes(x = yards_gained, 
             color = as.factor(complete_pass))) + 
  geom_density() +
  geom_rug(alpha = 0.3) +
  theme_bw() +
  labs(x = "Yards Gained",
       y = "Number of Passing Plays") +
  scale_color_manual(values = c("darkblue","darkorange"))

nfl_ecdf_compare <- nfl_passing_plays %>%
  ggplot(aes(x = yards_gained, 
             color = as.factor(complete_pass))) +
  stat_ecdf() +
  geom_rug(alpha = 0.3) +
  theme_bw() +
  labs(x = "Yards Gained",
       y = "Proportion of Passing Plays") +
  scale_color_manual(values = c("darkblue","darkorange"))

nfl_density <- nfl_passing_plays %>%
  ggplot(aes(x = yards_gained)) + 
  geom_density() +
  geom_rug(alpha = 0.3) +
  theme_bw() +
  labs(x = "Yards Gained",
       y = "Number of Passing Plays") 

nfl_ecdf <- nfl_passing_plays %>%
  ggplot(aes(x = yards_gained)) +
  stat_ecdf() +
  geom_rug(alpha = 0.3) +
  theme_bw() +
  labs(x = "Yards Gained",
       y = "Proportion of Passing Plays") 


nfl_density + nfl_ecdf + nfl_density_compare + nfl_ecdf_compare + plot_layout(guides = 'collect')

