library(tidyverse)
library(ggrepel)
library(ggimage)
library(nflfastR)
options(scipen = 9999)

data <- readRDS(url('https://raw.githubusercontent.com/guga31bb/nflfastR-data/master/data/play_by_play_2020.rds'))

pbp_rp <- data %>%
  filter(desc == "END QUARTER" | desc == "END GAME"| quarter_end == 1) %>%
  select(game_id, desc,total_home_score, total_away_score, qtr, quarter_end) %>%
  summarize(total_pts = total_home_score + total_away_score, qtr)

quart_data <- pbp_rp %>%
  group_by(qtr)%>%
  summarize(sum(total_pts))
  