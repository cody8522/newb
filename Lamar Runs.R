library(tidyverse)
library(dplyr)
library(ggrepel)
library(ggimage)
library(nflfastR)
options(scipen = 9999)

data <- readRDS(url('https://raw.githubusercontent.com/guga31bb/nflfastR-data/master/data/play_by_play_2020.rds'))

lamar_runs <- data %>%
  filter(rush == 1,rusher_player_name == "L.Jackson", !is.na(epa))%>%
  summarize(game_id, desc, epa, qb_dropback)


lamar_runs <- lamar_runs %>%
  summarize(meanEPA = mean(epa))

other_runners <- data %>%
  filter(rush == 1, !is.na(epa), rusher_player_name != "L.Jackson", posteam== "BAL") %>%
  summarize(meanEPA = mean(epa))
  