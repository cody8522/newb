library(tidyverse)
library(dplyr)
library(ggrepel)
library(ggimage)
library(nflfastR)
options(scipen = 9999)

data <- readRDS(url('https://raw.githubusercontent.com/guga31bb/nflfastR-data/master/data/play_by_play_2020.rds'))

team_drives <- data %>%
  filter(!is.na(posteam))%>%
  summarize(play_id, drive_play_id_started,game_id, fixed_drive, posteam, drive_game_clock_start, drive_game_clock_end, drive_start_yard_line, drive_end_yard_line, fixed_drive_result)
  team_drives$game_drive <- paste(team_drives$game_id, team_drives$fixed_drive)
  team_drives$first_play <- (team_drives$play_id - team_drives$drive_play_id_started)
team_drives <- team_drives %>%
  filter(first_play == 0) %>%
  group_by(game_drive) %>%
  summarize(posteam,drive_game_clock_start,drive_game_clock_end, drive_start_yard_line, drive_end_yard_line, fixed_drive_result)%>%
  filter(posteam == "KC")