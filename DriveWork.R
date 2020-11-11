library(tidyverse)
library(ggrepel)
library(ggimage)
library(nflfastR)
options(scipen = 9999)

data <- readRDS(url('https://raw.githubusercontent.com/guga31bb/nflfastR-data/master/data/play_by_play_2020.rds'))

pbp_rp <- data %>%
  filter(pass == 1 | rush == 1, !is.na(epa))

drive_data <- pbp_rp %>%
  filter(wp > .20 & wp < .80) %>%
  group_by(game_id, posteam, drive)%>%
  summarize(game_id, posteam, drive_time_of_possession, drive, drive_play_count, posteam_score, posteam_score_post)

drive_summary <- drive_data %>%
  group_by(posteam, game_id) %>%
  summarize(posteam, game_id, total_drives = (max(drive)), average_time = (mean(drive_time_of_possession)))