library(tidyverse)
library(dplyr)
library(ggrepel)
library(ggimage)
library(nflfastR)
options(scipen = 9999)

data <- readRDS(url('https://raw.githubusercontent.com/guga31bb/nflfastR-data/master/data/play_by_play_2020.rds'))


clutch_factor <- data %>%
  filter(qb_dropback == 1, !is.na(wpa), !is.na(passer), drive_quarter_start == 4, quarter_seconds_remaining <120, abs(score_differential) <= 8)%>%
  group_by(passer) %>%
  summarize(WPA = sum(wpa), dropbacks = sum(qb_dropback))%>%
  arrange(-WPA)
