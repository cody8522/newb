library(tidyverse)
library(dplyr)
library(ggrepel)
library(ggimage)
library(nflfastR)
options(scipen = 9999)

data <- readRDS(url('https://raw.githubusercontent.com/guga31bb/nflfastR-data/master/data/play_by_play_2020.rds'))


comeback_kings <- data %>%
  filter(qb_dropback == 1, !is.na(wpa), !is.na(passer), score_differential <= -9, wp > 0.10)%>%
  summarize(passer,game_id, desc, wp, wpa,meanEPA = mean(epa), dropbacks = sum(qb_dropback))%>%
  filter(dropbacks > 5)%>%
  arrange(passer)
