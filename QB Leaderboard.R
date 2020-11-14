library(tidyverse)
library(dplyr)
library(ggrepel)
library(ggimage)
library(nflfastR)
options(scipen = 9999)

data <- readRDS(url('https://raw.githubusercontent.com/guga31bb/nflfastR-data/master/data/play_by_play_2020.rds'))

qb_leaderboard <- data %>%
  filter(qb_dropback == 1, !is.na(epa), !is.na(passer))%>%
  group_by(passer) %>%
  summarize(meanEPA = mean(epa), dropbacks = sum(qb_dropback))%>%
  filter(dropbacks > 10)%>%
  arrange(-meanEPA)
