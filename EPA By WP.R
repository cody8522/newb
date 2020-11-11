library(tidyverse)
library(ggrepel)
library(ggimage)
library(nflfastR)
options(scipen = 9999)

data <- readRDS(url('https://raw.githubusercontent.com/guga31bb/nflfastR-data/master/data/play_by_play_2020.rds'))

wp_epa <- data %>%
  filter(rush_attempt == 1 | pass_attempt == 1, !is.na(epa)) %>%
  group_by(wp) %>%
  summarize(epa, n()) %>%
  round(2)

wp_epa2 <- wp_epa %>%
  group_by(wp) %>%
  summarize(epa_play = mean(epa))%>%
  cor(use = "complete.obs")
 

 