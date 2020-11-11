library(tidyverse)
library(ggrepel)
library(ggimage)
library(nflfastR)
options(scipen = 9999)

data <- readRDS(url('https://raw.githubusercontent.com/guga31bb/nflfastR-data/master/data/play_by_play_2020.rds'))
logos <- read_csv("https://raw.githubusercontent.com/leesharpe/nfldata/master/data/logos.csv")

  
negative_script <- data %>%
  filter(pass == 1 | rush == 1, def_wp < .30, !is.na(epa)) %>%
  group_by(defteam) %>%
  summarize(neg_epa = mean(epa))%>%
  rename(team = defteam)

positive_script <- data %>%
  filter(pass == 1 | rush == 1, def_wp > .70, !is.na(epa)) %>%
  group_by(defteam) %>%
  summarize(pos_epa = mean(epa))%>%
  rename(team = defteam)

game_script <-merge(positive_script,negative_script)


game_script <- game_script %>%
  left_join(teams_colors_logos, by = c('team' = 'team_abbr'))
game_script %>%
  ggplot(aes(x = pos_epa, y= neg_epa)) +
  geom_hline(yintercept = mean(game_script$neg_epa), color = "red", linetype = "dashed",alpha = 0.5) +
  geom_vline(xintercept = mean(game_script$pos_epa), color = "red", linetype = "dashed",alpha = 0.5) +
  geom_image(aes(image = team_logo_espn), size = 0.06, asp = 16/9) +
  labs(x ="Pos GS EPA (WP > 0.70)",y= "Neg GS EPA (WP < 0.30)",title = "EPA Allowed by Game Script (2020)", caption = "Data:@nflfastR, @CodyGoggin") +
  theme_bw() +
  theme(aspect.ratio = 9/16, plot.title = element_text(size = 14, hjust = 0.5, face = "bold"))+
  scale_y_continuous(breaks = scales::pretty_breaks(n=10))+
  scale_x_continuous(breaks = scales::pretty_breaks(n=10))