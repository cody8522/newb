library(tidyverse)
library(ggrepel)
library(ggimage)
library(nflfastR)
options(scipen = 9999)


data <- readRDS(url('https://raw.githubusercontent.com/guga31bb/nflfastR-data/master/data/play_by_play_2019.rds'))

pbp_rp <- data %>%
  filter(rush == 1 | pass == 1, !is.na(epa))


game_data <- pbp_rp %>%
  filter(wp > .20 & wp < .80) %>%
  group_by(posteam, week)%>%
  summarize(mean_epa = epa, sd_epa = sd(epa),score = posteam_score, plays = n()) %>%
  arrange(-mean_epa)

game_data <- game_data %>%
  left_join(teams_colors_logos, by = c('posteam' = 'team_abbr'))

game_data %>%
  ggplot(aes(x = score, y= mean_epa)) +
  geom_hline(yintercept = mean(game_data$mean_epa), color = "red", linetype = "dashed",alpha = 0.5) +
  geom_vline(xintercept = mean(game_data$score), color = "red", linetype = "dashed",alpha = 0.5) +
  geom_image(aes(image = team_logo_espn), size = 0.03, asp = 16/9) +
  labs(x ="MeanEPA",y= "Points Scored",title = "Efficency vs Points 2020", caption = "Data:@nflfastR") +
  theme_bw() +
  theme(aspect.ratio = 9/16, plot.title = element_text(size = 14, hjust = 0.5, face = "bold"))+
  scale_y_continuous(breaks = scales::pretty_breaks(n=10))+
  scale_x_continuous(breaks = scales::pretty_breaks(n=10))