library(tidyverse)
library(ggrepel)
library(ggimage)
library(nflfastR)
options(scipen = 9999)

data <- readRDS(url('https://raw.githubusercontent.com/guga31bb/nflfastR-data/master/data/play_by_play_2020.rds'))

run_plays <- data %>%
  filter(rush_attempt == 1 | pass_attempt == 1, wp < .80, wp > .20, !is.na(epa)) %>%
  group_by(posteam) %>%
  select(epa,desc,rush_attempt, drive, play_id, game_id, play)%>%
  mutate(prev_rush = lag(rush_attempt) + rush_attempt)

b2b_runs <- run_plays %>%
  filter (prev_rush == 2) %>%
  group_by(posteam) %>%
  select(epa, desc,prev_rush)%>%
  summarize(run_epa = mean(epa))

b2b_runs <- b2b_runs %>%
  left_join(teams_colors_logos, by = c('posteam' = 'team_abbr'))

  ggplot(b2b_runs, aes(x=reorder(posteam,run_epa), y= run_epa)) +
    theme_bw() +
    theme(aspect.ratio = 9/16, plot.title = element_text(size = 14, hjust = 0.5, face = "bold"))+
  scale_y_continuous(breaks = scales::pretty_breaks(n=20)) +
  scale_x_discrete (breaks = scales::pretty_breaks(n=20))  +
    geom_image(aes(image = team_logo_espn), size = 0.06, asp = 16/9)+
    labs(y= "EPA/Play",title = "EPA On 2nd Run in a Row (2020)", caption = "Data:@nflfastR, @CodyGoggin") 