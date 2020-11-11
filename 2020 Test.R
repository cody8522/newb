library(tidyverse)
library(ggrepel)
library(ggimage)
library(nflfastR)
options(scipen = 9999)

seasons <- 2020
data <- purrr::map_df(seasons, function(x) {
  readRDS(
    url(
      glue::glue("https://raw.githubusercontent.com/guga31bb/nflfastR-data/master/data/play_by_play_{x}.rds")
    )
  )
})
pbp_rp <- data %>%
  filter(rush == 1 | pass == 1, !is.na(epa), season == "seasons") %>%
  
  
  Cool_Stat <- pbp_rp %>%
  filter(wp > .20 & wp < .80) %>%
  group_by(posteam)%>%
  summarize(mean_epa = mean(epa), sd_epa = sd(epa), plays = n()) %>%
  arrange(-mean_epa)

Cool_Stat <- Cool_Stat %>%
  left_join(teams_colors_logos, by = c('posteam' = 'team_abbr'))
Cool_Stat %>%
  ggplot(aes(x = mean_epa, y= sd_epa)) +
  geom_hline(yintercept = mean(Cool_Stat$sd_epa), color = "red", linetype = "dashed",alpha = 0.5) +
  geom_vline(xintercept = mean(Cool_Stat$mean_epa), color = "red", linetype = "dashed",alpha = 0.5) +
  geom_image(aes(image = team_logo_espn), size = 0.07, asp = 16/9) +
  labs(x ="Efficiency(meanEPA)",y= "Variance(sdEPA)",title = "Efficency vs Variance 2019", caption = "Data:@nflfastR") +
  theme_bw() +
  theme(aspect.ratio = 9/16, plot.title = element_text(size = 14, hjust = 0.5, face = "bold"))+
  scale_y_continuous(breaks = scales::pretty_breaks(n=10))+
  scale_x_continuous(breaks = scales::pretty_breaks(n=10))