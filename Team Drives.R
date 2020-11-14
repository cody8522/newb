library(tidyverse)
library(dplyr)
library(ggrepel)
library(ggimage)
library(nflfastR)
options(scipen = 9999)

data <- readRDS(url('https://raw.githubusercontent.com/guga31bb/nflfastR-data/master/data/play_by_play_2020.rds'))

data_up <- data %>%
  separate(drive_start_yard_line, c("teamyard", "yrdlines"), sep = " ", remove = FALSE, convert = TRUE)%>%
  mutate("start_FP" = ifelse(teamyard == 50, 50, ifelse(posteam == teamyard, 100 - yrdlines, yrdlines)))
  

team_drives <- data_up %>%
  filter(!is.na(posteam))%>%
  summarize(play_id, start_FP, ydsnet,drive_play_id_started,game_id, fixed_drive, posteam, drive_game_clock_start, drive_quarter_start, drive_game_clock_end, drive_quarter_end, drive_start_yard_line, drive_end_yard_line, fixed_drive_result)%>%
  mutate("PNT" = ifelse(fixed_drive_result == "Punt",1,0))%>%
  mutate("FG" = ifelse(fixed_drive_result == "Field goal",1,0))%>%
  mutate("TD" = ifelse(fixed_drive_result == "Touchdown",1,0))%>%
  mutate("INT" = ifelse(fixed_drive_result == "Interception",1,0))%>%
  mutate("FUM" = ifelse(fixed_drive_result == "Fumble",1,0))%>%
  mutate("SAF" = ifelse(fixed_drive_result == "Safety",1,0))%>%
  mutate("TOD" = ifelse(fixed_drive_result == "Turnover on downs",1,0))%>%
  mutate("MFG" = ifelse(fixed_drive_result == "Missed field goal",1,0))%>%
  mutate("OTD" = ifelse(fixed_drive_result == "Opp touchdown",1,0))%>%
  mutate("EOH" = ifelse(fixed_drive_result == "End of half",1,0))%>%
  mutate("EOG" = ifelse(fixed_drive_result == "End of game",1,0))
  
  
team_drives$game_drive <- paste(team_drives$game_id, team_drives$fixed_drive)
team_drives$first_play <- (team_drives$play_id - team_drives$drive_play_id_started)


team_drives <- team_drives %>%
  filter(first_play == 0) %>%
  group_by(game_drive) %>%
  summarize(posteam,start_FP, ydsnet, drive_game_clock_start, drive_quarter_start, drive_game_clock_end,drive_quarter_end, drive_start_yard_line, drive_end_yard_line, fixed_drive_result, n(),
            TD, FG, PNT, INT, FUM, TOD, OTD, MFG, EOH,EOG, SAF)
team_drives$av_yds_pct <- (team_drives$ydsnet/team_drives$start_FP)

team_eff <- team_drives %>%
  filter(!is.na(start_FP))%>%
  group_by(posteam) %>%
  summarize(mean_AYP = mean(av_yds_pct), sd_AYP = sd(av_yds_pct),n(),TD = sum(TD), FG = sum(FG), PNT = sum(PNT), INT = sum(INT), FUM = sum(FUM), SAF = sum(SAF), TOD = sum(TOD), MFG = sum(MFG), OTD = sum(OTD), EOH =sum(EOH), EOG = sum(EOG))

team_eff <- team_eff %>%
  left_join(teams_colors_logos, by = c('posteam' = 'team_abbr'))
team_eff %>%
  ggplot(aes(x = sd_AYP, y= mean_AYP)) +
  geom_hline(yintercept = mean(team_eff$mean_AYP), color = "red", linetype = "dashed",alpha = 0.5) +
  geom_vline(xintercept = mean(team_eff$sd_AYP), color = "red", linetype = "dashed",alpha = 0.5) +
  geom_image(aes(image = team_logo_espn), size = 0.05, asp = 16/9) +
  labs(x ="Drive Variance",y= "Drive Efficiency",title = "Drive Efficency vs Variance 2020", caption = "Data:@nflfastR") +
  theme_bw() +
  theme(aspect.ratio = 9/16, plot.title = element_text(size = 14, hjust = 0.5, face = "bold"))+
  scale_y_continuous(breaks = scales::pretty_breaks(n=10))+
  scale_x_continuous(breaks = scales::pretty_breaks(n=10))
