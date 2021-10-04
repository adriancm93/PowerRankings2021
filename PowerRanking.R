setwd("~/GitHub/PowerRankings2021")

library(tidyverse)
library(nflfastR)

#Load data
data <- nflfastR::load_pbp(2021)

#Prepare data
filt = data %>% filter(
  season_type == 'REG',
  !play_type %in% c('no_play','qb_spike','qb_kneel'),
  !is.na(play_type),
  !is.na(epa)
)

#Sorry my SQL is pretty lame and I'll do this the long way
offense = filt %>%  
  group_by(
    game_id, posteam) %>%
  summarise(
    EPA = sum(epa),
    homet = unique(home_team),
    awayt = unique(away_team)
  ) %>% 
  ungroup() 

defense = filt %>%  
  group_by(
    game_id, defteam) %>%
  summarise(
    DefEPA = sum(epa)
  ) %>% 
  ungroup() 

team = dplyr::inner_join(
  offense,defense, by = c("game_id" = "game_id", "posteam" = "defteam")
  ) %>%  
  rename(
    team = posteam
  )%>% 
  mutate(
    EPADiff = EPA - DefEPA,
    opponent = if_else(team == homet, awayt,homet),
    home = if_else(team == homet,1,0)
    ) %>% 
  select(team,opponent,home,EPADiff) %>% arrange(by=team)


#Model
model = team %>%
  lme4::lmer(
    formula=
      # Response variable
      EPADiff ~
      #adjust for home field
      home +
      #adjust for opponent
      as.factor(opponent) +
      #this is the strength of each team
      (1|team)
  )

#Extract random variable effects
effects = broom.mixed::tidy(model,effects="ran_vals") %>%
  filter(group == 'team') %>% 
  select(level,estimate,std.error) %>% rename(team = level) %>% 
  arrange(
    by = estimate
  )


#Plot
#Let's do 95% confidence interval
z <- 1.96
#Pretty plot
effects %>%
  ggplot(aes(x=factor(team, level = team),estimate)) + 
  
  geom_linerange(alpha = .6, color = 'gray', linetype = 2,aes(ymin=estimate - z*std.error,
                                  ymax=estimate + z*std.error))+
  
  geom_point(aes(fill = estimate),color='gray',pch=21,size=3) +
  
  coord_flip() + ggdark::dark_theme_classic() + 
  theme(
    axis.title.y = element_blank(),
    plot.title = element_text(hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5),
    legend.position = 'none'
  ) +
  labs(y = "EPA Difference",
       caption = "Data: nflfastR, by  @adriancm93",
       title = "Power Rankings",
subtitle = paste0(
  unique(data$season),' season, weeks ',min(data$week),'-',max(data$week), ', adjusted for opponent and home field')
)

#Save it
ggsave(dpi = 600, filename = 'ranks.png',  width = 12,height = 6)

