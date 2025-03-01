agg_tbl <- final_df %>% 
  filter(play_type %in% c("Isolation", "Postup", "PRBallHandler", "Handoff","Cut", "OffRebound", "PRRollMan", "Spotup", "OffScreen", 'Transition', 'Putbacks', 'Misc'))%>%
  group_by(player_id, season) %>% 
  summarise(sum_poss=sum(poss),
            .groups = 'drop')

df=merge(final_df, agg_tbl, by=c("player_id", "season"))

onball=df%>%
  filter(play_type %in% c("Isolation", "Postup", "PRBallHandler", 'Transition', 'Handoff'))
agg_onball <- onball %>% 
  group_by(player_id, season) %>% 
  summarise(sum_poss_group=sum(poss), sum_pts=sum(pts),
            .groups = 'drop')
onball=merge(onball, agg_onball)
onball=onball%>%
  select(player_id, player_name, season, sum_poss, sum_poss_group, sum_pts)%>%
  mutate(type="OnBall", frequency=sum_poss_group/sum_poss, ppp=sum_pts/sum_poss_group)%>%
  select(-sum_pts)%>%
  distinct(player_id, season, .keep_all = TRUE)

offball_int=df%>%
  filter(play_type %in% c("Cut", "OffRebound", "PRRollMan"))

agg_offball_int <- offball_int %>% 
  group_by(player_id, season) %>% 
  summarise(sum_poss_group=sum(poss), sum_pts=sum(pts),
            .groups = 'drop')
offball_int=merge(offball_int, agg_offball_int)
offball_int=offball_int%>%
  select(player_id, player_name, season, sum_poss, sum_poss_group, sum_pts)%>%
  mutate(type="OffBallInt", frequency=sum_poss_group/sum_poss, ppp=sum_pts/sum_poss_group)%>%
  select(-sum_pts)%>%
  distinct(player_id, season, .keep_all = TRUE)



offball_per=df%>%
  filter(play_type %in% c("Spotup", "OffScreen"))

agg_offball_per <- offball_per %>% 
  group_by(player_id, season) %>% 
  summarise(sum_poss_group=sum(poss), sum_pts=sum(pts),
            .groups = 'drop')
offball_per=merge(offball_per, agg_offball_per)
offball_per=offball_per%>%
  select(player_id, player_name, season, sum_poss, sum_poss_group, sum_pts)%>%
  mutate(type="OffBallPer", frequency=sum_poss_group/sum_poss, ppp=sum_pts/sum_poss_group)%>%
  select(-sum_pts)%>%
  distinct(player_id, season, .keep_all = TRUE)



total_df=rbind(offball_int, offball_per, onball)
total_df$type=factor(total_df$type, levels=c("OnBall","OffBallInt","OffBallPer"))

data <- total_df %>%
  arrange(player_id, type, season)


mean <- final_df %>% 
  filter(play_type %in% c("Isolation", "Postup", "PRBallHandler", "Handoff","Cut", "OffRebound", "PRRollMan", "Spotup", "OffScreen", 'Transition'))%>%
  group_by(season, play_type) %>% 
  summarise(sum_poss=sum(poss),
            sum_pts=sum(pts),
            .groups = 'drop')%>%
  mutate(exp_pts=sum_pts/sum_poss)

mean <- final_df %>% 
  filter(play_type %in% c("Isolation", "Postup", "PRBallHandler", "Handoff","Cut", "OffRebound", "PRRollMan", "Spotup", "OffScreen", 'Transition'))%>%
  group_by(season, play_type) %>% 
  summarise(sum_poss=sum(poss),
            sum_pts=sum(pts),
            .groups = 'drop')%>%
  mutate(exp_pts=sum_pts/sum_poss)