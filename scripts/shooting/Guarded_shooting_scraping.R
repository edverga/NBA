library(teamcolors)
library(nbastatR)
library(ggimage)

theme_ev <- function () { 
  theme(panel.grid = element_line(color = "#afa9a9"),
        panel.grid.minor = element_blank(),
        plot.background = element_rect(fill = '#efe8e8', color = '#efe8e8')
  )
}

Sys.setenv("VROOM_CONNECTION_SIZE" = 131072 * 2)

#Set header connections 
headers = c(
  `Connection` = 'keep-alive',
  `Accept` = 'application/json, text/plain, */*',
  `x-nba-stats-token` = 'true',
  `X-NewRelic-ID` = 'VQECWF5UChAHUlNTBwgBVw==',
  `User-Agent` = 'Mozilla/5.0 (Macintosh; Intel Mac OS X 10_14_6) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/78.0.3904.87 Safari/537.36',
  `x-nba-stats-origin` = 'stats',
  `Sec-Fetch-Site` = 'same-origin',
  `Sec-Fetch-Mode` = 'cors',
  `Referer` = 'https://stats.nba.com/players/leaguedashplayerbiostats/',
  `Accept-Encoding` = 'gzip, deflate, br',
  `Accept-Language` = 'en-US,en;q=0.9'
)

#urls for pick & roll stats and isolation stats
url = "https://stats.nba.com/stats/leaguedashplayerptshot?CloseDefDistRange=0-2%20Feet%20-%20Very%20Tight&College=&Conference=&Country=&DateFrom=&DateTo=&Division=&DraftPick=&DraftYear=&DribbleRange=&GameScope=&GameSegment=&GeneralRange=&Height=&LastNGames=0&LeagueID=00&Location=&Month=0&OpponentTeamID=0&Outcome=&PORound=0&PerMode=Totals&Period=0&PlayerExperience=&PlayerPosition=&Season=2024-25&SeasonSegment=&SeasonType=Regular%20Season&ShotClockRange=&ShotDistRange=&StarterBench=&TeamID=0&TouchTimeRange=&VsConference=&VsDivision=&Weight="
res <- GET(url = url, add_headers(.headers=headers))
json_resp <- fromJSON(content(res, "text"))
verytight <- data.frame(json_resp$resultSets$rowSet)
colnames(verytight) <- json_resp[["resultSets"]][["headers"]][[1]]

url = "https://stats.nba.com/stats/leaguedashplayerptshot?CloseDefDistRange=2-4%20Feet%20-%20Tight&College=&Conference=&Country=&DateFrom=&DateTo=&Division=&DraftPick=&DraftYear=&DribbleRange=&GameScope=&GameSegment=&GeneralRange=&Height=&LastNGames=0&LeagueID=00&Location=&Month=0&OpponentTeamID=0&Outcome=&PORound=0&PerMode=Totals&Period=0&PlayerExperience=&PlayerPosition=&Season=2024-25&SeasonSegment=&SeasonType=Regular%20Season&ShotClockRange=&ShotDistRange=&StarterBench=&TeamID=0&TouchTimeRange=&VsConference=&VsDivision=&Weight="
res <- GET(url = url, add_headers(.headers=headers))
json_resp <- fromJSON(content(res, "text"))
tight <- data.frame(json_resp$resultSets$rowSet)
colnames(tight) <- json_resp[["resultSets"]][["headers"]][[1]]

df=rbind(verytight, tight)
df=df%>%
  retype()%>%
  clean_names()%>%
  group_by(player_id, player_name, player_last_team_id, player_last_team_abbreviation) %>% 
  summarise_each(list(sum))%>%
  select(player_id, player_id, fgm, fga, fg2m, fg2a, fg3m, fg3a)
df=df%>%
  mutate(efg=round(((fgm+ 0.5*fg3m)/fga)*100, 2))


url = "https://stats.nba.com/stats/leaguedashplayerstats?College=&Conference=&Country=&DateFrom=&DateTo=&Division=&DraftPick=&DraftYear=&GameScope=&GameSegment=&Height=&ISTRound=&LastNGames=0&LeagueID=00&Location=&MeasureType=Base&Month=0&OpponentTeamID=0&Outcome=&PORound=0&PaceAdjust=N&PerMode=Totals&Period=0&PlayerExperience=&PlayerPosition=&PlusMinus=N&Rank=N&Season=2024-25&SeasonSegment=&SeasonType=Regular%20Season&ShotClockRange=&StarterBench=&TeamID=0&VsConference=&VsDivision=&Weight="
res <- GET(url = url, add_headers(.headers=headers))
json_resp <- fromJSON(content(res, "text"))
tot <- data.frame(json_resp$resultSets$rowSet)
colnames(tot) <- json_resp[["resultSets"]][["headers"]][[1]]

tot=tot%>%
  retype()%>%
  clean_names()%>%
  select(player_id, min, fga)

df=merge(df, tot, by="player_id")
df=df%>%
  mutate(guarded_fga=fga.x, fga=fga.y, guarded_f3g_perc=fg3m/fg3a)%>%
  select(player_id, player_name, player_last_team_id, min, fga, fgm, guarded_fga, fg2m, fg2a, fg3m, fg3a, guarded_f3g_perc, efg)%>%
  arrange(min)%>%
  tail(300)

write.csv(df, "C:/Users/edoar/Documents/R Scripts/NBA/R-basket/Player Profile/guarded_shots_2023_2024.csv", row.names=FALSE)
