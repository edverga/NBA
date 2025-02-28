# load packages
library(tidyverse) # package for data manipulation and visualization
library(nbastatR) # package for NBA stats
library(httr) # package for http requests
library(jsonlite) # package for working with JSON data
library(janitor) # package for cleaning variable names
library(hablar) # package for formatting numbers and other values with Spanish conventions
library(writexl)
library(ggrepel)
library(ggtext)
library(dineq)
library(data.table)
library(sysfonts)
library(showtext)
library(ggstatsplot)
library(palmerpenguins)
library(tidyverse)
library(cropcircles)
library(dplyr)
library(ggimage)
library(magick)

devtools::install_github("abresler/nbastatR")

theme_ev <- function () { 
  # Use theme_minimal as the base theme and customize some elements
  theme_minimal(base_size=9, base_family="Archivo") %+replace% 
    theme(
      # Change the color of the panel grid
      panel.grid = element_line(color = "#afa9a9"),
      # Remove the minor grid
      panel.grid.minor = element_blank(),
      # Fill the plot background with the color '#efe8e8'
      plot.background = element_rect(fill = '#efe8e8', color = '#efe8e8')
    )
}

# Set environment variable for the API request
Sys.setenv("VROOM_CONNECTION_SIZE" = 131072 * 2)

# get NBA players and their IDs
players <- nba_players() %>% 
  select(namePlayer, idPlayer,isActive) %>% # select only the relevant columns
  filter(isActive==TRUE) %>% # filter only active players
  mutate(url = glue::glue("https://stats.nba.com/stats/leagueseasonmatchups?DateFrom=&DateTo=&DefPlayerID={idPlayer}&DefTeamID=&LeagueID=00&Matchup=Defense&OffPlayerID=&OffTeamID=&Outcome=&PORound=0&PerMode=Totals&Season=2024-25&SeasonType=Regular%20Season")) # create URL for each player

# set connections
headers = c(
  Connection = 'keep-alive',
  Accept = 'application/json, text/plain, */*',
  x-nba-stats-token = 'true',
  X-NewRelic-ID = 'VQECWF5UChAHUlNTBwgBVw==',
  User-Agent = 'Mozilla/5.0 (Macintosh; Intel Mac OS X 10_14_6) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/78.0.3904.87 Safari/537.36',
  x-nba-stats-origin = 'stats',
  Sec-Fetch-Site = 'same-origin',
  Sec-Fetch-Mode = 'cors',
  Referer = 'https://stats.nba.com/players/leaguedashplayerbiostats/',
  Accept-Encoding = 'gzip, deflate, br',
  Accept-Language = 'en-US,en;q=0.9'
)

# create a url string for our scraping function 
results_df = data.frame()
for (i in 1:nrow(players)) {
  url = players$url[i] # get URL for player
  res <- GET(url = url, add_headers(.headers=headers)) # make API request
  json_resp <- fromJSON(content(res, "text")) # parse JSON response
  df_totale <- data.frame(json_resp$resultSets$rowSet) # extract data
  results_df=rbind(results_df, df_totale) # append to results dataframe
}

#Select the name of the columns from the scraping
colnames(results_df) <- json_resp[["resultSets"]][["headers"]][[1]] # set column names
results_df <- results_df %>% 
  retype() %>% # convert variable types
  clean_names() %>% # clean variable names
  mutate(matchup_min=matchup_time_sec/60, matchup_fg2m=matchup_fgm-matchup_fg3m, matchup_fg2a=matchup_fga-matchup_fg3a, efg_pct=((matchup_fg2m+1.5*matchup_fg3m)/matchup_fga)*100) # add new variables

#Calculate minimum number of matchups required for a defender to be considered
results_df=results_df %>%
  group_by(def_player_id) # Group by defender player id

#Set the URL to retrieve advanced player statistics data from NBA website
url="https://stats.nba.com/stats/leaguedashplayerstats?College=&Conference=&Country=&DateFrom=&DateTo=&Division=&DraftPick=&DraftYear=&GameScope=&GameSegment=&Height=&LastNGames=0&LeagueID=00&Location=&MeasureType=Advanced&Month=0&OpponentTeamID=0&Outcome=&PORound=0&PaceAdjust=N&PerMode=PerGame&Period=0&PlayerExperience=&PlayerPosition=&PlusMinus=N&Rank=N&Season=2024-25&SeasonSegment=&SeasonType=Regular%20Season&ShotClockRange=&StarterBench=&TeamID=0&VsConference=&VsDivision=&Weight="

#Make a GET request to the URL with the headers
res <- GET(url = url, add_headers(.headers = headers))
#Convert the JSON response to a data frame
json_resp <- fromJSON(content(res, "text"))
df <- data.frame(json_resp$resultSets$rowSet)

#Set the column names of the data frame to the headers from the JSON response
colnames(df) <- json_resp[["resultSets"]][["headers"]][[1]]  

# Cleans variables and reclassify them
df <- df %>% 
  clean_names() %>% 
  retype()%>%
  mutate(fga=fga/gp)%>%
  select(player_id, player_name, ts_pct, fga, usg_pct)
colnames(df)[1]="off_player_id"
colnames(df)[2]="off_player_name"


df_real=merge(results_df, df, by=c("off_player_id", "off_player_name"))
colnames(df_real)[6]="matchup_gp"


url = "https://stats.nba.com/stats/leaguedashplayerbiostats?College=&Conference=&Country=&DateFrom=&DateTo=&Division=&DraftPick=&DraftYear=&GameScope=&GameSegment=&Height=&LastNGames=0&LeagueID=00&Location=&Month=0&OpponentTeamID=0&Outcome=&PORound=0&PerMode=PerGame&Period=0&PlayerExperience=&PlayerPosition=&Season=2024-25&SeasonSegment=&SeasonType=Regular%20Season&ShotClockRange=&StarterBench=&TeamID=0&VsConference=&VsDivision=&Weight="

res <- GET(url = url, add_headers(.headers=headers))
json_resp <- fromJSON(content(res, "text"))
df_height_weight <- data.frame(json_resp$resultSets$rowSet)

#Select the name of the columns from the scraping
colnames(df_height_weight) <- json_resp[["resultSets"]][["headers"]][[1]]

df_height_weight=df_height_weight%>% 
  retype() %>% 
  clean_names()%>%
  mutate(player_height=round(player_height_inches*2.54,1), player_weight=round(player_weight/2.205, 1))%>%
  select(player_id, player_height, player_weight)
colnames(df_height_weight)[1]="off_player_id"

df_real=merge(df_real, df_height_weight, by=c("off_player_id"))

df_real=df_real%>%
  mutate(fga_weighted=fga*matchup_min, usg_weighted=usg_pct*matchup_min, ts_weighted=ts_pct*matchup_min, height_weighted=player_height*matchup_min, weighted_weight=player_weight*matchup_min)
df_weighted=df_real%>%
  group_by(def_player_name)%>%
  summarise(tot_min=sum(matchup_min), player_activity=sum(fga_weighted), player_usage=sum(usg_weighted), player_shooting=sum(ts_weighted), matchup_avg_height=sum(height_weighted), matchup_avg_weight=sum(weighted_weight))
df_weighted=df_weighted%>%
  mutate(player_activity=player_activity/tot_min, player_usage=player_usage/tot_min, player_shooting=player_shooting/tot_min, matchup_avg_height=round(matchup_avg_height/tot_min,1), matchup_avg_weight=round(matchup_avg_weight/tot_min, 2))


poss_df=df_real%>%
  group_by(def_player_name)%>%
  summarise(tot_min=sum(matchup_min),tot_fga=sum(matchup_fga), tot_3fga=sum(matchup_fg3a), tot_fgm=sum(matchup_fgm), tot_fg3a=sum(matchup_fg3a), tot_fg3m=sum(matchup_fg3m),tot_fg2a=sum(matchup_fg2a),tot_fg2m=sum(matchup_fg2m), tot_sfl=sum(sfl), tot_ast=sum(matchup_ast), tot_blk=sum(matchup_blk), tot_tov=sum(matchup_tov), tot_pos=sum(partial_poss))

poss_df=poss_df%>%
  mutate(sf_per_pos=tot_sfl/tot_pos,efg_pct=((tot_fg2m+1.5*tot_fg3m)/tot_fga)*100, fg2_def_pct=tot_fg2a/tot_fga, fg3_def_pct=tot_fg3a/tot_fga, fga_per_pos=tot_fga/tot_pos)

def_df=merge(df_weighted, poss_df, by=c("def_player_name", "tot_min"))
def_df=def_df%>%
  mutate(fga_z=scale(player_activity),usg_pct_z=scale(player_usage),ts_pct_z=scale(player_shooting),efg_pct_z=scale(efg_pct), perc_fg2a=(tot_fga-tot_fg3a)/tot_fga, perc_fg3a=(tot_fga-tot_fg2a)/tot_fga, ast_per_pos=tot_ast/tot_pos, blk_per_pos=tot_blk/tot_pos, tov_per_pos=tot_tov/tot_pos)%>%
  mutate(player_usage=round(player_usage*100, 1), player_shooting=round(player_shooting*100, 1), perc_fg2a=round(perc_fg2a*100, 1), perc_fg3a=round(perc_fg3a*100, 1), tot_min=round(tot_min, 1), player_activity=round(player_activity, 1), fga_per_pos=round(fga_per_pos, 2), efg_pct=round(efg_pct, 1), perc_fg2a=round(perc_fg2a, 1), perc_fg3a=round(perc_fg3a, 1), ast_per_pos=round(ast_per_pos, 3), blk_per_pos=round(blk_per_pos, 3), stl_per_pos=round(tov_per_pos, 3), sf_per_pos=round(sf_per_pos, 3), fga_z=round(fga_z, 3), usg_pct_z=round(usg_pct_z, 3), ts_pct_z=round(ts_pct_z, 3), tot_stl=tot_tov)%>%
  select(def_player_name, tot_min, tot_pos, player_activity, player_usage, player_shooting, tot_fgm, tot_fga, fga_per_pos, tot_fg2m, tot_fg2a, tot_fg3m, tot_fg3a, efg_pct, perc_fg2a, perc_fg3a, tot_ast,ast_per_pos, tot_blk, blk_per_pos, tot_stl, stl_per_pos, tot_sfl, sf_per_pos, fga_z, usg_pct_z, ts_pct_z, matchup_avg_height, matchup_avg_weight)

def_df=mutate(def_df, mean_z=round(rowMeans(select(def_df, c(fga_z, usg_pct_z, ts_pct_z))),3))
def_df=def_df%>%
  mutate(ast_per_100= ast_per_pos*100, blk_per_100 = blk_per_pos*100, stl_per_100 = stl_per_pos*100, sf_per_100 = sf_per_pos*100, perc_2pt= round(tot_fg2m/tot_fg2a, 3)*100, perc_3pt= round(tot_fg3m/tot_fg3a, 3)*100)%>%
  select(def_player_name, tot_min, tot_pos, player_activity, player_usage, player_shooting, tot_fgm, tot_fga, fga_per_pos, tot_fg2m, tot_fg2a, tot_fg3m, tot_fg3a, perc_2pt, perc_3pt, efg_pct, perc_fg2a, perc_fg3a, tot_ast,ast_per_100, tot_blk, blk_per_100, tot_stl, stl_per_100, tot_sfl, sf_per_100, fga_z, usg_pct_z, ts_pct_z, mean_z, matchup_avg_height, matchup_avg_weight)

url = "https://stats.nba.com/stats/leaguedashplayerbiostats?College=&Conference=&Country=&DateFrom=&DateTo=&Division=&DraftPick=&DraftYear=&GameScope=&GameSegment=&Height=&LastNGames=0&LeagueID=00&Location=&Month=0&OpponentTeamID=0&Outcome=&PORound=0&PerMode=PerGame&Period=0&PlayerExperience=&PlayerPosition=&Season=2024-25&SeasonSegment=&SeasonType=Regular%20Season&ShotClockRange=&StarterBench=&TeamID=0&VsConference=&VsDivision=&Weight="

res <- GET(url = url, add_headers(.headers=headers))
json_resp <- fromJSON(content(res, "text"))
df_height_weight <- data.frame(json_resp$resultSets$rowSet)

#Select the name of the columns from the scraping
colnames(df_height_weight) <- json_resp[["resultSets"]][["headers"]][[1]]

df_height_weight=df_height_weight%>% 
  retype() %>% 
  clean_names()%>%
  select(player_name, player_height_inches, player_weight)%>%
  mutate(player_height=round(player_height_inches*2.54,1), player_weight=round(player_weight/2.205, 1))%>%
  select(player_name, player_height, player_weight)
colnames(df_height_weight)[1]="def_player_name"

def_df=merge(def_df, df_height_weight, by=c("def_player_name"))

def_df=def_df%>%
  mutate(height_delta=matchup_avg_height-player_height)%>%
  mutate(weight_delta=matchup_avg_weight-player_weight)%>%
  select(def_player_name, player_height, player_weight, matchup_avg_height, matchup_avg_weight, height_delta, weight_delta, tot_min, tot_pos, player_activity, player_usage, player_shooting, tot_fgm, tot_fga, fga_per_pos, tot_fg2m, tot_fg2a, tot_fg3m, tot_fg3a, perc_2pt, perc_3pt, efg_pct, perc_fg2a, perc_fg3a, tot_ast,ast_per_100, tot_blk, blk_per_100, tot_stl, stl_per_100, tot_sfl, sf_per_100, fga_z, usg_pct_z, ts_pct_z, mean_z)

saveRDS(df_real, file = "matchupdb.rds")
write_xlsx(df_real, "matchupdb.xlsx")
##########################################
ids=df_real%>%
  select(def_player_id, def_player_name)%>%
  mutate(def_player_name = str_remove_all(def_player_name, "( Jr\\.| Sr\\.| III| II| IV)"))%>%
  distinct()
min_df=def_df%>%
  filter(tot_pos>=median(tot_pos))
min_df <- min_df %>% 
  mutate(def_player_name = str_remove_all(def_player_name, "( Jr\\.| Sr\\.| III| II| IV)"))
min_df$short_name <- paste0(substr(min_df$def_player_name, 1, 1), ". ", word(min_df$def_player_name, -1))
min_df=merge(min_df, ids, by="def_player_name")
def_df <- def_df %>% 
  mutate(def_player_name = str_remove_all(def_player_name, "( Jr\\.| Sr\\.| III| II| IV)"))
def_df$short_name <- paste0(substr(def_df$def_player_name, 1, 1), ". ", word(def_df$def_player_name, -1))
min_df <- min_df %>% 
  mutate(def_player_name = str_remove_all(def_player_name, "( Jr\\.| Sr\\.| III| II| IV)"),
         img = glue::glue("https://cdn.nba.com/headshots/nba/latest/1040x760/{def_player_id}.png"))


