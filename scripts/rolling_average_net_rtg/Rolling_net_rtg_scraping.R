library(rvest)
library(janitor)
library(tibble)
library(tidyverse)
library(zoo)
library(hablar)

# create a list of team names and abbreviations
teams <- list(
  c("Atlanta Hawks", "ATL"),
  c("Boston Celtics", "BOS"),
  c("Brooklyn Nets", "BRK"),
  c("Charlotte Hornets", "CHO"),
  c("Chicago Bulls", "CHI"),
  c("Cleveland Cavaliers", "CLE"),
  c("Dallas Mavericks", "DAL"),
  c("Denver Nuggets", "DEN"),
  c("Detroit Pistons", "DET"),
  c("Golden State Warriors", "GSW"),
  c("Houston Rockets", "HOU"),
  c("Indiana Pacers", "IND"),
  c("Los Angeles Clippers", "LAC"),
  c("Los Angeles Lakers", "LAL"),
  c("Memphis Grizzlies", "MEM"),
  c("Miami Heat", "MIA"),
  c("Milwaukee Bucks", "MIL"),
  c("Minnesota Timberwolves", "MIN"),
  c("New Orleans Pelicans", "NOP"),
  c("New York Knicks", "NYK"),
  c("Oklahoma City Thunder", "OKC"),
  c("Orlando Magic", "ORL"),
  c("Philadelphia 76ers", "PHI"),
  c("Phoenix Suns", "PHO"),
  c("Portland Trail Blazers", "POR"),
  c("Sacramento Kings", "SAC"),
  c("San Antonio Spurs", "SAS"),
  c("Toronto Raptors", "TOR"),
  c("Utah Jazz", "UTA"),
  c("Washington Wizards", "WAS")
)

game_logs <- list()
for (i in 1:length(teams)) {
  team_name <- teams[[i]][1]
  team_abbr <- teams[[i]][2]
  
  url <- paste0("https://www.basketball-reference.com/teams/", team_abbr, "/2025/gamelog-advanced/")
  
  raw_html <- read_html(url)
  raw_table <- raw_html %>%
    html_node("#tgl_advanced") %>%
    html_table(fill = TRUE) %>%
    janitor::clean_names() %>%
    tibble() %>%
    mutate(team = team_name)
  
  game_logs[[team_name]] <- raw_table
}

# combine all game logs into one data frame
game_logs_df <- bind_rows(game_logs)

game_logs_df <- row_to_names(game_logs_df, 1, remove_row = TRUE, remove_rows_above = FALSE)
game_logs_df <- clean_names(game_logs_df)

game_logs_df <- game_logs_df[!(game_logs_df$rk == "Rk"),]
game_logs_df <- game_logs_df[!(game_logs_df$o_rtg == "Advanced"),]

colnames(game_logs_df)[29] <- "team_name"

ratings <- game_logs_df %>%
  retype() %>%
  select(team_name, g, o_rtg, d_rtg) %>%
  mutate(n_rtg = o_rtg - d_rtg) %>%
  group_by(team_name) %>%
  mutate(
    n_rtg_ra = rollmean(n_rtg, k = 5, na.pad = TRUE, align = "right"),
    o_rtg_ra = rollmean(o_rtg, k = 5, na.pad = TRUE, align = "right"),
    d_rtg_ra = rollmean(d_rtg, k = 5, na.pad = TRUE, align = "right")
  ) %>%
  ungroup()


write.csv(ratings, "C:/Users/edoar/Documents/Coding/NBA/scripts/rolling_average_net_rtg/rolling_net_rtg_scraping.csv", row.names=FALSE)