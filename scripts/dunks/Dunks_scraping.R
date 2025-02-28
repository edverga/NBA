library(httr)
library(jsonlite)
library(dplyr)
library(janitor)
library(ggplot2)
library(vroom)
library(hablar)

#My custom theme for the plot
theme_ev <- function () { 
  theme_minimal(base_size=9, base_family="Archivo") %+replace% 
    theme(panel.grid = element_line(color = "#afa9a9"),
          panel.grid.minor = element_blank(),
          plot.background = element_rect(fill = '#efe8e8', color = '#efe8e8')
    )
}

#Headers needed to scrape from the NBA website
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

#I use this to not get a error while scraping, I don't know what it is
Sys.setenv("VROOM_CONNECTION_SIZE" = 131072 * 2)

url="https://stats.nba.com/stats/dunkscoreleaders?LeagueID=00&Season=2024-25&SeasonType=Regular%20Season"

res <- GET(url = url, add_headers(.headers=headers))
json_resp <- fromJSON(content(res, "text"))
dunks <- data.frame(json_resp$dunks)

dunks <- dunks%>% 
  retype() %>% 
  clean_names()

write.csv(dunks, file = file.path("~/Coding/NBA/scripts/dunks", "dunks_data.csv"), row.names = FALSE)

print("Dunks data saved successfully as 'dunks_data.csv' in the 'outputs' folder.")