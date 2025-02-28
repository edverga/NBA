library(httr)
library(jsonlite)
library(dplyr)
library(janitor)
library(vroom)
library(hablar)

# Function to scrape NBA dunk score leaders
download_nba_dunks <- function(season = "2024-25", season_type = "Regular Season", output_path = "~/Coding/NBA/scripts/dunks/dunks_data.csv") {
  
  # Headers needed to scrape from the NBA website
  headers <- c(
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
  
  # Prevent vroom connection errors
  Sys.setenv("VROOM_CONNECTION_SIZE" = 131072 * 2)
  
  # Construct URL
  url <- paste0("https://stats.nba.com/stats/dunkscoreleaders?LeagueID=00&Season=",
                season, "&SeasonType=", URLencode(season_type))
  
  # Make GET request
  res <- GET(url = url, add_headers(.headers = headers))
  
  # Check if request was successful
  if (http_type(res) != "application/json") {
    stop("Failed to fetch data. HTTP response type: ", http_type(res))
  }
  
  # Parse JSON response
  json_resp <- fromJSON(content(res, "text"))
  
  # Convert data to a dataframe
  if (!"dunks" %in% names(json_resp)) {
    stop("Unexpected response structure. Check if the API has changed.")
  }
  
  dunks <- data.frame(json_resp$dunks) %>% 
    retype() %>% 
    clean_names()
}

dunks_data <- download_nba_dunks(season = "2024-25", season_type = "Regular Season", output_path = "path/to/dunks_data.csv")

write.csv(dunks_data, file = file.path("~/Coding/NBA/scripts/dunks", "dunks_data.csv"), row.names = FALSE)

print("Dunks data saved successfully as 'dunks_data.csv' in the 'outputs' folder.")