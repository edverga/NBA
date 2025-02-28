library(httr)
library(jsonlite)
library(janitor)
library(hablar)
library(writexl)
library(data.table)
library(dplyr)

Sys.setenv("VROOM_CONNECTION_SIZE" = 131072 * 2)

# Function to scrape NBA synergy play types
download_nba_synergy_data <- function(start_season = 2015, end_season = 2024) {
  
  # Set header connections 
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
  
  # Initialize an empty dataframe to store the results
  final_df <- data.frame()
  
  # List of play types
  play_types <- c("Isolation", "Postup", "PRBallHandler", "Handoff", "Cut", "OffRebound", "PRRollMan", "Spotup", "OffScreen", "Transition", "Misc")
  
  # Loop through play types
  for (play_type in play_types) {
    
    # Loop through seasons
    for (season in start_season:end_season) {
      
      # Construct the URL for the API request
      url <- paste0("https://stats.nba.com/stats/synergyplaytypes?LeagueID=00&PerMode=Totals&PlayType=", 
                    play_type, "&PlayerOrTeam=P&SeasonType=Regular%20Season&SeasonYear=", 
                    paste0(season, "-", (season%%100)+1), "&TypeGrouping=offensive")
      
      # Print the URL being scraped
      cat("Scraping URL:", url, "\n")
      
      # Make the API request
      res <- GET(url = url, add_headers(.headers=headers))
      
      # Parse the JSON response
      json_resp <- fromJSON(content(res, "text"))
      
      # Extract the data and convert it to a dataframe
      if (!"resultSets" %in% names(json_resp) || length(json_resp$resultSets$rowSet) == 0) {
        next
      }
      
      df <- data.frame(json_resp$resultSets$rowSet)
      
      # Set column names
      colnames(df) <- json_resp[["resultSets"]][["headers"]][[1]]
      
      # Append the data to the final dataframe
      final_df <- rbind(final_df, df)
      
      # Pause for 5 seconds to avoid hitting request limits
      Sys.sleep(5)
    }
  }
  
  # Clean and transform data
  final_df <- final_df %>%
    retype() %>%
    clean_names()
  
  final_df$season <- paste0(
    as.integer(substr(final_df$season_id, 4, 5)) + 2000, "-",
    sprintf("%02d", (as.integer(substr(final_df$season_id, 4, 5)) + 1) %% 100)
  )
  
  return(final_df)
}



final_df=final_df%>%
  retype()%>%
  clean_names()

final_df$season <- paste0(
  as.integer(substr(final_df$season_id, 4, 5)) + 2000, "-",
  sprintf("%02d", (as.integer(substr(final_df$season_id, 4, 5)) + 1) %% 100)
)

dunks_data <- download_nba_synergy_data(start_season = 2015, end_season = 2024)

write.csv(dunks_data, file = file.path("~/Coding/NBA/scripts/synergy", "synergy_data.csv"), row.names = FALSE)

print("Dunks data saved successfully as 'dunks_data.csv' in the 'outputs' folder.")
