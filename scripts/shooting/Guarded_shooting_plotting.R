library(tidyverse)   # Contains ggplot2, dplyr, tidyr, readr, purrr, tibble, stringr, forcats
library(jsonlite)    # For JSON parsing (fromJSON)
library(dplyr)       # For data manipulation (group_by, summarise, mutate, filter, select, arrange)
library(janitor)     # For clean_names()
library(hablar)      # For retype()
library(gt)          # For creating tables (gt, tab_spanner, tab_header, cols_label, etc.)
library(webshot2)    # For saving tables as images (gtsave)
library(nbastatR)    # For fetching NBA player data (nba_players)
library(scales)      # For color scales in tables (scales::col_numeric)
library(ggtext)      # For markdown text formatting (md())

#My custom theme for the plot
theme_ev <- function () { 
  theme_minimal(base_size=9, base_family="Archivo") 
  theme(panel.grid = element_line(color = "#afa9a9"),
        panel.grid.minor = element_blank(),
        plot.background = element_rect(fill = '#efe8e8', color = '#efe8e8')
  )
}

shooters <- df %>%
  filter(fg3a>=35)


image_df = nba_players()
image_df = image_df %>%
  select(idPlayer, urlPlayerHeadshot)

shooters = merge(shooters, image_df, by.x = "player_id", by.y = "idPlayer")


gt_theme_538 <- function(data,...) {
  data %>%
    opt_all_caps()  %>%
    opt_table_font(
      font = list(
        google_font("Chivo"),
        default_fonts()
      )
    ) %>%
    tab_style(
      style = cell_borders(
        sides = "bottom", color = "transparent", weight = px(2)
      ),
      locations = cells_body(
        columns = TRUE,
        # This is a relatively sneaky way of changing the bottom border
        # Regardless of data size
        rows = nrow(data$`_data`)
      )
    )  %>% 
    tab_options(
      table.width = px(700),
      column_labels.background.color = "white",
      table.border.top.width = px(3),
      table.border.top.color = "transparent",
      table.border.bottom.color = "transparent",
      table.border.bottom.width = px(3),
      column_labels.border.top.width = px(3),
      column_labels.border.top.color = "transparent",
      column_labels.border.bottom.width = px(3),
      column_labels.border.bottom.color = "black",
      data_row.padding = px(3),
      source_notes.font.size = 9,
      table.font.size = 16,
      heading.align = "left"
    ) 
}




tab_538 <- shooters %>%
  arrange((-guarded_f3g_perc)) %>%
  head(13) %>%
  mutate(guarded_f3g_perc = round(guarded_f3g_perc*100, 1),
         efg = round(efg, 1)) %>%
  select(urlPlayerHeadshot, player_name, fg3m, fg3a, guarded_f3g_perc, efg) %>%
  gt()  %>%
  tab_spanner(
    label = "STATS",
    columns = 3:6
  ) %>% 
  tab_header(
    title = md("**Guarding Them Doesn't Matter**"),
    subtitle = md("Players with ≥35 Tightly or Very Tightly Guarded 3PA")
  ) %>% 
  data_color(
    columns = vars(guarded_f3g_perc),
    colors = scales::col_numeric(
      palette = c("white", "#cc0000"),
      domain = NULL
    )
  ) %>% 
  cols_label(
    urlPlayerHeadshot = '', 
    player_name = '',
    fg3a = '3PTA', 
    fg3m = '3PTM', 
    guarded_f3g_perc = '3PT %', 
    efg = 'eFG%'
  ) %>% 
  tab_source_note(
    source_note = md("SOURCE: NBA.COM - TABLE: @ED_VERGANI - REF: @THOMAS_MOCK")
  ) %>% 
  text_transform(
    locations = cells_body(urlPlayerHeadshot),
    fn = function(x) {
      web_image(url = x, height = 35)
    }
  ) %>%
  gt_theme_538(table.width = px(1200))

gtsave(tab_538, filename = "outputs/guarded_shooting.png")


passer <- dunks %>%
  group_by(passer_name, passer_id) %>%
  summarise(
    dunks_assisted = n(),
    avg_pass_length = mean(pass_length, na.rm = TRUE)
  )%>%
  filter(dunks_assisted>=10 & passer_id != 0)

passer = merge(passer, image_df, by.x = "passer_id", by.y = "idPlayer")

pass_table <- passer %>%
  arrange((-avg_pass_length)) %>%
  head(15) %>%
  mutate(dunks_assisted = round(dunks_assisted, 1), 
         avg_pass_length  = round(avg_pass_length , 1)) %>%
  select(urlPlayerHeadshot, passer_name,  dunks_assisted, avg_pass_length) %>%
  gt()  %>%
  tab_header(
    title = md("**Alley-oop pass masters**"),
    subtitle = md("Players with 10 or more dunks assisted, NBA 2024-25")
  ) %>% 
  data_color(
    columns = vars(avg_pass_length),
    colors = scales::col_numeric(
      palette = c("white", "#cc0000"),
      domain = NULL
    )
  ) %>% 
  cols_label(
    urlPlayerHeadshot = '', 
    passer_name = '',
    dunks_assisted = 'DUNKS ASSISTED', 
    avg_pass_length = 'AVG PASS LENGTH (FT)', 
  ) %>% 
  tab_source_note(
    source_note = md("SOURCE: NBA.COM <br> TABLE: @ED_VERGANI <br> REF: @THOMAS_MOCK")
  ) %>% 
  text_transform(
    locations = cells_body(urlPlayerHeadshot),
    fn = function(x) {
      web_image(url = x, height = 35)
    }
  ) %>%
  gt_theme_538(table.width = px(400))

gtsave(pass_table, filename = "DunksPassers.png")

most_frequent <- dunks %>%
  group_by(passer_name, passer_id, player_name, player_id) %>%
  summarise(
    dunks_assisted = n(),
    avg_pass_length = mean(pass_length, na.rm = TRUE),
    avg_player_vertical = mean(player_vertical, na.rm = TRUE)
  )%>%
  filter(dunks_assisted>=10 & passer_id != 0)

most_frequent = merge(most_frequent, image_df, by.x = "passer_id", by.y = "idPlayer")
most_frequent = merge(most_frequent, image_df, by.x = "player_id", by.y = "idPlayer")

duo_table <- most_frequent %>%
  arrange((-dunks_assisted)) %>%
  head(15) %>%
  mutate(dunks_assisted = round(dunks_assisted, 1), 
         avg_pass_length  = round(avg_pass_length , 1),
         avg_player_vertical = round(avg_player_vertical, 1)) %>%
  select(urlPlayerHeadshot.x, passer_name,, urlPlayerHeadshot.y, player_name, dunks_assisted, avg_pass_length, avg_player_vertical) %>%
  gt()  %>%
  tab_spanner(
    label = "AVERAGE DUNK STATS",
    columns = 6:7
  ) %>% 
  tab_header(
    title = md("**Alley-oop duos!**"),
    subtitle = md("Duos with 10 or more dunks in passer-dunker combo, NBA 2024-25")
  ) %>% 
  data_color(
    columns = vars(dunks_assisted),
    colors = scales::col_numeric(
      palette = c("white", "#cc0000"),
      domain = NULL
    )
  ) %>% 
  data_color(
    columns = vars(avg_pass_length),
    colors = scales::col_numeric(
      palette = c("white", "blue"),
      domain = NULL
    )
  ) %>% 
  data_color(
    columns = vars(avg_player_vertical),
    colors = scales::col_numeric(
      palette = c("white", "orange"),
      domain = NULL
    )
  ) %>% 
  cols_label(
    urlPlayerHeadshot.x = '', 
    passer_name = 'FROM...',
    urlPlayerHeadshot.y = '', 
    player_name = '...TO',
    dunks_assisted = 'DUNKS ASSISTED', 
    avg_pass_length = 'PASS LENGTH (FT)', 
    avg_player_vertical = 'VERTICAL (IN)'
  ) %>% 
  tab_source_note(
    source_note = md("SOURCE: NBA.COM <br> TABLE: @ED_VERGANI <br> REF: @THOMAS_MOCK")
  ) %>% 
  text_transform(
    locations = cells_body(urlPlayerHeadshot.x),
    fn = function(x) {
      web_image(url = x, height = 35)
    }
  ) %>%
  text_transform(
    locations = cells_body(urlPlayerHeadshot.y),
    fn = function(x) {
      web_image(url = x, height = 35)
    }
  )   %>% 
  gt_theme_538(table.width = px(650))

gtsave(duo_table, filename = "DunksDuos.png")


scores <- dunks %>%
  group_by(player_name, player_id) %>%
  summarise(
    dunks = n(),
    dunk_score = mean(dunk_score, na.rm = TRUE),
    jump_subscore = mean(jump_subscore, , na.rm = TRUE),
    power_subscore = mean(power_subscore, na.rm = TRUE),
    style_subscore = mean(style_subscore, na.rm = T),
    defensive_contest_subscore = mean(defensive_contest_subscore , na.rm = TRUE),
  )%>%
  filter(dunks>=30)

scores = merge(scores, image_df, by.x = "player_id", by.y = "idPlayer")


scores_table <- scores %>%
  arrange((-dunk_score)) %>%
  head(13) %>%
  mutate(dunk_score = round(dunk_score, 1), 
         jump_subscore  = round(jump_subscore , 1), 
         power_subscore = round(power_subscore, 1),
         style_subscore = round(style_subscore, 1),
         defensive_contest_subscore = round(defensive_contest_subscore, 1)
  ) %>%
  select(urlPlayerHeadshot, player_name, dunks, dunk_score, jump_subscore, power_subscore, style_subscore, defensive_contest_subscore) %>%
  gt()  %>%
  tab_spanner(
    label = "AVERAGE DUNK SCORES",
    columns = 4:8
  ) %>% 
  tab_header(
    title = md("**Dunk Wars: The Ultimate Year-Round Dunk Contest!**"),
    subtitle = md("Players with 30 or more dunks, scores via NBA website")
  ) %>% 
  tab_style(
    style = list(
      cell_text(weight = "bold")
    ),
    locations = cells_body(
      columns = 4
    )
  ) %>%
  tab_style(
    style = list(
      cell_text(weight = "bold")
    ),
    locations = cells_body(
      columns = jump_subscore,
      rows = jump_subscore > mean(jump_subscore)*1.2
    )
  )%>%
  tab_style(
    style = list(
      cell_text(weight = "bold")
    ),
    locations = cells_body(
      columns = power_subscore,
      rows = power_subscore > mean(power_subscore)*1.2
    )
  )%>%
  tab_style(
    style = list(
      cell_text(weight = "bold")
    ),
    locations = cells_body(
      columns = style_subscore,
      rows = style_subscore > mean(style_subscore)*1.2
    )
  )%>%
  tab_style(
    style = list(
      cell_text(weight = "bold")
    ),
    locations = cells_body(
      columns = defensive_contest_subscore,
      rows = defensive_contest_subscore > mean(defensive_contest_subscore)*1.2
    )
  )%>%
  data_color(
    columns = vars(dunk_score),
    colors = scales::col_numeric(
      palette = c("white", "#d7191c"),
      domain = NULL
    )
  )%>% 
  cols_label(
    urlPlayerHeadshot = '', 
    player_name = '',
    dunks = 'DUNKS', 
    dunk_score = 'DUNK SCORE', jump_subscore = 'JUMP', power_subscore = 'POWER', style_subscore = 'STYLE', defensive_contest_subscore = ' DEF. CONTEST'
  ) %>% 
  tab_source_note(
    source_note = md("SOURCE: NBA.COM - TABLE: @ED_VERGANI - REF: @THOMAS_MOCK")
  ) %>% 
  text_transform(
    locations = cells_body(urlPlayerHeadshot),
    fn = function(x) {
      web_image(url = x, height = 35)
    }
  ) %>%
  gt_theme_538(table.width = px(550))

gtsave(scores_table, filename = "DunksScores.png")
