library(tidyverse)
library(jsonlite)
library(Matrix)
library(data.table)
library(rcartocolor)
library(extrafont)
library(ggtext)
library(ggbump)
library(systemfonts)
library(pdftools)
library("colorspace")


url="https://api.pbpstats.com/get-totals/nba?Season=2024-25&SeasonType=Regular%2BSeason&StarterState=0v3,0v4,0v5,1v3,1v4,1v5,2v4,2v3,2v5,3v3,3v4,3v5,4v3,4v4,4v5,5v3,5v4,5v5&Type=Player"

theme_ev <- function () { 
  theme_minimal(base_size=9, base_family="Archivo") %+replace% 
    theme(panel.grid = element_line(color = "#efe8e8"),
          panel.grid.minor = element_blank(),
          plot.background = element_rect(fill = '#efe8e8', color = '#efe8e8')
    )
}

starters_df <- read_json(url)
starters_df = starters_df[["multi_row_table_data"]]
starters_df=Map(as.data.frame, starters_df)
starters_df=rbindlist(starters_df, fill=TRUE)


url="https://api.pbpstats.com/get-totals/nba?Season=2024-25&SeasonType=Regular%2BSeason&StarterState=5v2,5v1,5v0,4v2,4v1,4v0,3v2,3v1,3v0,2v2,2v1,2v0,1v2,1v1,1v0,0v2,0v1,0v0&Type=Player"

reserves_df <- read_json(url)
reserves_df = reserves_df[["multi_row_table_data"]]
reserves_df=Map(as.data.frame, reserves_df)
reserves_df=rbindlist(reserves_df, fill=TRUE)

df_start=starters_df%>%
  retype()%>%
  select(Name, Minutes, OffPoss, FG2M, FG2A, Fg2Pct, FG3M, FG3A, Fg3Pct, Points, Assists, Rebounds, Steals, Turnovers, FtPoints, FTA, OnOffRtg, OnDefRtg, EfgPct, TsPct, Usage)%>%
  filter(OffPoss>=2000)
df_res=reserves_df%>%
  retype()%>%
  select(Name, Minutes, OffPoss, FG2M, FG2A, Fg2Pct, FG3M, FG3A, Fg3Pct, Points, Assists, Rebounds, Steals, Turnovers, FtPoints, FTA, OnOffRtg, OnDefRtg, EfgPct, TsPct, Usage)%>%
  filter(OffPoss>=850)
df=merge(df_start, df_res, by="Name")
df=df%>%
  rename_at(vars(ends_with(".x")), ~ str_c("Start_", .))%>%
  rename_at(vars(ends_with(".y")), ~ str_c("Res_", .))
for ( col in 1:ncol(df)){
  colnames(df)[col] <-  sub(".x", "", colnames(df)[col])
}
for ( col in 1:ncol(df)){
  colnames(df)[col] <-  sub(".y", "", colnames(df)[col])
}

df=df%>%
  mutate(dif_Fg2Pct=Start_Fg2Pct-Res_Fg2Pct, dif_Fg3Pct=Start_Fg3Pct-Res_Fg3Pct, dif_EfgPct=Start_EfgPct-Res_EfgPct,dif_TsPct=Start_TsPct-Res_TsPct, dif_Usage=Start_Usage-Res_Usage)%>%
  mutate(Start_FG2M_100=Start_FG2M/Start_OffPoss*100,
         Start_FG2A_100=Start_FG2A/Start_OffPoss*100,
         Start_FG3M_100=Start_FG3M/Start_OffPoss*100,
         Start_FG3A_100=Start_FG3A/Start_OffPoss*100,
         Start_Points_100=Start_Points/Start_OffPoss*100,
         Res_FG2M_100=Res_FG2M/Res_OffPoss*100,
         Res_FG2A_100=Res_FG2A/Res_OffPoss*100,
         Res_FG3M_100=Res_FG3M/Res_OffPoss*100,
         Res_FG3A_100=Res_FG3A/Res_OffPoss*100,
         Res_Points_100=Res_Points/Res_OffPoss*100
         )

ts_plot=df%>%
  select(Name, Start_TsPct, Res_TsPct)%>%
  mutate(
    Start_TsPct=round(Start_TsPct*100,1), Res_TsPct=round(Res_TsPct*100,1),
    rank_start = rank(Start_TsPct, ties.method = "random"),
    rank_res = rank(Res_TsPct, ties.method = "random")
  )

x <- c(seq(-127, -57, by = 10), seq(-18.5, 58.5, by = 10.5))
y <- 1:(n_distinct(ts_plot$rank_start) + 1)
lines <- crossing(x, y)

theme_set(theme_void(base_family = "Archivo"))
theme_update(
  legend.position = "none",
  plot.margin = margin(25, 35, 15, 35),
  plot.background = element_rect(fill = "#171717"),
  plot.title = element_markdown(color = "grey70", size = 24, 
                                family = "Archivo", face = "bold",
                                lineheight = 1.2),
  plot.subtitle = element_markdown(color = "grey50", size = 12,
                                   family = "Archivo",
                                   lineheight = 1.2, 
                                   margin = margin(t = 15, b = 10)),
  plot.caption = element_text(color = "grey35", size = 10,
                              margin = margin(t = 0), hjust=0.5)
)

ggplot(ts_plot) + 
  geom_sigmoid(
    aes(x = -50, xend = -25, 
        y = rank_start, yend = rank_res, 
        group = Name,
        color = rank_start,
        color = after_scale(colorspace::lighten(color, .4))), 
    alpha = .45, smooth = 8, size = 1.2
  ) +
  ## start-end points connections
  geom_point(
    aes(x = -50, y = rank_start,
        color = rank_start,
        color = after_scale(colorspace::desaturate(colorspace::lighten(color, .2), .2))), 
    size = 4,
    shape = "|"
  )+
  geom_point(
    aes(x = -25, y = rank_res,
        color = rank_start,
        color = after_scale(colorspace::desaturate(colorspace::lighten(color, .2), .2))), 
    size = 4,
    shape = "|"
  )+ 
  
  ## rounded bar plots
  geom_segment(
    aes(x = -58, xend = -Start_TsPct-58, 
        y = rank_start, yend = rank_start, 
        color = rank_start,
        color = after_scale(colorspace::lighten(color, .2))), 
    size = 3, 
    lineend = "round"
  )+ 
  geom_segment(
    aes(x = -18, xend = Res_TsPct -18, 
        y = rank_res, yend = rank_res, 
        color = rank_start,
        color = after_scale(colorspace::lighten(color, .2))), 
    size = 3, 
    lineend = "round"
  ) +
  ## add wagon separators
  geom_linerange(
    data = lines,
    aes(x = x, ymin = y - .85, ymax = y - .15),
    inherit.aes = FALSE,
    color = "#330000",
    size = .3
  ) +
  ## label costs
  geom_richtext(
    aes(x = -53.5, y = rank_start, 
        label = Start_TsPct), 
    hjust = 0.5, 
    color = "#efe8e8",
    size = 3.5,
    family = "Archivo",
    fill = NA, 
    label.color = NA
  ) +
  geom_richtext(
    aes(x = -21.5, y = rank_res, 
        label = Res_TsPct), 
    hjust = 0.5, 
    color = "#efe8e8",
    size = 3.5,
    family = "Archivo",
    fill = NA, 
    label.color = NA
  ) +
  ## labels countries
  geom_richtext(
    aes(x = -Start_TsPct-58, y = rank_start,
        label = Name),
    hjust = 1,
    color = "#efe8e8",
    size = 3,
    family = "Archivo",
    nudge_x = -2,
    fill = NA, 
    label.color = NA
  ) +
  geom_text(
    data = ts_plot,
    aes(x = Res_TsPct -17, y = rank_res, 
        label = Name), 
    hjust = 0, 
    color = "#efe8e8",
    size = 3,
    family = "Archivo",
    nudge_x = 2.5,
  ) +
  ## coord + scales
  coord_cartesian(clip = "off") +
  scale_color_continuous_diverging(palette = "Blue-Red 2", mid=30) +
  theme(
    plot.caption=element_text(color="#efe8e8", size=7),
    plot.background = element_rect(fill="#330000")
  )+
  ## title & co
  labs(title = "<b style='color:#ffb861;'>NBA</b> players <b style='color:#ffb861;'>TS%</b> against <b style='color:#f3592f;'>3 or more starters</b> and against <b style='color:#fff6a1;'>2 or less starters</b>.",
       subtitle = "Based on a selection of 63 players who played <b style='color:#ffb861;'>at least </b><b style='color:#f3592f;'>2000 offensive possessions</b> against <b style='color:#f3592f;'>3 or more starters</b> and at least <b style='color:#fff6a1;'>900</b> against <b style='color:#fff6a1;'>2 or less starters</b>.",
       caption = "@ed_vergani  •  pbp.stats.com") 

ggsave("gigaplotlong.pdf", width = 16.3, height = 9.5, device = cairo_pdf, 
       path = "C:/Users/edoar/Documents/Coding/NBA/outputs")


ggsave("gigaplottwitter.pdf", width = 16, height = 16, device = cairo_pdf, 
       path = "C:/Users/edoar/Documents/Coding/NBA/outputs")
pdf_convert(pdf = "C:/Users/edoar/Documents/Coding/NBA/outputs/gigaplottwitter.pdf", 
            filenames = "C:/Users/edoar/Documents/Coding/NBA/outputs/gigaplottwitter.png",
            format = "png", dpi = 350)

mean(ts_plot$Res_TsPct)

##########Alternative versions##########
df_alt=df%>%
  filter(Start_OffPoss>2950)%>%
  select(Name, Start_TsPct, Res_TsPct)%>%
  mutate(
    Start_TsPct=round(Start_TsPct*100,1), Res_TsPct=round(Res_TsPct*100,1),
    rank_start = rank(Start_TsPct, ties.method = "random"),
    rank_res = rank(Res_TsPct, ties.method = "random")
  )

x <- c(seq(-127, -57, by = 10), seq(-18.5, 58.5, by = 10.5))
y <- 1:(n_distinct(ts_plot$rank_start) + 1)
lines <- crossing(x, y)


theme_set(theme_void(base_family = "Roboto Condensed"))
theme_update(
  legend.position = "none",
  plot.margin = margin(25, 65, 25, 65),
  plot.background = element_rect(fill = "#171717"),
  plot.title = element_markdown(color = "grey70", size = 16, 
                                family = "ScotchW06-MicroBold", face = "bold",
                                lineheight = 1.2,
                                hjust=0.5),
  plot.subtitle = element_markdown(color = "grey50", size = 8,
                                   family = "ScotchW06-MicroBold",
                                   lineheight = 1.2,
                                   hjust=0.5),
  plot.caption = element_text(color = "grey35", size = 5,
                              margin = margin(t = 0), hjust=0.5)
)

ggplot(ts_plot) + 
  geom_sigmoid(
    aes(x = -13, xend = 2, 
        y = rank_start, yend = rank_res, 
        group = Name,
        color = rank_start,
        color = after_scale(colorspace::lighten(color, .4))), 
    alpha = .45, smooth = 8, size = 1.2
  ) +
  ## start-end points connections
  geom_point(
    aes(x = -13, y = rank_start,
        color = rank_start,
        color = after_scale(colorspace::desaturate(colorspace::lighten(color, .2), .2))), 
    size = 4,
    shape = "|"
  )+
  geom_point(
    aes(x = 2, y = rank_res,
        color = rank_start,
        color = after_scale(colorspace::desaturate(colorspace::lighten(color, .2), .2))), 
    size = 4,
    shape = "|"
  )+ 
  ## rounded bar plots
  geom_segment(
    aes(x = -16-1.1, xend = -Start_TsPct/3-16+1.1, 
        y = rank_start, yend = rank_start, 
        color = rank_start,
        color = after_scale(colorspace::lighten(color, .2))), 
    size = 3, 
    lineend = "round"
  )+ 
  geom_segment(
    aes(x = 5+1.1, xend = Res_TsPct/3 +5-1.1, 
        y = rank_res, yend = rank_res, 
        color = rank_start,
        color = after_scale(colorspace::lighten(color, .2))), 
    size = 3, 
    lineend = "round"
  ) +
  ## label costs
  geom_richtext(
    aes(x = -15, y = rank_start, 
        label = Start_TsPct), 
    hjust = 0.5, 
    color = "#efe8e8",
    size = 3.5,
    family = "Roboto Condensed",
    fill = NA, 
    label.color = NA
  ) +
  geom_richtext(
    aes(x = 4, y = rank_res, 
        label = Res_TsPct), 
    hjust = 0.5, 
    color = "#efe8e8",
    size = 3.5,
    family = "Roboto Condensed",
    fill = NA, 
    label.color = NA
  ) +
  ## labels countries
  geom_richtext(
    aes(x = -Start_TsPct/3-13, y = rank_start,
        label = Name),
    hjust = 1,
    color = "#efe8e8",
    size = 3,
    family = "Roboto Condensed",
    nudge_x = -2,
    fill = NA, 
    label.color = NA
  ) +
  geom_text(
    data = ts_plot,
    aes(x = Res_TsPct/3+2, y = rank_res, 
        label = Name), 
    hjust = 0, 
    color = "#efe8e8",
    size = 3,
    family = "Roboto Condensed",
    nudge_x = 2.5,
  ) +
  ## coord + scales
  coord_cartesian(clip = "off") +
  scale_color_continuous_diverging(palette = "Blue-Red 2", mid=30) +
  theme(
    plot.caption=element_text(color="#efe8e8", size=7),
    plot.background = element_rect(fill="#330000")
  )+
  ## title & co
  labs(title = "<b style='color:#ffb861;'>NBA</b> players <b style='color:#ffb861;'>TS%</b> against <b style='color:#f3592f;'>3 or more starters</b> and against <b style='color:#fff6a1;'>2 or less starters</b>.",
       subtitle = "Based on a selection of 63 players who played <b style='color:#ffb861;'>at least </b><b style='color:#f3592f;'>2000 offensive possessions</b> against <b style='color:#f3592f;'>3 or more starters</b> and at least <b style='color:#fff6a1;'>900</b> against <b style='color:#fff6a1;'>2 or less starters</b>.",
       caption = "@ed_vergani  •  pbp.stats.com") 

ggsave("gigaplot.pdf", width = 9.5, height = 9.5, device = cairo_pdf)
ggsave("gigaplottwitter.pdf", width = 16, height = 16, device = cairo_pdf)
pdf_convert(pdf = "gigaplot.pdf", 
            filenames = ("gigaplot.png"),
            format = "png", dpi = 350)


ggsave("gigaplottest.pdf", width = 8.5, height = 8.5, device = cairo_pdf)
display.brewer.all(colorblindFriendly = TRUE)

rm(x)
rm(y)
