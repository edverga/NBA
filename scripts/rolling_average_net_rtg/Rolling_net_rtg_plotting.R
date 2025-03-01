library(teamcolors)  
library(dplyr)       
library(ggplot2)     
library(forcats)     
library(stringr)     
library(tidyr)

teamcolors <- teamcolors::teamcolors %>% filter(league == "nba") 

theme_ev <- function() { 
  theme_minimal(base_size = 15, base_family = "archivo") + 
    theme(
      panel.grid.minor = element_blank(),
      plot.background = element_rect(fill = '#efe8e8', color = "#efe8e8"),
      text = element_text(family = "archivo")
    )
}

teamcolors=teamcolors%>%
  select(name, primary, secondary)
ratings <- left_join(ratings, teamcolors, by = c("team_name" = "name")) 

ratings$team_nameDuplicate <- ratings$team_name
ratings$team_name=word(ratings$team_name,-1)


ratings <- ratings %>% 
  mutate(primary = case_when(
    team_name == "Spurs" ~ "#000000",
    team_name == "Pacers" ~ "#002D62",
    team_name == "Warriors" ~ "#1D428A",
    team_name == "Lakers" ~ "#552583",
    team_name == "76ers" ~ "#006BB6",
    team_name == "Heat" ~ "#41B6E6",
    team_name == "Grizzlies" ~ "#00B2A9",
    team_name == "Thunder" ~ "#EF3B24",
    TRUE ~ primary
  ))

p = ratings %>%
  ggplot(aes(x = g, y = n_rtg_ra)) +
  geom_smooth(data = mutate(ratings, team_name = NULL), aes(group = team_nameDuplicate),
              method = "loess", se = F, colour = "grey80", size = .25, alpha = .5, show.legend = F) +
  geom_smooth(aes(group = team_name, color = primary), method = "loess", se = F, linewidth = .5, alpha = 1, show.legend = FALSE) +
  scale_x_continuous(breaks = seq(0, 82, 20), limits = c(0, 82)) +
  scale_color_identity() +
  theme_ev() +
  geom_hline(yintercept = 0, linetype = "dashed", alpha = .35, size = .45) +
  facet_wrap(~fct_reorder(team_name, -n_rtg_ra)) +
  theme(panel.background = element_rect(fill = '#efe8e8',
                                        colour = '#efe8e8',
                                        size = 0.5, linetype = "solid"),
        axis.ticks.x=element_blank(),
        axis.ticks.y=element_blank(),
        plot.title.position = 'plot', 
        plot.title = element_text(face = 'bold', size = 80), 
        plot.subtitle = element_text(face = "italic", size = 50),
        plot.margin = margin(10, 10, 15, 10), 
        plot.caption = element_text(size = 30),
        panel.spacing = unit(0.5, 'lines'),
        panel.grid.major = element_line(colour = "#d0bbbb"),
        strip.text = element_text(size = 40),
        
        # Axis Title Size (already adjusted)
        axis.title.x = element_text(size = 30, face = "bold"),
        axis.title.y = element_text(size = 30, face = "bold"),
        
        # Tick Label Improvements
        axis.text.x = element_text(size = 30, face = "plain", angle = 0, vjust = 0.5),  # X-axis labels
        axis.text.y = element_text(size = 30, face = "plain")    )+
  labs(
    x = "Game", 
    y = "Net Rating", 
    title = "Team's Net Rating Over A 5-Game Moving Average", 
    subtitle = "Lines smoothed for readability, teams ordered by Net Rating",
    caption = "@ed_vergani | basketball-reference.com"
  )


ggsave("outputs/netratingrolling.png", p, w = 9, h = 9, dpi = 300, type = 'cairo')

o=ratings%>%
  ggplot(aes(x=g, y=o_rtg_ra))+
  geom_smooth(data=mutate(ratings, team_name=NULL), aes(group=team_nameDuplicate),
              method="loess", se=F, colour="grey80", size=.25, alpha=.5, show.legend = F)+
  geom_smooth(aes(group=team_name, color=primary), method="loess",se=F, size = .5, alpha = 1, show.legend = FALSE)+
  scale_x_continuous(breaks=seq(0, 65, 20), limits=c(0, 65, 20))+
  scale_color_identity()+
  theme_owen()+
  facet_wrap(~fct_reorder(team_name, -n_rtg_ra))+
  theme(plot.title.position = 'plot', 
        plot.title = element_text(face = 'bold', size=20), 
        plot.subtitle = element_text(face = "italic", size=12),
        plot.margin = margin(10, 10, 15, 10), 
        plot.caption = element_text(size=6),
        panel.spacing = unit(0.5, 'lines'),
        panel.grid.major = element_line(colour="#d0bbbb"),
        strip.text=element_text(size=10)) +
  labs(x = "Game", 
       y = "Off Rating", 
       title = "Team's Off Rating Over A 5-Game Moving Average", 
       subtitle = "Lines smoothed for readability!",
       caption="@ed_vergani | basketball-reference.com")

ggsave("offratingrolling.png", o, w = 6, h = 6, dpi = 300, type = 'cairo')

d=ratings%>%
  ggplot(aes(x=g, y=d_rtg_ra))+
  geom_smooth(data=mutate(ratings, team_name=NULL), aes(group=team_nameDuplicate),
              method="loess", se=F, colour="grey80", size=.25, alpha=.5, show.legend = F)+
  geom_smooth(aes(group=team_name, color=primary), method="loess",se=F, size = .5, alpha = 1, show.legend = FALSE)+
  scale_x_continuous(breaks=seq(0, 65, 20), limits=c(0, 65, 20))+
  scale_color_identity()+
  theme_ev()+
  facet_wrap(~fct_reorder(team_name, -n_rtg_ra))+
  theme(plot.title.position = 'plot', 
        plot.title = element_text(face = 'bold', size=20), 
        plot.subtitle = element_text(face = "italic", size=12),
        plot.margin = margin(10, 10, 15, 10), 
        plot.caption = element_text(size=6),
        panel.spacing = unit(0.5, 'lines'),
        panel.grid.major = element_line(colour="#d0bbbb"),
        strip.text=element_text(size=10)) +
  labs(x = "Game", 
       y = "Def Rating", 
       title = "Team's Def Rating Over A 5-Game Moving Average", 
       subtitle = "Lines smoothed for readability!",
       caption="@ed_vergani | basketball-reference.com")

ggsave("defratingrolling.png", d, w = 6, h = 6, dpi = 300, type = 'cairo')