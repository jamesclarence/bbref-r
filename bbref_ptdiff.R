# NBA Team Season Comparison using Basketball Reference

# Compare point differential by game from season to season
# X axis: Games (1-82)
# Y axis: Point Differential

# Load packages
library(tidyverse)
library(rvest)
library(janitor)
  

# Function - Get Team Game Log by Season ------------------------------------

get_team_game_log_by_season <- function(team, season){
  webpage <- paste0("https://www.basketball-reference.com/teams/", team,"/", season, "/gamelog/")
  webpage_html <- read_html(webpage)
  table_html <- html_table(webpage_html)
  data.frame(table_html) %>% 
    row_to_names(row_number = 1) %>% 
    clean_names() %>%
    remove_empty("cols") %>% 
    rename_with(~ gsub("_2", "_opp", .x), ends_with("_2")) %>%
    rename(home_away = x,
           pt_opp = opp_opp,
           pt_tm = tm) %>% 
    mutate(home_away =if_else(home_away == "@", "away", "home")) %>% 
    filter(g != "G" & g != "")
}


# Function - Chart for One Year/One Team Cumulative Point Differential ---------
# https://www.basketball-reference.com/teams/ATL/2021/gamelog/
# f("ATL", 2021)

chart_pt_diff_one_year_one_team <- function(t, s1) {
  t21_df <- get_team_game_log_by_season(t, s1)
  
  # New variable for point differential
  t21_chart <- t21_df %>% 
    mutate(pt_diff = as.numeric(pt_tm)-as.numeric(pt_opp)) %>% 
    drop_na(pt_diff) %>% 
    mutate(pt_diff_cumsum = cumsum(pt_diff), g = as.integer(g), season = 2021) %>% 
    select(season, g, pt_diff, pt_diff_cumsum)
  
  # Final Chart
  t21_chart %>%
    ggplot(aes(g, pt_diff_cumsum, group = 1)) +
      geom_step() +
      labs(
        title = paste(t, ": Point Differential for", s1, "Season")
      ) +
      theme_minimal() 
}
 

# Function: Chart for One Team/Two Years Cumulative Point Differential ---------
# f("ATL", 2021, 2022)

chart_pt_diff_two_years_one_team <- function(t, s1, s2) {
  # First Season s1
  # https://www.basketball-reference.com/teams/CLE/2021/gamelog/
  t1_df <- get_team_game_log_by_season(t, s1)
  
  # New variable for point differential
  t1_chart <- t1_df %>% 
    mutate(pt_diff = as.numeric(pt_tm)-as.numeric(pt_opp)) %>% 
    drop_na(pt_diff) %>% 
    mutate(pt_diff_cumsum = cumsum(pt_diff), g = as.integer(g), season = s1) %>% 
    select(season, g, pt_diff, pt_diff_cumsum)
  
  # Second Season s2
  # https://www.basketball-reference.com/teams/CLE/2022/gamelog/
  t2_df <- get_team_game_log_by_season(t, s2)
  
  # New variable for point differential
  t2_chart <- t2_df %>% 
    mutate(pt_diff = as.numeric(pt_tm)-as.numeric(pt_opp)) %>% 
    drop_na(pt_diff) %>% 
    mutate(pt_diff_cumsum = cumsum(pt_diff), g = as.integer(g), season = s2) %>% 
    select(season, g, pt_diff, pt_diff_cumsum)
  
  # Bind season 1 and season 2
  t1t2 <- bind_rows(t1_chart, t2_chart)
  
  # Chart
  t1t2 %>% 
    ggplot(aes(g, pt_diff_cumsum, group = season, color = as.character(season))) +
      geom_step() + 
      labs(
        title = paste0(t,": ", "Point Differential for ", s1, " and ", s2, " Seasons"),
        x = "Games",
        y = "Point Differential",
        color = "Seasons"
      ) +
      theme_minimal() +
      scale_x_continuous(breaks = seq(0,82,5))
}

