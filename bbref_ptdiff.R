# NBA Team Season Comparison using Basketball Reference

# Compare point differential by game from season to season
# X axis: Games (1-82)
# Y axis: Point Differential

# Load packages
library(tidyverse)
library(rvest)
library(janitor)
  
# Function - One Year/One Team --------------------------------------------
f <- function(t, s1) {
  # 2021 game results
  # https://www.basketball-reference.com/teams/CLE/2021/gamelog/
  webpage <- paste0("https://www.basketball-reference.com/teams/", t,"/", s1, "/gamelog/")
  t21_webpage <- read_html(webpage)
  t21_table <- html_table(t21_webpage)
  t21_df <- data.frame(t21_table) %>% 
    row_to_names(row_number = 1) %>% 
    clean_names()
  
  # New variable for point differential
  t21_chart <- t21_df %>% 
    mutate(pt_diff = as.numeric(tm)-as.numeric(opp_2)) %>% 
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
 

# Function: One Team/Two Years --------------------------------------------
f2 <- function(t, s1, s2) {
  # First Season s1
  # https://www.basketball-reference.com/teams/CLE/2021/gamelog/
  webpage1 <- paste0("https://www.basketball-reference.com/teams/", t,"/", s1, "/gamelog/")
  t1_webpage <- read_html(webpage1)
  t1_table <- html_table(t1_webpage)
  t1_df <- data.frame(t1_table) %>% 
    row_to_names(row_number = 1) %>% 
    clean_names()
  
  # New variable for point differential
  t1_chart <- t1_df %>% 
    mutate(pt_diff = as.numeric(tm)-as.numeric(opp_2)) %>% 
    drop_na(pt_diff) %>% 
    mutate(pt_diff_cumsum = cumsum(pt_diff), g = as.integer(g), season = s1) %>% 
    select(season, g, pt_diff, pt_diff_cumsum)
  
  # Second Season s2
  # https://www.basketball-reference.com/teams/CLE/2022/gamelog/
  webpage2 <- paste0("https://www.basketball-reference.com/teams/", t,"/", s2, "/gamelog/")
  t2_webpage <- read_html(webpage2)
  t2_table <- html_table(t2_webpage)
  t2_df <- data.frame(t2_table) %>% 
    row_to_names(row_number = 1) %>% 
    clean_names()
  
  # New variable for point differential
  t2_chart <- t2_df %>% 
    mutate(pt_diff = as.numeric(tm)-as.numeric(opp_2)) %>% 
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

