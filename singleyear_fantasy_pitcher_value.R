library(jsonlite)
library(dplyr)
library(rvest)
library(baseballr)
library(purrr)
library(lubridate)
library(tidyverse)
library(tidyr)
library(httr)
library(gt)
library(mlbplotR)

# projection model can be changed below
url <- "https://www.fangraphs.com/api/projections?type=steamer&stats=pit&pos=&team=0&players=0&lg=all"
pitcher_data <- fromJSON(url)

pitcher_data <- pitcher_data %>%
  mutate(
    projFPTS = IP*3 - H - ER*2 + HLD*2 - BB + SO + W*2 - L*2 + SV*2, # You may change the values here if your league has different scoring
    minpos = 'P'
  )

sorted_pitcher_data <- pitcher_data %>%
  arrange(desc(projFPTS)) %>%
  select(PlayerName, Team, projFPTS)

ranked_pitcher_data <- head(sorted_pitcher_data, 50) %>%
  mutate(Rank = row_number()) %>%
  select(Rank, everything())

gt_table <- ranked_pitcher_data %>%
  gt() %>%  tab_header(
    title = "Top 50 Fantasy Pitchers for 2024",
    subtitle = "Utilized Steamer data from 12/29"
  ) %>%
  tab_style(
    style = cell_borders(
      sides = "all",
      color = "lightgray",
      weight = "px"
    ),
    locations = cells_body(columns = everything())
  ) %>%
  cols_align(
    align = "center",
    columns = everything()
  ) %>%
  cols_label(
    Rank = "Rank",
    PlayerName = "Player Name",
    Team = "Team",
    projFPTS = "Proj FPTS"
  ) %>%
  fmt_number(
    columns = c(projFPTS),
    decimals = 0
  )

print(gt_table)

gtsave(gt_table, "/Users/Mason/Desktop/rimages/fantasy2024.png", expand = 10)
