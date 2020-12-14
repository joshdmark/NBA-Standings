# devtools::install_github("abresler/nbastatR")
library(tidyverse)
library(data.table)
library(sqldf)
library(nbastatR)
library(readxl)

## get league standings since 2000 season
league_standings <- nbastatR::standings(seasons = 2000:2020, season_types = 'Regular Season') %>%
  data.frame() %>% 
  mutate(slugTeam = case_when(
    nameTeam == 'LA Clippers' ~ 'LAC', 
    nameTeam == 'New Jersey Nets' ~ 'NJN', 
    nameTeam == 'Seattle SuperSonics' ~ 'SEA', 
    nameTeam == 'Vancouver Grizzlies' ~ 'VAN', 
    nameTeam == 'New Orleans Hornets' ~ 'NOH', 
    nameTeam == 'Charlotte Bobcats' ~ 'CHB', 
    nameTeam == 'New Orleans/Oklahoma City Hornets' ~ 'NOKC', 
    TRUE ~ slugTeam
  )) %>% 
  mutate(rankPlayoffs = case_when(
    yearSeason == 2000 & slugTeam == 'PHI' ~ 5, 
    yearSeason == 2000 & slugTeam == 'MIL' ~ 8,
    yearSeason == 2000 & slugTeam == 'PHX' ~ 5,
    yearSeason == 2001 & slugTeam == 'UTA' ~ 4,
    yearSeason == 2001 & slugTeam == 'DAL' ~ 5,
    TRUE ~ rankPlayoffs 
  )) %>% 
  mutate(lebron_ind = 0) %>% 
  mutate(lebron_ind = case_when(
    slugTeam == 'CLE' & yearSeason %in% 2004:2010 ~ 1, 
    slugTeam == 'MIA' & yearSeason %in% 2011:2014 ~ 1, 
    slugTeam == 'CLE' & yearSeason %in% 2015:2018 ~ 1, 
    slugTeam == 'LAL' & yearSeason %in% 2019:2020 ~ 1, 
    TRUE ~ 0
  ))

## get 2020-21 team OUs
team_ous <- readxl::read_xlsx("Documents/Sports/NBA Standings/Team OUs.xlsx")

## add over unders 
league_standings <- merge(league_standings, team_ous, 
                          by.x = 'slugTeam', by.y = 'Organization', 
                          all.x = TRUE)

## write final file
fwrite(league_standings, "Documents/Sports/NBA Standings/leauge_standings.csv")
