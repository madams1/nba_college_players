require(dplyr)
require(tidyr)
require(rvest)
require(magrittr)
require(stringr)

# functions for getting player data
source("01_get_draft_tables.R")
source("02_get_player_attributes.R")
source("03_get_ncaa_stats.R")

# get 25 years worth of career NBA stats for collegiate athletes drafted
nba_draft_stats <- lapply(1982:2006, getCollegiateDrafted) %>%
    bind_rows %>%
    arrange(desc(nba_ws))

# get the urls for the players that have them
player_urls <- nba_draft_stats$player_url[!is.na(nba_draft_stats$player_url)]

# get player attributes -- takes some time
player_attrs <- lapply(player_urls, getPlayerAttributes) %>% bind_rows

# get player ncaa stats -- takes some time
player_ncaa_stats <- lapply(player_urls, getPlayerCollegeStats) %>% bind_rows

# populate database with tables
db <- src_postgres("nba_college_players", host = "192.168.11.5")

copy_to(db, nba_draft_stats, temporary = FALSE)
copy_to(db, player_attrs, temporary = FALSE)
copy_to(db, player_ncaa_stats, temporary = FALSE)
