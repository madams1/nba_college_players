
getPlayerCollegeStats <- function(player_url) {

    base_uri <- "http://basketball-reference.com"
    
    current_session <- html_session(paste0(base_uri, player_url))
    
    ncaa_stat_link <- current_session %>%
        html_nodes("p.padding_bottom_half a") %>% html_attrs %>% unlist %>%
        .[str_detect(., "sports-reference.com/cbb")]
    
    # check if ncaa link actually exists
    if (length(ncaa_stat_link) != 0) {
    
        ncaa_per_game_table <- current_session %>%
            jump_to(ncaa_stat_link) %>%
            html %>% html_node("#players_per_game") %>%
            html_table(header = TRUE)
        
        # NCAA conference
        conference <- ncaa_per_game_table[nrow(ncaa_per_game_table) - 1, 3]
        
        ncaa_career_stats <- ncaa_per_game_table %>%
            select(4:ncol(ncaa_per_game_table)) %>%
            .[nrow(ncaa_per_game_table), ] %>% as_data_frame
        
        # function to clean up and distinguish column names for ncaa per game stats 
        cleanColNames <- function(col_names) {
            all_names <- paste0("ncaa_", tolower(col_names)) %>%
                str_replace_all("\\%", "_perc")
            
            c(all_names[1], paste0(all_names[2:length(all_names)], "_per_game"))
        }
        
        colnames(ncaa_career_stats) <- cleanColNames(colnames(ncaa_career_stats))
            
        
        # TODO: get advanced stats
        
        ncaa_career_stats %>% mutate(ncaa_conference = conference, player_url = player_url)
    }

}