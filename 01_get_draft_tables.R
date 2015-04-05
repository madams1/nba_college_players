# function to get nba draft tables by year from basketball-reference

getCollegiateDrafted <- function(y) {
    
    # get the draft page for the year y
    draft_tree <- html(paste0("http://www.basketball-reference.com/draft/NBA_", y, ".html"))
    
    # get hrefs from the table -- html_table() doesn't preserve the hrefs
    player_rows <- draft_tree %>% html_nodes(xpath = "//tbody/tr/td[4]") %>% html_node("a")
    missing_link_rows <- which(sapply(player_rows, is.null)) # rows with missing links
    player_links <- draft_tree %>% html_nodes(xpath = "//tbody/tr/td[4]/a/@href") %>% as.character # rows with player links
    
    # hacky way of interleaving the missing values with the rows with links
    for (i in missing_link_rows) player_links %<>% append(NA, i - 1) # create vector of links to ncaa page
    
    # get the data from the table in a dataframe
    draft_table <- draft_tree %>%
        html_node("#stats") %>%
        html_table(header = FALSE)
    
    # make the column names more friendly
    colnames(draft_table) <- c(draft_table[2, 1:7],
                               paste0(draft_table[2, 8:14], "_overall"),
                               paste0(draft_table[2, 15:18], "_per_game"),
                               draft_table[2, 19:20]) %>%
        sapply(as.vector) %>%
        str_replace_all("\\%", "_perc") %>%
        str_replace("\\/", "_") %>% tolower
    
    # get rid of the subheaders in the middle of the table, get players that went to college
    college_players <- draft_table %>%
        mutate(draft_year = y,
               pk = as.numeric(pk)) %>%
        filter(!is.na(pk)) %>%
        select(draft_year, 2:ncol(draft_table)) %>%
        mutate(player_url = player_links) %>%
        filter(college != "")
    
    # treat the numeric columns nicely
    stat_indices <- 6:(ncol(college_players) - 1)
    colnames(college_players)[stat_indices] <- paste0("nba_", colnames(college_players)[stat_indices])
    college_players[, stat_indices] %<>% sapply(as.numeric)
    
    # return the cleaned dataframe
    college_players
}
