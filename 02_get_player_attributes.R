# function to get player attributes from player urls

getPlayerAttributes <- function(player_url) {
    
    base_uri <- "http://www.basketball-reference.com"
    
    # get the page that contains the player's attributes
    player_info_text <- read_html(paste0(base_uri, player_url)) %>%
        html_nodes("p.padding_bottom_half") %>% html_text
    
    # helper function to check for attribute on page
    detectPlayerAttr <- function(player_attr) {
        str_detect(tolower(player_info_text), player_attr)
    }
    
    # helper function to convert height string to inches
    getHeightInches <- function(hgt_str) {
        hgt_vec <- str_split(hgt_str, "-") %>%
            sapply(extract_numeric, USE.NAMES = FALSE)
        
        hgt_vec[1] * 12 + hgt_vec[2]
    }
    
    # function to return player attribute value based on attribute description
    getPlayerAttr <- function(player_attr = character(), custom = FALSE) {
        if (!custom) {
            attr_match <- if (player_attr == "position") {
                player_info_text %>% gsub(".*?Position: (.*?)Shoots.*", "\\1", .) %>%
                    iconv("latin1", "ASCII", sub = "")
            } else if (player_attr == "shoots") {
                player_info_text %>% gsub(".*?Shoots: (.*?)Height.*", "\\1", .)
            } else if (player_attr == "height") {
                player_info_text %>% gsub(".*?Height: (.*?)Weight.*", "\\1", .) %>%
                    getHeightInches
            } else if (player_attr == "weight") {
                player_info_text %>% gsub(".*?Weight: (.*?)Born.*", "\\1", .) %>%
                    extract_numeric
            }
            ifelse(detectPlayerAttr(player_attr), attr_match, NA) # if no match for the attribute, return NA as the value
        } else {
            player_info_text %>% gsub(player_attr, "\\1", .)
        }
    }
    
    # get the attributes
    nba_pos <- getPlayerAttr("position")
    shoots <- getPlayerAttr("shoots")
    height <- getPlayerAttr("height")
    weight <- getPlayerAttr("weight")
    
    # store the collection of attributes in a dataframe
    data_frame(player_url, nba_pos, shoots, height, weight)
    
}
