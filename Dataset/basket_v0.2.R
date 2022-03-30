#clear the environment 
rm(list=ls()) 
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

listofpackages <- c("quantmod","PerformanceAnalytics","ellipse","reshape2","ggplot2", "rvest",
                    "dygraphs", "dplyr","forecast", "aod","readr","rvest","lubridate", "xml2", "bbr")

#Uploading libraries
for (j in listofpackages){
  if(sum(installed.packages()[, 1] == j) == 0) {
    install.packages(j)
  }
  library(j, character.only = T)
}

###RETRIEVE SLUGS
diz = c("A","B","C","D","E","F","G","H","I","J","K","L","M",
        "N","O","P","Q","R","S","T","U","V","W","X","Y","Z")

#Build first dataframe
data_slug = data.frame()

#Remove basketball players that started before 2000
for (i in 1:(length(diz))){
  pla = get_players(diz[i])
  pla = subset(pla, from >= 2000)
  pla = pla[,c("player","slug")]
  data_slug <- rbind(data_slug, pla)
}

###SET OF CRUCIAL FUNCTIONS
maybe_as_numeric <- function(x) {
  # tries to make numeric columns numeric (from char)
  numeric_x <- suppressWarnings(as.numeric(x))
  if (!all(is.na(numeric_x))) x <- numeric_x
  x
}
empty_string_to_na <- function(x) {
  # sometimes (especially old datasets), empty values are ""
  if (class(x) == "character") {
    res <- ifelse(x == "", NA, x)
  } else {
    res <- x
  }
  res
}
clean_colnames <- function(df) {
  # clean up column names for a data frame
  stopifnot(is.data.frame(df))
  df <- df[!(names(df) == "Rk")] # remove "Rank" column
  names(df) <- gsub("\\.", "_pct", names(df))
  names(df) <- gsub("X2", "two_", names(df))
  names(df) <- gsub("X3", "three_", names(df))
  names(df) <- tolower(names(df))
  df
}
parse_season_table <- function(table) {
  duplicated_header_rows <- table$Rk == "Rk"
  table <- table[!duplicated_header_rows, ]
  converted <- lapply(table, maybe_as_numeric)
  converted <- lapply(converted, empty_string_to_na)
  df <- as.data.frame(converted, stringsAsFactors = FALSE)
  df <- clean_colnames(df)
  df
}
#Most important one to scrape players
get_player_data_pvt <- function(Nome_Giocatore, slug, tabella) {
  stopifnot(is.character(slug))
  initial <- substr(slug, 1, 1)
  url <- paste0("http://www.basketball-reference.com/players/",
                initial, "/", 
                slug, '.html')
  html <- xml2::read_html(url)
  node <- rvest::html_node(html, paste0("table#", tabella))
  table <- rvest::html_table(node, header = TRUE)
  table$slug <- slug
  
  player_name_node <- rvest::html_node(html, 
                                       '#footer_header > 
                                       div:nth-child(2) > 
                                       span:nth-child(4) > 
                                       strong:nth-child(1) > 
                                       span:nth-child(1)')
  player_name <- rvest::html_text(player_name_node)
  table$player <- gsub("\\*", "", player_name)
  
  converted <- lapply(table, empty_string_to_na)
  converted <- as.data.frame(converted, stringsAsFactors = FALSE)
  
  # ensure player name is the first column
  num_cols <- ncol(converted)
  reordered_df <- converted[, c(num_cols, 1:(num_cols - 1))]
  reordered_df <- clean_colnames(reordered_df)
  
  # strip out summary rows
  career_row <- which(reordered_df$season == 'Career')
  clean_df <- reordered_df[-c(career_row:nrow(reordered_df)), ]
  
  stats_sal <- url %>%
    read_html %>%
    html_nodes(xpath = '//comment()') %>%
    html_text() %>%
    paste(collapse='') %>%
    read_html() %>%
    html_node('table#all_salaries') %>%
    html_table()
  
  final_tab = merge(clean_df, stats_sal[,c("Season", "Salary")], by.x = "season", by.y = "Season")
  final_tab[,c("slug", "player")] <- NULL
  final_tab$Player = Nome_Giocatore
  final_tab <- final_tab %>% relocate(Player, .before = season)
  final_tab
}
#Function to keep only TOT row (when a player played for more than a team in a season)
funz_TOT <- function(dataset,colonna1, colonna2, valore){
  new_dataset = data.frame()
  for (i in unique(dataset[,c(colonna1)])){
    if (dim(subset(aa, dataset[,c(colonna1)] == i))[1] == 1){
      new_dataset = rbind(new_dataset, subset(dataset, dataset[,c(colonna1)] == i))
    } else {
      new_dataset = rbind(new_dataset, subset(dataset, dataset[,c(colonna1)] == i & dataset[,c(colonna2)] == valore))
    }
  }
  new_dataset
}

###START SCRAPING

#Useful for range(len(...))
ran = dim(data_slug)[1]

#Create dataframe
data_giocat_totale = data.frame()

#Start the scrape
for (i in 1:ran){
  
  tryCatch({
    
    #Take per-game statistics and advanced statistics for each player
    aa = get_player_data_pvt(data_slug[i,c("player")],data_slug[i,c("slug")],"advanced")
    bb = get_player_data_pvt(data_slug[i,c("player")],data_slug[i,c("slug")],"per_game")
    
    #Take only TOT values for season with more than a club  
    aa_1 = funz_TOT(aa, "season", "tm", "TOT")
    bb_1 = funz_TOT(bb, "season", "tm", "TOT")
    #Take the largest salary in season with more than one club
    aa_1 <- merge(aa_1, aggregate(data = aa_1, Salary~season, FUN = max), by=c("season","Salary"))
    bb_1 <- merge(bb_1, aggregate(data = bb_1, Salary~season, FUN = max), by=c("season","Salary"))
    
    #Remove total points per season, you already have the per-game one
    aa_1$mp <- NULL
    
    #Remove problematic columns from advanced statistics dataset
    col_n = colnames(aa_1)
    col_n = col_n[sapply(col_n , nchar) > 30]
    for (i in 1:(length(col_n))){
      aa_1[,c(col_n[i])] <- NULL
    }
    
    #Merge the two dataset of per_game and advanced statistics
    total1 <- merge(aa_1, bb_1,by=c("Player","season","age","tm","lg","pos","g","Salary"))
    data_giocat_totale <- rbind(data_giocat_totale, total1)
  
  }, error = function(e){})

}