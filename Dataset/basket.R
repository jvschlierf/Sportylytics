#clear the environment 
rm(list=ls()) 
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

#SET OF FUNCTIONS
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
get_season <- function(year, tabella) {
  newest_year <- 1 + as.numeric(format(Sys.Date(), "%Y"))
  if (year < 1947 | year > newest_year) {
    stop("Data are only available after 1947 and up to the present.")
  }
  url <- paste0("http://www.basketball-reference.com/leagues/NBA_",
                year, "_", tabella, ".html")
  
  html <- xml2::read_html(url)
  node <- rvest::html_node(html, "table")
  table <- rvest::html_table(node, header = TRUE)
  d <- parse_season_table(table)
  d$start_year <- year - 1
  d$end_year <- year
  d
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

#FIRST DATASET: IN-GAME DATA + ADVANCED STATISTICS
stagione_2021_pergame <- get_season(2021, "per_game")
stagione_2021_advanced <- get_season(2021, "advanced")
stagione_2021_advanced$mp <- NULL
stagione_2021_advanced$c_pctna_pct_pctna_pct_pctna_pct_pctna_pct_pctna_pct_pctna_pct_pctna_pct_pctna_pct_pctna_pct_pctna_pct_pctna_pct_pctna_pct_pctna_pct_pctna_pct_pctna_pct_pct <- NULL
stagione_2021_advanced$c_pctna_pct_pctna_pct_pctna_pct_pctna_pct_pctna_pct_pctna_pct_pctna_pct_pctna_pct_pctna_pct_pctna_pct_pctna_pct_pctna_pct_pctna_pct_pctna_pct_pctna_pct_pct_pct1 <- NULL
players = unique(stagione_2021_pergame$player)

dataset_giocatori_PG = data.frame()
dataset_giocatori_A = data.frame()

for (i in 1:(length(players))) {
  if (dim(stagione_2021_pergame[stagione_2021_pergame$player == players[i],])[1] == 1){
    dataset_giocatori_PG = rbind(dataset_giocatori_PG, stagione_2021_pergame[stagione_2021_pergame$player == players[i],])
    dataset_giocatori_A = rbind(dataset_giocatori_A, stagione_2021_advanced[stagione_2021_advanced$player == players[i],])
  } else {
    dataset_giocatori_PG = rbind(dataset_giocatori_PG, subset(stagione_2021_pergame[stagione_2021_pergame$player == players[i],], tm == "TOT"))
    dataset_giocatori_A = rbind(dataset_giocatori_A, subset(stagione_2021_advanced[stagione_2021_advanced$player == players[i],], tm == "TOT"))
  }
}

total1 <- merge(dataset_giocatori_PG,dataset_giocatori_A,by=c("player","pos","age","tm","g","start_year","end_year"))

#SECOND DATASET: PLAYER COMPENSATION
df <- data.frame()

# get webpage
url <- paste0("https://www.basketball-reference.com/contracts/players.html")
webpage <- read_html(url)

# get column names
col_names <- webpage %>% 
  html_nodes("table#player-contracts > thead > tr > th") %>% 
  html_attr("data-stat")    
col_names <- col_names[4:(length(col_names))]
col_names = col_names[col_names != "ranker"]

# extract all columns (except date)
data <- webpage %>% 
  html_nodes("table#player-contracts > tbody > tr > td") %>% 
  html_text() %>%
  matrix(ncol = length(col_names), byrow = TRUE)

# combine game IDs, dates and columns in dataframe for this month, add col names
month_df <- as.data.frame(cbind(data), stringsAsFactors = FALSE)
names(month_df) <- col_names

#Rearrange things
month_df = month_df[,c("player", "y1")]
month_df = unique(month_df)
month_df = subset(month_df, nchar(unlist(y1)) > 0)
names(month_df)[names(month_df) == 'y1'] <- 'Player Salary 2021'

### MERGING EVERYTHING NOW

total_dataset_players = merge(total1, month_df,by=c("player"))
write.csv(total_dataset_players, "basket_players.csv")