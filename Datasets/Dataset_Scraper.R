#clear the environment 
rm(list=ls()) 
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

#Library for scraping data from FBref.com
#devtools::install_github("JaseZiv/worldfootballR")
library(worldfootballR)

#Saving as a dataframe the data regarding FBref & Transfermarkt links for every player
mapped_players <- player_dictionary_mapping()

#Taking only first 100 players
players = mapped_players[1:200,"PlayerFBref"]
numb = dim(players)[1]

#Initializing our dataframes
df_players = data.frame()
df_keepers = data.frame()

for (i in (1:numb)){
  tryCatch({
    
    #First Step, select the player name
    player_FB = unlist(c(mapped_players[i,"UrlFBref"]))
    player_TRM = unlist(c(mapped_players[i,"UrlTmarkt"]))
    
    #Now scrape both from FBref and Transfermarkt
    new_player_FB = fb_player_scouting_report(player_url = player_FB, pos_versus = "primary")
    new_player_TRM = tm_player_bio(player_url = player_TRM)
    
    #Select FBref data for the last 365 days
    players_data = subset(new_player_FB, scouting_period == "Last 365 Days")
    
    #Transpose the dataframe
    players_data = data.frame(t(players_data))
    
    #Put the Statistic row as header
    names(players_data) = c(players_data["Statistic",])
    
    #Select the statistics
    players_data = data.frame(players_data["Per90",])
    
    #Change index
    rownames(players_data) <- unlist(c(mapped_players[i,"PlayerFBref"]))
    
    #Add the TMK Value as a new column and reduce the dataset
    players_data$Player_valuation = as.integer(unlist(c(new_player_TRM[,"player_valuation"])))
    
    #Append the dataframes (firstly the one for VALUES, then for PLAYERS, basing on position)
    if (isTRUE(dim(players_data)[2] >= 2) == TRUE){
      if (isTRUE(unlist(c(new_player_FB[2,"Versus"]))=="Goalkeepers") == TRUE){
        df_keepers = rbind(df_keepers, players_data)
        } else {
        df_players = rbind(df_players, players_data)
        }
    } else {next}
  }, error = function(e){})
}

#Making Index the first column (Player Name)
df_players = cbind(Player_Names = rownames(df_players), df_players)
rownames(df_players) = 1:nrow(df_players)

df_keepers = cbind(Keeper_Names = rownames(df_keepers), df_keepers)
rownames(df_keepers) = 1:nrow(df_keepers)

#Saving the two datasets
write.csv(df_players,'df_players.csv')
write.csv(df_keepers,'df_keepers.csv')