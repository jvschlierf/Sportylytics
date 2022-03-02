#clear the environment 
rm(list=ls()) 
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

#devtools::install_github("JaseZiv/worldfootballR")

library(worldfootballR)
mapped_players <- player_dictionary_mapping()

players = mapped_players[1:50,"PlayerFBref"]
numb = dim(players)[1]

df_players = data.frame()
df_keepers = data.frame()
df_mark_value = data.frame()

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
    new_player_TRM$player_valuation = as.integer(unlist(c(new_player_TRM[,"player_valuation"])))
    new_player_TRM = new_player_TRM[,c("player_name","player_valuation")]
    
    #Append the dataframes (firstly the one for VALUES, then for PLAYERS, basing on position)
    df_mark_value = rbind(df_mark_value, new_player_TRM)
    if (isTRUE(unlist(c(new_player_FB[2,"Versus"]))=="Goalkeepers") == TRUE){
      df_keepers = rbind(df_keepers, players_data)
    } else {
      df_players = rbind(df_players, players_data)
    }
  }, error = function(e){})
}

#Reindexing to merge the datasets
rownames(df_mark_value) = df_mark_value$player_name

#Finalizing players by merging the two datasets on players names and deleting extra row
df_players = merge(df_players, df_mark_value, by.x = 0, by.y = 0)
df_players = df_players[,c(which(colnames(df_players)=="player_name"),which(colnames(df_players)!="player_name"))]
df_players = subset(df_players, select = -c(Row.names))

#Finalizing keepers by merging the two datasets on players names and deleting extra row
df_keepers = merge(df_keepers, df_mark_value, by.x = 0, by.y = 0)
df_keepers = df_keepers[,c(which(colnames(df_keepers)=="player_name"),which(colnames(df_keepers)!="player_name"))]
df_keepers = subset(df_keepers, select = -c(Row.names))

#Saving the two datasets
write.csv(df_players,'df_players.csv')
write.csv(df_keepers,'df_keepers.csv')