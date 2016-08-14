# Obtains box score information for every game in the 2015-2016 NBA season from
# ESPN.com, saving two csv files: one containing team information and the other
# containing player information.
# 
# Instructions: Run the full script.

library(httr)
library(rvest)
library(stringr)

source('get_data.R')

# Stops evaluation if a warning occurs.
options(warn=2)

# Initiate matricies to store the box score results .
team.results             = matrix(NA, nrow=1230 * 2,      ncol=20)
player.results           = matrix(NA, nrow=1230 * 2 * 15, ncol=26)

colnames(team.results)   = c('GameID', 'Team', 'Opponent', 'Home', 'Win',                              'FGM', 'FGA', '3PM', '3PA', 'FTM', 'FTA', 'OREB', 'DREB', 'REB', 'AST', 'STL', 'BLK', 'TO', 'PF', 'PTS') 
colnames(player.results) = c('GameID', 'Team', 'Opponent', 'Home', 'Win', 
                             'Player', 'Position', 'Starter', 'MIN', 'FGM', 
                             'FGA', '3PM', '3PA', 'FTM', 'FTA', 'OREB', 'DREB', 
                             'REB', 'AST', 'STL', 'BLK', 'TO', 'PF', 'PM', 
                             'PTS', 'DNP') 

# Defines the game ids used in the ESPN urls.
game.ids = 400827888:400829117

# Two games were postponed and thus their box score information was moved to
# the pages of the reschduled ids.
postponed   = c(400828541, 400828543)
rescheduled = c(400866670, 400861758)

team.results.index   = 1
player.results.index = 1

# Loops through all games.
for (i in 1:length(game.ids)) {
  
  id = game.ids[i]

  # Prints the progress of the for loop every 100 iterations.
  if (i %% 100 == 0){
    print(paste0('Iteration ', i, ' of ', 1230))
    save(team.data.frame,   file='2015_2016_NBA_box_score_team_data.RData')
    save(player.data.frame, file='2015_2016_NBA_box_score_player_data.RData')
  }

  # Determines if the current game was postponed and, if so, determines the 
  # rescheduled game id.
  if (id %in% postponed){
     postponed.index = which(postponed == id)
     id = rescheduled[postponed.index]
  }

  url  = paste("http://espn.go.com/nba/boxscore?gameId=", id, sep='')
  game = read_html(url)

  # Calls the get_data.R function to obtain the box score information for the
  # given game.
  game.data = get_data(game, id)
  
  # Determines how many players information was obtained for.
  number.of.players = nrow(game.data[[2]])

  # Add the current game information to the infromation matricies.
  team.results[  team.results.index  :(team.results.index   + 1)                , ]     = game.data[[1]]
  player.results[player.results.index:(player.results.index + number.of.players - 1), ] = game.data[[2]]
  
  team.results.index   = team.results.index   + 2
  player.results.index = player.results.index + number.of.players
  
  # Pauses for 1 second before continuing to the next game.
  Sys.sleep(1)
}

# Removes and unused rows from the initiated matricies.
if (team.results.index < nrow(team.results)){
  team.results = team.results[1:(team.results.index - 1), ]
}
if (player.results.index < nrow(player.results)){
  player.results = player.results[1:(player.results.index - 1), ]
}

# Convert the matricies to data frames.
team.data.frame   = as.data.frame(team.results)
player.data.frame = as.data.frame(player.results)

# Save the data frames as csv files.
write.csv(team.data.frame,   file='2015_2016_NBA_box_score_team_data.csv')
write.csv(player.data.frame, file='2015_2016_NBA_box_score_player_data.csv')
