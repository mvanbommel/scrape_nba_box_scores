get_data = function(game, id){
  # Obtains team and player boxscore data for the game corresponding to the 
  # given url.
  #
  # Inputs:
  # game - a parsed url from the read_html function
  # id   - the game id corresponding to the given game
  #
  # Outputs:
  # - a list containing a matrix for the team information and a matrix for the
  #   game information
  
  # Obtain the boxscore information from the 'game' object.
  data = game %>% 
    html_nodes(".table-caption, #gamepackage-boxscore-module td, 
               #gamepackage-boxscore-module th") %>%
    html_text() %>%
    as.vector()
  
  # player.gap is the number of vector elements between players (equal to the 
  # number.of.statistics plus 1).
  player.gap = 15
  number.of.statistics = player.gap - 1
  
  ###
  # Team Statistics
  ###
  away.start.index = 1
  home.start.index = which(str_detect(data, "All Stats"))[2]
  
  # Determine the team names
  away.team = str_sub(data[1], 1, str_length(data[1]) - 9)
  home.team = str_sub(data[home.start.index], 1, 
                      str_length(data[home.start.index]) - 9)
  
  # Get the team statistics
  team.statistics.indicies = which(str_detect(data, coll("TEAM")))
  if (length(team.statistics.indicies) > 2){
    # Remove any detected "TEAM" strings that do not signify the start of team 
    # statistics (eg. "DNP-SUSPENDED BY TEAM").
    team.statistics.indicies = team.statistics.indicies[which(nchar(data[team.statistics.indicies]) == 4)]
  } 
  away.team.statistics.index = team.statistics.indicies[1]
  home.team.statistics.index = team.statistics.indicies[2]
  
  away.team.statistics = data[away.team.statistics.index + c(2:12, 14)]
  home.team.statistics = data[home.team.statistics.index + c(2:12, 14)]
  
  # Determine which team won the game.
  if (as.numeric(tail(away.team.statistics, n=1)) > 
      as.numeric(tail(home.team.statistics, n=1))) {
    away.win = 1
    home.win = 0
  } else {
    away.win = 0
    home.win = 1
  }
  
  # Split FG, 3P, and FT into makes and attempts.
  away.fg.split = str_split(away.team.statistics[1], '-')
  away.fgm      = sapply(away.fg.split, "[", 1)
  away.fga      = sapply(away.fg.split, "[", 2)
  
  home.fg.split = str_split(home.team.statistics[1], '-')
  home.fgm      = sapply(home.fg.split, "[", 1)
  home.fga      = sapply(home.fg.split, "[", 2)
  
  away.3p.split = str_split(away.team.statistics[2], '-')
  away.3pm      = sapply(away.3p.split, "[", 1)
  away.3pa      = sapply(away.3p.split, "[", 2)
  
  home.3p.split = str_split(home.team.statistics[2], '-')
  home.3pm      = sapply(home.3p.split, "[", 1)
  home.3pa      = sapply(home.3p.split, "[", 2)
  
  away.ft.split = str_split(away.team.statistics[3], '-')
  away.ftm      = sapply(away.ft.split, "[", 1)
  away.fta      = sapply(away.ft.split, "[", 2)
  
  home.ft.split = str_split(home.team.statistics[3], '-')
  home.ftm      = sapply(home.ft.split, "[", 1)
  home.fta      = sapply(home.ft.split, "[", 2)
  
  # Combine all information for the away and home teams.
  away.team.statistics = c(id, away.team, home.team, 0, away.win, away.fgm, 
                           away.fga, away.3pm, away.3pa, away.ftm, away.fta, 
                           away.team.statistics[4:12])
  home.team.statistics = c(id, home.team, away.team, 1, home.win, home.fgm, 
                           home.fga, home.3pm, home.3pa, home.ftm, home.fta, 
                           home.team.statistics[4:12])
  
  team.statistics = rbind(home.team.statistics, away.team.statistics)
  
  ###
  # Player Statistics
  ###
  # Determine where the starter information begins for both teams.
  starter.indicies   = which(str_detect(data, "starters"))
  away.starter.index = starter.indicies[1]
  home.starter.index = starter.indicies[2]
  
  # Determine where the bench information begins for both teams.
  bench.indicies   = which(str_detect(data, "bench"))
  away.bench.index = bench.indicies[1]
  home.bench.index = bench.indicies[2]
  
  # Determine the locations of all player names.
  starter.player.indicies = seq(from = player.gap, by = player.gap, length = 5)
  
  away.starter.indicies = starter.player.indicies + away.starter.index
  home.starter.indicies = starter.player.indicies + home.starter.index
  
  away.bench.indicies = seq(from = away.bench.index + player.gap, 
                            to = away.team.statistics.index - player.gap, 
                            by = player.gap)
  home.bench.indicies = seq(from = home.bench.index + player.gap, 
                            to = home.team.statistics.index - player.gap, 
                            by = player.gap)
  
  away.player.indicies = c(away.starter.indicies, away.bench.indicies)
  home.player.indicies = c(home.starter.indicies, home.bench.indicies)
  
  # Obtain the combined player and position information.
  away.player.positions = data[away.player.indicies]
  home.player.positions = data[home.player.indicies]
  
  # Separate the player and position information.
  nchar.away.position = rep(1,length(away.player.positions))
  nchar.away.position[which(str_detect(str_sub(away.player.positions, -2, -1), 
                                       '[[:upper:]][[:upper:]]'))] = 2

  nchar.home.position = rep(1,length(home.player.positions))
  nchar.home.position[which(str_detect(str_sub(home.player.positions, -2, -1), 
                                       '[[:upper:]][[:upper:]]'))] = 2
  
  away.positions = str_sub(away.player.positions, -1 * nchar.away.position, 
                           rep(-1, length(away.player.positions)))
  home.positions = str_sub(home.player.positions, -1 * nchar.home.position, 
                           rep(-1, length(home.player.positions)))
  
  away.players = str_sub(away.player.positions, 1, -1 * nchar.away.position - 1)
  home.players = str_sub(home.player.positions, 1, -1 * nchar.home.position - 1)
  
  # Obtain the staistic values for all players
  away.player.range      = away.start.index:(tail(away.bench.indicies, n=1) + 
                                          number.of.statistics)
  away.player.statistics = matrix(data[away.player.range[-(1 - away.start.index + c(away.start.index:(away.start.index + player.gap), 
                                                                                    away.player.indicies, away.bench.index:(away.bench.index + number.of.statistics)))]], 
                                  ncol=number.of.statistics, byrow=TRUE)
    
  home.player.range      = home.start.index:(tail(home.bench.indicies, n=1) +
                                          number.of.statistics)
  home.player.statistics = matrix(data[home.player.range[-(1 - home.start.index + c(home.start.index:(home.start.index + player.gap), 
                                                                                    home.player.indicies, home.bench.index:(home.bench.index + number.of.statistics)))]], 
                                  ncol=number.of.statistics, byrow=TRUE)
  
  # Split FG, 3P, and FT into makes and attempts.
  away.fg.split = str_split(away.player.statistics[,2], '-')
  away.fgm      = sapply(away.fg.split, "[", 1)
  away.fga      = sapply(away.fg.split, "[", 2)
  
  home.fg.split = str_split(home.player.statistics[,2], '-')
  home.fgm      = sapply(home.fg.split, "[", 1)
  home.fga      = sapply(home.fg.split, "[", 2)
  
  away.3p.split = str_split(away.player.statistics[,3], '-')
  away.3pm      = sapply(away.3p.split, "[", 1)
  away.3pa      = sapply(away.3p.split, "[", 2)
  
  home.3p.split = str_split(home.player.statistics[,3], '-')
  home.3pm      = sapply(home.3p.split, "[", 1)
  home.3pa      = sapply(home.3p.split, "[", 2)
  
  away.ft.split = str_split(away.player.statistics[,4], '-')
  away.ftm      = sapply(away.ft.split, "[", 1)
  away.fta      = sapply(away.ft.split, "[", 2)
  
  home.ft.split = str_split(home.player.statistics[,4], '-')
  home.ftm      = sapply(home.ft.split, "[", 1)
  home.fta      = sapply(home.ft.split, "[", 2)
  
  # Denote which players start and which come off the bench.
  away.starters = c(rep(1, 5), rep(0, length(away.bench.indicies)))
  home.starters = c(rep(1, 5), rep(0, length(home.bench.indicies)))
  
  # Combine the statistics for all players.
  away.player.statistics = cbind(away.players, away.positions, away.starters, 
                                 away.player.statistics[,1], away.fgm, 
                                 away.fga, away.3pm, away.3pa, away.ftm, 
                                 away.fta, away.player.statistics[, 5:14])
  home.player.statistics = cbind(home.players, home.positions, home.starters,
                                 home.player.statistics[,1], home.fgm, 
                                 home.fga, home.3pm, home.3pa, home.ftm, 
                                 home.fta, home.player.statistics[, 5:14])
  
  
  # Add DNP reason column to the statistics matrix.
  away.player.statistics = cbind(away.player.statistics, 
                                 rep(NA, length(away.players)))
  home.player.statistics = cbind(home.player.statistics, 
                                 rep(NA, length(home.players)))
  
  # Determine the DNP information and combine it with the statistics matricies.
  dnp.indicies = which(str_detect(data, "DNP"))
  away.dnp.indicies = dnp.indicies[dnp.indicies < home.start.index]
  home.dnp.indicies = dnp.indicies[dnp.indicies > home.start.index]
  
  if (length(away.dnp.indicies) > 0) {
    away.dnp.player.positions = data[away.dnp.indicies - 1]
    away.dnp.reasons = data[away.dnp.indicies]
    
    nchar.away.dnp.position = rep(1,length(away.dnp.player.positions))
    nchar.away.dnp.position[which(str_detect(str_sub(away.dnp.player.positions, -2, -1), '[[:upper:]][[:upper:]]'))] = 2
    
    away.dnp.positions = str_sub(away.dnp.player.positions, 
                                 -1 * nchar.away.dnp.position, 
                                 rep(-1, length(away.dnp.player.positions)))
    away.dnp.players = str_sub(away.dnp.player.positions, 1, 
                               -1 * nchar.away.dnp.position - 1)
    
    away.dnp.reasons = str_sub(away.dnp.reasons, 5)
    
    away.dnp.statistics = cbind(away.dnp.players, away.dnp.positions, 
                                matrix(NA, ncol=ncol(away.player.statistics) - 3, 
                                       nrow=length(away.dnp.indicies)), 
                                away.dnp.reasons)
    
    away.player.statistics = rbind(away.player.statistics, away.dnp.statistics)
  }
  
  if (length(home.dnp.indicies) > 0) {
    home.dnp.player.positions = data[home.dnp.indicies - 1]
    home.dnp.reasons = data[home.dnp.indicies]
    
    nchar.home.dnp.position = rep(1,length(home.dnp.player.positions))
    nchar.home.dnp.position[which(str_detect(str_sub(home.dnp.player.positions, -2, -1), '[[:upper:]][[:upper:]]'))] = 2
    
    home.dnp.positions = str_sub(home.dnp.player.positions, 
                                 -1 * nchar.home.dnp.position, 
                                 rep(-1, length(home.dnp.player.positions)))
    home.dnp.players = str_sub(home.dnp.player.positions, 1, 
                               -1 * nchar.home.dnp.position - 1)
    
    home.dnp.reasons = str_sub(home.dnp.reasons, 5)
    
    home.dnp.statistics = cbind(home.dnp.players, home.dnp.positions, 
                                matrix(NA, ncol=ncol(home.player.statistics) - 3, 
                                       nrow=length(home.dnp.indicies)), 
                                home.dnp.reasons)
    
    home.player.statistics = rbind(home.player.statistics, home.dnp.statistics)
  }
  
  # Define other game information.
  away.information = matrix(rep(c(id, away.team, home.team, 0, away.win), 
                                nrow(away.player.statistics)), 
                            nrow=nrow(away.player.statistics), byrow=TRUE)
  home.information = matrix(rep(c(id, home.team, away.team, 1, home.win), 
                                nrow(home.player.statistics)), 
                            nrow=nrow(home.player.statistics), byrow=TRUE)
  
  # Combine all player information.
  away.player.statistics = cbind(away.information, away.player.statistics)
  home.player.statistics = cbind(home.information, home.player.statistics)
  
  player.statistics = rbind(home.player.statistics, away.player.statistics)
  
  # Return the team and player information.
  return(list(team.statistics, player.statistics))

}