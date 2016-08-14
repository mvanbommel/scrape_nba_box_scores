# scrape_nba_box_scores
Scrapes NBA box score information for the 2015-2016 season from ESPN.com

The main function is **espn_nba_box_score_scraper_2015_2016.R** which then calls **get_data.R** to get the box score data from each game page. The team and player results for the full season are contained in **2015_2016_NBA_box_score_team_data.csv** and **2015_2016_NBA_box_score_player_data.csv** respectively. In addition to the standard box score statistics, the collected data contains information on positions, starters, and DNP (did not play) reasons.