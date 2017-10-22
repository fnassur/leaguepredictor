#using poissons distribution to predict scorelines
#Find the leagues average attacking strength and defensive strength in 2016
#attack strength @ home = season total goals scored @home / number of games in a season
#attack strength @ away = season total goals scored @away / number of games in a season
#defense strength @ home = season total goals concceded @home / number of games in a season
#defense strenght @ away = season total goals conceded @away / number of games in a season

#Team A attack strength = Team A goals @home x Attack strength @ home / number of games @home
#Team B defense strength = Team B goals conceded @away x Defense strength @away / number of games @away

#Team A defense strength = Team A goals conceded @home x Defense strength @ home / number of games @home
#Team B attack strength = Team B goals scored @away x Attack strength @away / number of games @away

#Prediciting Team A's Goals = Team A attack strength x Team B defense strength x Attack strength @home
#Predicting Team B's Goals = Team B attack strength x Team A defense strength x Attack strength @away

team.name = c("AFC Bournemouth", "Arsenal",
               "Brighton & Hove Albion",
               "Burnley",
               "Chelsea",
               "Crystal Palace",
               "Everton",
               "Huddersfield Town",
               "Leicester City",
               "Liverpool",
               "Manchester City",
               "Manchester United",
               "Newcastle United",
               "Southampton",
               "Stoke City",
               "Swansea City",
               "Tottenham Hotspur",
               "Watford",
               "West Bromwich Albion",
               "West Ham United"
               )
goals.home <- c(35,
                39,
                38,
                26,
                55,
                24,
                42,
                28,
                31,
                45,
                37,
                26,
                40,
                17,
                24,
                27,
                47,
                25,
                27,
                19
)
goals.away <- c(20,
                38,
                23,
                13,
                30,
                26,
                20,
                18,
                17,
                33,
                43,
                28,
                30,
                24,
                17,
                18,
                39,
                15,
                16,
                28
)
conceded.home <- c(29,
                   16,
                   12,
                   20,
                   17,
                   25,
                   16,
                   21,
                   25,
                   18,
                   17,
                   12,
                   19,
                   21,
                   24,
                   34,
                   9,
                   29,
                   22,
                   31
)
conceded.away <- c(38,
                   28,
                   21,
                   35,
                   16,
                   38,
                   28,
                   26,
                   38,
                   24,
                   22,
                   17,
                   14,
                   27,
                   32,
                   36,
                   17,
                   39,
                   29,
                   33
)
league.df <- data.frame(team.name,goals.home,conceded.home,goals.away,conceded.away)
totalgames = 380
attackstrength.home = sum(league.df$goals.home)/totalgames
attackstrength.away = sum(league.df$goals.away)/totalgames
defensestrength.home = sum(league.df$conceded.home)/totalgames
defensestrength.away = sum(league.df$conceded.away)/totalgames
homegames = 19
awaygames = 19
league.df$attackstrength.home = league.df$goals.home/attackstrength.home/homegames
league.df$defensestrength.home = league.df$conceded.home/defensestrength.home/homegames
league.df$attackstrength.away = league.df$goals.away/attackstrength.away/awaygames
league.df$defensestrength.away = league.df$conceded.away/defensestrength.away/awaygames


afcbournemouth.home <- data.frame(league.df$team.name)
afcbournemouth.home$homexg <- league.df$attackstrength.home[1]*league.df$defensestrength.away*attackstrength.home

afcbournemouth.home$homecg <- league.df$defensestrength.home[1]*league.df$attackstrength.away*defensestrength.home
afcbournemouth.home$zerogoals = afcbournemouth.home$zerogoals*100
afcbournemouth.home$onegoal = dpois(1,afcbournemouth.home$homexg)*100
afcbournemouth.home$twogoals = dpois(2,afcbournemouth.home$homexg)*100
afcbournemouth.home$threegoals = dpois(3,afcbournemouth.home$homexg)*100
afcbournemouth.home$fourgoals = dpois(4,afcbournemouth.home$homexg)*100
afcbournemouth.home$fivegoals = dpois(5,afcbournemouth.home$homexg)*100

afcbournemouth.home$zerogc = dpois(0,afcbournemouth.home$homecg)*100
afcbournemouth.home$onegc = dpois(1,afcbournemouth.home$homecg)*100
afcbournemouth.home$twogc = dpois(2,afcbournemouth.home$homecg)*100
afcbournemouth.home$threegc = dpois(3,afcbournemouth.home$homecg)*100
afcbournemouth.home$fourgc = dpois(4,afcbournemouth.home$homecg)*100
afcbournemouth.home$fivegc = dpois(5,afcbournemouth.home$homecg)*100