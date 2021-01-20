#devtools::install_github("abresler/nbastatR")
setwd("/Users/crawfordw/Dropbox/nbastats")
pacman::p_load(rvest,tidyverse,broom,purrr,corrplot,janitor,psych,data.table,plyr,dplyr,matrixcalc,gdata,scales,R.utils,nbastatR)
lapply((list.files(pattern='.csv')), unlink)
game_logs(seasons = 2021, result_types = c("team", "player"))
games<-dataGameLogsPlayer
games$PRA<-games$pts+games$ast+games$treb

#points,rebounds,assists,threes, all summary stats for each by player
points<-describeBy(games$pts, games$namePlayer, mat = TRUE) 
points$item<-"points"
assists<-describeBy(games$ast, games$namePlayer, mat = TRUE) 
assists$item<-"assists"
rebounds<-describeBy(games$treb, games$namePlayer, mat = TRUE) 
rebounds$item<-"rebounds"
threes<-describeBy(games$fg3m, games$namePlayer, mat = TRUE) 
threes$item<-"threes"
totals<-rbind(points,assists,rebounds,threes)
totals<-totals[,-3]
is.num <- sapply(totals, is.numeric)
totals[is.num] <- lapply(totals[is.num], round, 4)
totals<-setnames(totals, old = c('item','group1','n','sd'), new = c('stat','namePlayer','number of games','standard deviation'))
team<-unique(games[,c(16,42)])
totals<-left_join(totals,team, by = "namePlayer")
totals$'get this number or more'<-totals$mean-totals$`standard deviation`
player_average_totals<-totals[,c(15,2,3,1,4:14,16)]
player_average_totals$'get this number or more'<-totals$mean-totals$`standard deviation`
write.csv(player_average_totals, file = paste0(sub('\\..*', '', "player_average_totals"), format(Sys.Date(),'_%m-%d-%Y'), '.csv'))
player_stats_by_game<-games[,c(42,16,35,30,51,59)]
write.csv(player_stats_by_game, file = paste0(sub('\\..*', '', "player_stats_by_game"), format(Sys.Date(),'_%m-%d-%Y'), '.csv'))
view(player_average_totals);view(player_stats_by_game)
#other player data
alt_player_data<-teams_players_stats(2021)
alt_player_data<-alt_player_data[[7]][[1]]
alt_player_data<-alt_player_data[,c(7,13:18)]

#join players data
wide_player_data<-reshape(player_average_totals, idvar = "namePlayer", timevar = "stat", direction = "wide")
names(wide_player_data)
wide_player_data<-wide_player_data[,-c(15:16,29:30,43:44,45)]
playersdata<-left_join(wide_player_data,alt_player_data,by="namePlayer")
#players own team data
teams<-bref_teams_stats(seasons = 2021)
teams<-teams[[2]][[1]]
names(playersdata)[2]<-"nameTeam"
playersdata<-left_join(playersdata,teams,by="nameTeam")
playersdata <- playersdata[order(playersdata$nameTeam),]
x<-playersdata
##prepare data for modeling
#remove unwanted character vectors
chrvectors<-names(x[sapply(x, function(x) is.character(x))])
print(chrvectors)
names(x)
df<-x[,-c(58,60,61,67,68,70,75,76,223)]
#there are no zero variance predictors
#remove identical variables
####start back up here
df<-t(df)
dupes<-(duplicated(df))
dupes<-which(unname(dupes==TRUE))
df<-x[,-c(58,60,61,67,68,70,75,76,223)]
df<-df[,-c(dupes)]
#get rid of useless variables
glimpse(games)
glimpse(df)
which( colnames(df)=="isSeasonCurrent" )
which( colnames(df)=="isPlayoffTeam" )
df<-df[,-c(55,57)]
glimpse(df)
##get data for players and team back to game level to predict PRAs for any given game
names(games)
glimpse(games)
games<-games[,-c(1:4,13,37,41:42,53:58)]
cols <- sapply(games, is.logical)
games[,cols] <- lapply(games[,cols], as.numeric)
names(games)
games<-games[,c(11,1:10,12:45)]
games
combined<-left_join(games, df, by = "namePlayer")
combined<-combined[,c(1,46,2:45,47:279)]
write.csv(combined, file = paste0(sub('\\..*', '', "combined-nba-data"), format(Sys.Date(),'_%m-%d-%Y'), '.csv'))

#build regression model
glimpse(combined)
names(combined)
which( colnames(combined)=="idTeamNBA")
testdata<-combined[,-c(3:25,27:30,32:35,43,102)]
testdata<-na.omit(testdata)
names(testdata)
#data files with no game data (i.e., no cheating)
test_data_pts<-testdata[,-c(3,12,14)]
test_data_ast<-testdata[,-c(4,12,14)]
test_data_treb<-testdata[,-c(3,4,14)]
test_data_PRA<-testdata[,-c(3,4,12)]
names(test_data_pts)
teamnames<-unique(testdata$nameTeam)

full_model<-lapply(1:n, function(x) lm(Lung[,x] ~ Blood[,x] + Age + Gender)))
df %>% group_by(nameTeam) %>% nest() %>% 
  mutate(fit=map(data, ~lm(COUNT~YEAR, data = .)), results = map(fit, tidy)) %>%
  unnest(results)

models_full<-dlply(test_data_pts, "nameTeam", function(df)
  lm(pts ~ . - nameTeam - namePlayer - fpts, data=test_data_pts))
  #lm(pts ~ hlfAShootingTeam + countLayupsShootingTeam, data=test_data_pts))
ldply(models, coef)
l_ply(models, summary, .print=TRUE)
models_empty<-dlply(test_data_pts, "nameTeam", function(df)
  lm(pts ~ 1, data=test_data_pts))
ldply(models, coef)
l_ply(models, summary, .print=TRUE)
models_step<-dlply(test_data_pts, "nameTeam", function(df)
  step(models_empty, scope = list(lower = models_empty, upper = models_full), direction = "forward"))
ldply(models, coef)
l_ply(models, summary, .print=TRUE)

names(test_data_pts)


#model for pts
full_modelp <- lm(pts ~ . - nameTeam - namePlayer -fpts, data=test_data_pts)
sumpp<-summary(full_modelp)
null_modelp <- lm(pts~1, data = test_data_pts)
step_modelp <- step(null_modelp, scope = list(lower = null_modelp, upper = full_modelp), direction = "forward")
sump<-summary(step_modelp)
print(sump)
print(sumpp)

#model for ast
full_modela <- lm(ast ~ ., data=test_data_ast)
summary(full_modela)
null_modela <- lm(ast~1, data = test_data_ast)
step_modela <- step(null_modela, scope = list(lower = null_modela, upper = full_modela), direction = "forward")
suma<-summary(step_modela)

#model for reb
full_modelr <- lm(treb ~ ., data=test_data_treb)
summary(full_modelr)
null_modelr <- lm(treb~1, data = test_data_treb)
step_modelr <- step(null_modelr, scope = list(lower = null_modelr, upper = full_modelr), direction = "forward")
sumr<-summary(step_modelr)
print(sumr)
#model for PRA

full_modelpra <- lm(PRA ~ ., data=test_data_PRA)
summary(full_modelpra)
null_modelpra <- lm(PRA~1, data = test_data_PRA)
step_modelpra <- step(null_modelpra, scope = list(lower = null_modelpra, upper = full_modelpra), direction = "forward")
sumpra<-summary(step_modelpra)





r2matrix<-matrix(,nrow=2,ncol=4)
r2matrix[1,1]<-sump[["r.squared"]]
r2matrix[1,2]<-sumr[["r.squared"]]
r2matrix[1,3]<-suma[["r.squared"]]
r2matrix[1,4]<-sumpra[["r.squared"]]


#data files with no game data (i.e., no cheating)
#test_data_pts<-test2data[,-c(1,10,11,12)]
#test_data_ast<-test2data[,-c(2,10,11,12)]
#test_data_treb<-test2data[,-c(1,2,11,12)]
#test_data_PRA<-test2data[,-c(1,2,10,11)]

#model for pts
#full_modelp <- lm(pts ~ ., data=test_data_pts)
#summary(full_modelp)
#null_modelp <- lm(pts~1, data = test_data_pts)
#step_modelp <- step(null_modelp, scope = list(lower = null_modelp, upper = full_modelp), direction = "forward")
#sump<-summary(step_modelp)

#model for ast
#full_modela <- lm(ast ~ ., data=test_data_ast)
#summary(full_modela)
#null_modela <- lm(ast~1, data = test_data_ast)
#step_modela <- step(null_modela, scope = list(lower = null_modela, upper = full_modela), direction = "forward")
#suma<-summary(step_modela)

#model for reb
#full_modelr <- lm(treb ~ ., data=test_data_treb)
#summary(full_modelr)
#null_modelr <- lm(treb~1, data = test_data_treb)
#step_modelr <- step(null_modelr, scope = list(lower = null_modelr, upper = full_modelr), direction = "forward")
#sumr<-summary(step_modelr)
#print(sumr)

#model for PRA
#full_modelpra <- lm(PRA ~ ., data=test_data_PRA)
#summary(full_modelpra)
#null_modelpra <- lm(PRA~1, data = test_data_PRA)
#step_modelpra <- step(null_modelpra, scope = list(lower = null_modelpra, upper = full_modelpra), direction = "forward")
#sumpra<-summary(step_modelpra)
#print(sumpra)

#r2matrix[2,1]<-sump[["r.squared"]]
#r2matrix[2,2]<-sumr[["r.squared"]]
#r2matrix[2,3]<-suma[["r.squared"]]
#r2matrix[2,4]<-sumpra[["r.squared"]]

#rownames(r2matrix) <- c("r2 with fantasy", "r2 without fantasy")
#colnames(r2matrix) <- c("points", "assists", "rebounds", "PRA")

#schedule for current season
schedule<-current_schedule()
#predict value for today given current player/team data and who the opposing team is

