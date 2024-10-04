######
######

library(mgcv)
library(tidyverse)
library(splines)
library(MASS)
library(DHARMa)


## Whether to remove extra time, to align with minute-by-minute approach
## (plus take care of the issue where events get stacked up on 90th minute, instead of going into 91+)
remove.extra <- TRUE

## If we use models where "Minute.clean" was included
include.minute <- TRUE

league.name <- c("Bundesliga", "SerieA", "LaLiga", "Ligue1", "PremierLeague")

# load(file="gam_nb_obj_Corners.Robj")

load(file=paste0(ifelse(include.minute,
                        "",
                        "NO_MINUTES_TEST_"),
                 "gam_nb_obj",  
                 ifelse(remove.extra, "_NO_EXTRA_TIME", ""), 
                 "_Corners.Robj"))


for (league in league.name){
  
  print(league)
  
  cat("\n")
  
  #######
  ### LOADING, SETTING UP DATA
  #######
  
  #  our.df <- read.csv(paste0(league, "_NO_EXTRA_TIME_Sami_minute_by_minute_df_w_red_cards_WITH_BOOKING_ODDS.csv"))
  
  ## To be used for prediction (including everything, so that totals correspond to full data)
  our.df.pred <- read.csv(paste0(league, "_Sami_minute_by_minute_df_w_red_cards_WITH_BOOKING_ODDS.csv"))
  
  # ## REMOVING NA betting data
  # our.df <- our.df %>% filter(!is.na(Weighted.Win.Prob))
  # dim(our.df)
  # 
  # ## For EXTRA TIME: JUST FORCE IT TO BE EQUAL TO THE LAST MINUTE (e.g. for extra time in 1st half - make it =45, in 2nd half - =90)
  # 
  # # our.df$Minute.clean[our.df$half_id == 1 & our.df$Minute.clean > 45] <- 45
  # # our.df$Minute.clean[our.df$Minute.clean > 90] <- 90
  # 
  # 
  # our.df  <- our.df  %>%
  #   mutate(abs.Score.Diff = abs(Score.Diff),
  #          abs.RedCard.Diff = abs(RedCard.Diff))
  # 
  # our.df  <- our.df  %>%
  #   mutate(Period.ID = group_indices(our.df, .dots=c("ID", "Minute.clean", "abs.Score.Diff", "abs.RedCard.Diff"))) %>%
  #   arrange(Period.ID) %>%
  #   mutate(Period.ID = factor(Period.ID))
  # 
  # our.df <- our.df %>% mutate(Period.ID = factor(Period.ID), ID = factor(ID))
  
  
  #######
  ### GETTING ADJUSTMENTS
  #######
  
  
  
  if (include.minute == TRUE){
    ### 1. SCORE DIFFERENTIAL
    
    # Getting the DIFFERENTIALS (log-scale "linear" effects)
    all.score.diffs <- c(min(our.df.pred$Score.Diff):max(our.df.pred$Score.Diff))
    log.score.diff.effects <- as.numeric(predict(gam.nb.obj[[league]],
                                                 newdata = data.frame(Minute.clean=45, Weighted.Win.Prob = 0, HomeAway = "Home", RedCard.Diff =0,
                                                                      Score.Diff = all.score.diffs))) -
      as.numeric(predict(gam.nb.obj[[league]],
                         newdata = data.frame(Minute.clean=45, Weighted.Win.Prob = 0, HomeAway = "Home", RedCard.Diff =0,
                                              Score.Diff = rep(0, length(all.score.diffs)))))
    
    names(log.score.diff.effects) <- all.score.diffs
    log.score.diff.effects
    
    
    # Multiplicative effects
    exp(log.score.diff.effects)
    
    # Doing the multiplication
    our.df.pred$Adj.Coef.ScoreDiff <- 
      exp(-log.score.diff.effects)[sapply(our.df.pred$Score.Diff, function(x) which(names(log.score.diff.effects) == x))]
    
    
    
    
    ### 2. RED CARD DIFF
    
    all.redcard.diffs <- c(min(our.df.pred$RedCard.Diff):max(our.df.pred$RedCard.Diff))
    log.redcard.diff.effects <- as.numeric(predict(gam.nb.obj[[league]],
                                                   newdata = data.frame(Minute.clean=45, Weighted.Win.Prob = 0, HomeAway = "Home", Score.Diff =0,
                                                                        RedCard.Diff = all.redcard.diffs))) -
      as.numeric(predict(gam.nb.obj[[league]],
                         newdata = data.frame(Minute.clean=45, Weighted.Win.Prob = 0, HomeAway = "Home", Score.Diff =0,
                                              RedCard.Diff = rep(0, length(all.redcard.diffs)))))
    
    names(log.redcard.diff.effects) <- all.redcard.diffs
    log.redcard.diff.effects
    
    # Multiplicative effects
    exp(log.redcard.diff.effects)
    
    # Doing the multiplication
    our.df.pred$Adj.Coef.RedCardDiff <- 
      exp(-log.redcard.diff.effects)[sapply(our.df.pred$RedCard.Diff, function(x) which(names(log.redcard.diff.effects) == x))]
    
    
    
    
    ### 3. HOME/AWAY FACTOR
    
    all.homeaway <- unique(our.df.pred$HomeAway)
    log.homeaway.effects <- as.numeric(predict(gam.nb.obj[[league]],
                                               newdata = data.frame(Minute.clean=45, Weighted.Win.Prob = 0, Score.Diff = 0, RedCard.Diff =0,
                                                                    HomeAway = all.homeaway))) -
      as.numeric(predict(gam.nb.obj[[league]],
                         newdata = data.frame(Minute.clean=45, Weighted.Win.Prob = 0, Score.Diff = 0, RedCard.Diff =0,
                                              HomeAway = rep("Home", length(all.homeaway)))))
    
    names(log.homeaway.effects) <- all.homeaway
    log.homeaway.effects
    
    # Multiplicative effects
    exp(log.homeaway.effects)
    
    # Doing the multiplication
    our.df.pred$Adj.Coef.HomeAway <- 
      exp(-log.homeaway.effects)[sapply(our.df.pred$HomeAway, function(x) which(names(log.homeaway.effects) == x))]
    
    
    ### 4. Minute
    
    # Getting the DIFFERENTIALS (log-scale "linear" effects)
    all.minutes <- c(min(our.df.pred$Minute.clean):max(our.df.pred$Minute.clean))
    log.minute.effects <- as.numeric(predict(gam.nb.obj[[league]],
                                             newdata = data.frame(Score.Diff = 0, Weighted.Win.Prob = 0, HomeAway = "Home", RedCard.Diff =0,
                                                                  Minute.clean = all.minutes))) -
      as.numeric(predict(gam.nb.obj[[league]],
                         newdata = data.frame(Score.Diff = 0, Weighted.Win.Prob = 0, HomeAway = "Home", RedCard.Diff =0,
                                              Minute.clean = rep(45, length(all.minutes)))))
    
    names(log.minute.effects) <- all.minutes
    log.minute.effects
    
    
    # Multiplicative effects
    exp(log.minute.effects)
    
    # Doing the multiplication
    our.df.pred$Adj.Coef.Minute <- 
      exp(-log.minute.effects)[sapply(our.df.pred$Minute.clean, function(x) which(names(log.minute.effects) == x))]
    
    
    our.df.pred <- our.df.pred %>%
      mutate(Corners.Adj = Corners*Adj.Coef.ScoreDiff*Adj.Coef.RedCardDiff*Adj.Coef.HomeAway*Adj.Coef.Minute,
             Corners.Adj.No.Min = Corners*Adj.Coef.ScoreDiff*Adj.Coef.RedCardDiff*Adj.Coef.HomeAway)
    
  } else {
    
    ### 1. SCORE DIFFERENTIAL
    
    # Getting the DIFFERENTIALS (log-scale "linear" effects)
    all.score.diffs <- c(min(our.df.pred$Score.Diff):max(our.df.pred$Score.Diff))
    log.score.diff.effects <- as.numeric(predict(TEST_gam.nb.obj[[league]],
                                                 newdata = data.frame(Weighted.Win.Prob = 0, HomeAway = "Home", RedCard.Diff =0,
                                                                      Score.Diff = all.score.diffs))) -
      as.numeric(predict(TEST_gam.nb.obj[[league]],
                         newdata = data.frame(Weighted.Win.Prob = 0, HomeAway = "Home", RedCard.Diff =0,
                                              Score.Diff = rep(0, length(all.score.diffs)))))
    
    names(log.score.diff.effects) <- all.score.diffs
    log.score.diff.effects
    
    
    # Multiplicative effects
    exp(log.score.diff.effects)
    
    # Doing the multiplication
    our.df.pred$Adj.Coef.ScoreDiff <- 
      exp(-log.score.diff.effects)[sapply(our.df.pred$Score.Diff, function(x) which(names(log.score.diff.effects) == x))]
    
    
    
    
    ### 2. RED CARD DIFF
    
    all.redcard.diffs <- c(min(our.df.pred$RedCard.Diff):max(our.df.pred$RedCard.Diff))
    log.redcard.diff.effects <- as.numeric(predict(TEST_gam.nb.obj[[league]],
                                                   newdata = data.frame(Weighted.Win.Prob = 0, HomeAway = "Home", Score.Diff =0,
                                                                        RedCard.Diff = all.redcard.diffs))) -
      as.numeric(predict(TEST_gam.nb.obj[[league]],
                         newdata = data.frame(Weighted.Win.Prob = 0, HomeAway = "Home", Score.Diff =0,
                                              RedCard.Diff = rep(0, length(all.redcard.diffs)))))
    
    names(log.redcard.diff.effects) <- all.redcard.diffs
    log.redcard.diff.effects
    
    # Multiplicative effects
    exp(log.redcard.diff.effects)
    
    # Doing the multiplication
    our.df.pred$Adj.Coef.RedCardDiff <- 
      exp(-log.redcard.diff.effects)[sapply(our.df.pred$RedCard.Diff, function(x) which(names(log.redcard.diff.effects) == x))]
    
    
    
    
    ### 3. HOME/AWAY FACTOR
    
    all.homeaway <- unique(our.df.pred$HomeAway)
    log.homeaway.effects <- as.numeric(predict(TEST_gam.nb.obj[[league]],
                                               newdata = data.frame(Weighted.Win.Prob = 0, Score.Diff = 0, RedCard.Diff =0,
                                                                    HomeAway = all.homeaway))) -
      as.numeric(predict(TEST_gam.nb.obj[[league]],
                         newdata = data.frame( Weighted.Win.Prob = 0, Score.Diff = 0, RedCard.Diff =0,
                                               HomeAway = rep("Home", length(all.homeaway)))))
    
    names(log.homeaway.effects) <- all.homeaway
    log.homeaway.effects
    
    # Multiplicative effects
    exp(log.homeaway.effects)
    
    # Doing the multiplication
    our.df.pred$Adj.Coef.HomeAway <- 
      exp(-log.homeaway.effects)[sapply(our.df.pred$HomeAway, function(x) which(names(log.homeaway.effects) == x))]
    
    
    our.df.pred <- our.df.pred %>%
      mutate(Corners.Adj = Corners*Adj.Coef.ScoreDiff*Adj.Coef.RedCardDiff*Adj.Coef.HomeAway,
      )
    
    
  }
  
  ### MAKING SURE TEAM NAMES ARE CORRECT (there's still some other leagues getting mistakenly mixed in here)
  team.names <- read.csv(paste0("Odds_Data/Final_matched_teams_", league, ".csv"))$Team_ESPN
  our.df.pred <- our.df.pred %>% filter(Team %in% team.names)
  

  # ### 
  # cat("\n")
  # print(paste0("Top-10 single game shifts, ", ifelse(include.minute, "WITH MINUTE:", "NO MINUTE:")))
  # 
  final.df <- our.df.pred %>%
    group_by(gameId, Team, season) %>%
    summarise(Corners = sum(Corners),
              Corners.Adj= sum(Corners.Adj),
              Corners.Diff = Corners.Adj - Corners,
              Corners.Adj.No.Min = sum(Corners.Adj.No.Min),
              Corners.Diff.No.Min = Corners.Adj.No.Min - Corners) %>%
    ungroup() %>%
    group_by(Team, season) %>%
    summarise(Corners.Per.Game = round(mean(Corners),1),
              Corners.Adj.Per.Game = round(mean(Corners.Adj),1),
              Corners.Diff.Per.Game = round(Corners.Adj.Per.Game - Corners.Per.Game,1),
              n.games = length(unique(gameId))) %>%
    ungroup() %>%
    group_by(season) %>%
    mutate(Corners.Per.Game.Full = paste0(round(Corners.Per.Game,1), " (#", rank(-Corners.Per.Game, ties="first"), ")"),
           Corners.Adj.Per.Game.Full = paste0(round(Corners.Adj.Per.Game,1), " (#", rank(-Corners.Adj.Per.Game, ties="first"), ")")) %>%
    dplyr::select(Team, season, n.games, 
                  Corners.Per.Game.Full, Corners.Adj.Per.Game.Full, Corners.Diff.Per.Game) %>%
    arrange(desc(abs(Corners.Diff.Per.Game)))
  

    
  
  
  
  
  # print(head(final.df %>% filter(Corners.Diff.Per.Game > 0), 10))
  # print(head(final.df  %>% filter(Corners.Diff.Per.Game < 0), 10))
  
  # Corners in the lead (Minutes in the lead), Corners shorthanded (Minutes shorthanded)
  
  
  ## Getting Corners in the lead, Corners shorthanded
  
  scorediff.diff.ranks <- our.df.pred %>%
    # filter(Score.Diff > 0) %>%
    group_by(gameId, Team, season) %>%
    summarise(Corners.Lead = sum(Corners[Score.Diff > 0]),
              Minutes.Lead = sum(Score.Diff > 0),
              Corners.Trail = sum(Corners[Score.Diff < 0]),
              Minutes.Trail = sum(Score.Diff < 0)) %>%
    ungroup() %>%
    group_by(Team, season) %>%
    summarise(Corners.Lead.Per.Game = mean(Corners.Lead),
              Minutes.Lead.Per.Game = mean(Minutes.Lead),
              Corners.Trail.Per.Game = mean(Corners.Trail),
              Minutes.Trail.Per.Game = mean(Minutes.Trail),
              n.games = length(unique(gameId))) %>%
    ungroup() %>%
    #filter(season==2020)
    group_by(season) %>%
    mutate(Corners.Lead.Per.Game.Full = paste0(round(Corners.Lead.Per.Game,1), " (#", rank(-Corners.Lead.Per.Game, ties="first"), ")"),
           Minutes.Lead.Per.Game.Full = paste0(round(Minutes.Lead.Per.Game,1), " (#", rank(-Minutes.Lead.Per.Game, ties="first"), ")"),
           Corners.Trail.Per.Game.Full = paste0(round(Corners.Trail.Per.Game,1), " (#", rank(-Corners.Trail.Per.Game, ties="first"), ")"),
           Minutes.Trail.Per.Game.Full = paste0(round(Minutes.Trail.Per.Game,1), " (#", rank(-Minutes.Trail.Per.Game, ties="first"), ")")) %>%
    dplyr::select(Team, season, n.games, 
                  Corners.Lead.Per.Game.Full, Minutes.Lead.Per.Game.Full,
                  Corners.Trail.Per.Game.Full, Minutes.Trail.Per.Game.Full)
    # arrange(desc(Corners.Lead.Per.Game))
  
  
  redcard.diff.ranks <- our.df.pred %>%
   #  filter(RedCard.Diff > 0) %>%
    group_by(gameId, Team, season) %>%
    summarise(Corners.DownMen = sum(Corners[RedCard.Diff > 0]),
              Minutes.DownMen = sum(RedCard.Diff > 0),
              Corners.UpMen = sum(Corners[RedCard.Diff < 0]),
              Minutes.UpMen = sum(RedCard.Diff < 0)) %>%
    ungroup() %>%
    group_by(Team, season) %>%
    summarise(Corners.DownMen.Per.Game = mean(Corners.DownMen),
              Minutes.DownMen.Per.Game = mean(Minutes.DownMen),
              Corners.UpMen.Per.Game = mean(Corners.UpMen),
              Minutes.UpMen.Per.Game = mean(Minutes.UpMen),
              n.games = length(unique(gameId))) %>%
    ungroup() %>%
    #filter(season==2020)
    group_by(season) %>%
    mutate(Corners.DownMen.Per.Game.Full = paste0(round(Corners.DownMen.Per.Game,1), " (#", rank(-Corners.DownMen.Per.Game, ties="first"), ")"),
           Minutes.DownMen.Per.Game.Full = paste0(round(Minutes.DownMen.Per.Game,1), " (#", rank(-Minutes.DownMen.Per.Game, ties="first"), ")"),
           Corners.UpMen.Per.Game.Full = paste0(round(Corners.UpMen.Per.Game,1), " (#", rank(-Corners.UpMen.Per.Game, ties="first"), ")"),
           Minutes.UpMen.Per.Game.Full = paste0(round(Minutes.UpMen.Per.Game,1), " (#", rank(-Minutes.UpMen.Per.Game, ties="first"), ")")) %>%
    dplyr::select(Team, season, n.games, 
           Corners.DownMen.Per.Game.Full, Minutes.DownMen.Per.Game.Full,
           Corners.UpMen.Per.Game.Full, Minutes.UpMen.Per.Game.Full)
   #  arrange(desc(Corners.DownMen.Per.Game))
  
  
  options(tibble.width=Inf)
  print(head(final.df %>% left_join(scorediff.diff.ranks) %>% left_join(redcard.diff.ranks) %>%
               filter(Corners.Diff.Per.Game > 0), 10))
  
  
  # print(head(final.df, 50))
  
  # our.df %>%
  #   filter(gameId == head(final.df$gameId, 1), Team == head(final.df$Team, 1))
  
  
  
  
  cat("\n")
  cat("\n")
}



######
######
### SHOTS
######
######


### BUNDESLIGA

#    Team              season n.games Shots.Per.Game.Full Shots.Adj.Per.Game.Full Shots.Diff.Per.Game Shots.Lead.Per.Game.Full Minutes.Lead.Per.Game.Full
#    <chr>              <int>   <int> <chr>               <chr>                                 <dbl> <chr>                    <chr>                     
#  1 Bayern Munich       2020      34 18 (#1)             19.7 (#1)                               1.7 10.2 (#1)                52.2 (#1)                 
#  2 Bayern Munich       2023      34 18.4 (#1)           20.1 (#1)                               1.7 9.6 (#1)                 54.2 (#1)                 
#  3 Bayern Munich       2015      34 17.1 (#1)           18.7 (#1)                               1.6 8.8 (#1)                 44.9 (#1)                 
#  4 Bayern Munich       2021      34 17 (#1)             18.5 (#1)                               1.5 8.3 (#1)                 47.4 (#1)                 
#  5 Bayern Munich       2022      34 19.5 (#1)           21 (#1)                                 1.5 9.2 (#1)                 45.7 (#1)                 
#  6 Bayern Munich       2012      33 15.7 (#2)           17.1 (#2)                               1.4 6.8 (#2)                 39.2 (#2)                 
#  7 Borussia Dortmund   2013      34 15.9 (#3)           17.3 (#2)                               1.4 7.7 (#2)                 48.2 (#2)                 
#  8 Bayern Munich       2013      34 17 (#1)             18.2 (#1)                               1.2 8.8 (#1)                 51 (#1)                   
#  9 Bayern Munich       2014      34 18.5 (#1)           19.7 (#1)                               1.2 9.3 (#1)                 47.9 (#1)                 
# 10 Bayern Munich       2019      34 18.5 (#1)           19.7 (#1)                               1.2 9.5 (#1)                 50.7 (#1)                 
#    Shots.Trail.Per.Game.Full Minutes.Trail.Per.Game.Full Shots.DownMen.Per.Game.Full Minutes.DownMen.Per.Game.Full Shots.UpMen.Per.Game.Full Minutes.UpMen.Per.Game.Full
#    <chr>                     <chr>                       <chr>                       <chr>                         <chr>                     <chr>                      
#  1 1.8 (#18)                 8.9 (#18)                   0.3 (#6)                    3.2 (#8)                      0.6 (#7)                  2.1 (#9)                   
#  2 2.2 (#17)                 9.2 (#18)                   0.4 (#2)                    4.2 (#5)                      0 (#16)                   0 (#16)                    
#  3 1.5 (#17)                 7.9 (#18)                   0.5 (#2)                    4.7 (#2)                      0.3 (#7)                  1 (#12)                    
#  4 3.8 (#7)                  19.8 (#13)                  0.6 (#1)                    3.4 (#3)                      0 (#14)                   0 (#14)                    
#  5 2.4 (#17)                 12.2 (#18)                  0.1 (#6)                    1.5 (#7)                      0 (#12)                   0 (#12)                    
#  6 2.5 (#16)                 11.4 (#17)                  0.5 (#3)                    2.9 (#5)                      0.4 (#10)                 2.2 (#10)                  
#  7 2.8 (#13)                 14.6 (#16)                  0.9 (#1)                    4.6 (#3)                      0.4 (#10)                 1.7 (#13)                  
#  8 1.6 (#18)                 5.5 (#18)                   0 (#15)                     0.8 (#17)                     0.6 (#5)                  5.2 (#5)                   
#  9 1.8 (#18)                 8.1 (#18)                   0 (#16)                     0.3 (#17)                     0.2 (#12)                 0.4 (#14)                  
# 10 2 (#18)                   9.1 (#18)                   0.1 (#12)                   1.1 (#11)                     0.9 (#3)                  3.2 (#3)  



### SERIE A

#    Team           season n.games Shots.Per.Game.Full Shots.Adj.Per.Game.Full Shots.Diff.Per.Game Shots.Lead.Per.Game.Full Minutes.Lead.Per.Game.Full
#    <chr>           <int>   <int> <chr>               <chr>                                 <dbl> <chr>                    <chr>                     
#  1 Napoli           2017      38 17.6 (#2)           19.9 (#1)                               2.3 8.1 (#1)                 46.9 (#2)                 
#  2 AS Roma          2017      38 17.6 (#1)           19.6 (#2)                               2   7.8 (#2)                 44.8 (#3)                 
#  3 Napoli           2018      38 17.1 (#1)           19.1 (#1)                               2   6.8 (#1)                 40.1 (#3)                 
#  4 AC Milan         2012      33 15.8 (#3)           17.7 (#3)                               1.9 6.4 (#2)                 42.4 (#1)                 
#  5 Atalanta         2021      37 16.1 (#2)           18 (#2)                                 1.9 7.6 (#1)                 41.9 (#3)                 
#  6 Internazionale   2009      33 14.7 (#7)           16.6 (#3)                               1.9 6.1 (#1)                 41.8 (#1)                 
#  7 Internazionale   2020      36 16.1 (#6)           17.9 (#5)                               1.8 6.9 (#1)                 49 (#1)                   
#  8 Internazionale   2022      37 17.5 (#1)           19.3 (#1)                               1.8 7.5 (#1)                 43.6 (#3)                 
#  9 Juventus         2014      37 15.5 (#3)           17.3 (#1)                               1.8 6.5 (#1)                 47.5 (#1)                 
# 10 Juventus         2015      38 15.6 (#4)           17.4 (#1)                               1.8 5.9 (#1)                 44.6 (#1)                 
#    Shots.Trail.Per.Game.Full Minutes.Trail.Per.Game.Full Shots.DownMen.Per.Game.Full Minutes.DownMen.Per.Game.Full Shots.UpMen.Per.Game.Full Minutes.UpMen.Per.Game.Full
#    <chr>                     <chr>                       <chr>                       <chr>                         <chr>                     <chr>                      
#  1 2.5 (#18)                 12.7 (#19)                  0.2 (#7)                    1.9 (#13)                     0.7 (#5)                  2.9 (#8)                   
#  2 2.6 (#17)                 13.4 (#18)                  0 (#17)                     0.1 (#19)                     0.1 (#20)                 1.1 (#17)                  
#  3 2.4 (#18)                 12 (#19)                    0.2 (#12)                   3.3 (#10)                     0 (#20)                   0 (#20)                    
#  4 2 (#19)                   7.5 (#20)                   0.5 (#4)                    2 (#10)                       0.2 (#15)                 1.2 (#14)                  
#  5 2.3 (#20)                 14.4 (#19)                  0.2 (#11)                   2.4 (#11)                     0.3 (#12)                 1.5 (#12)                  
#  6 0.6 (#20)                 3.5 (#20)                   0.5 (#8)                    3.3 (#12)                     0.5 (#13)                 1.7 (#15)                  
#  7 2.5 (#19)                 11.7 (#19)                  0.2 (#13)                   2.5 (#13)                     0.6 (#8)                  2.8 (#12)                  
#  8 2.8 (#18)                 12.5 (#19)                  0 (#19)                     0 (#20)                       0.5 (#8)                  1.7 (#14)                  
#  9 0.9 (#19)                 5.2 (#20)                   0.2 (#12)                   2.3 (#14)                     0.7 (#11)                 4 (#8)                     
# 10 0.8 (#20)                 3.8 (#20)                   0.1 (#17)                   1.3 (#16)                     0.2 (#17)                 1.4 (#17) 



### LA LIGA

#    Team           season n.games Shots.Per.Game.Full Shots.Adj.Per.Game.Full Shots.Diff.Per.Game Shots.Lead.Per.Game.Full Minutes.Lead.Per.Game.Full
#    <chr>           <int>   <int> <chr>               <chr>                                 <dbl> <chr>                    <chr>                     
#  1 Atltico Madrid   2011       1 21 (#1)             26 (#1)                                 5   16 (#1)                  79 (#1)                   
#  2 Real Madrid      2011      33 19.9 (#2)           23.5 (#2)                               3.6 10.2 (#2)                48.9 (#3)                 
#  3 Barcelona        2012      29 16.5 (#2)           19.6 (#2)                               3.1 9.8 (#1)                 53.3 (#1)                 
#  4 Barcelona        2010      34 15.9 (#3)           18.9 (#2)                               3   8.6 (#2)                 50.2 (#1)                 
#  5 Real Madrid      2015      38 17.7 (#1)           20.7 (#1)                               3   8.9 (#1)                 51.2 (#1)                 
#  6 Real Madrid      2010      36 21.6 (#1)           24.5 (#1)                               2.9 9.9 (#1)                 45.9 (#2)                 
#  7 Real Madrid      2012      30 19.2 (#1)           21.9 (#1)                               2.7 9.5 (#2)                 50.3 (#2)                 
#  8 Real Madrid      2013      36 18.1 (#1)           20.7 (#1)                               2.6 8.9 (#1)                 46.2 (#2)                 
#  9 Real Madrid      2016      37 18.6 (#1)           21.2 (#1)                               2.6 8.9 (#1)                 50.8 (#2)                 
# 10 Barcelona        2009      35 18.3 (#1)           20.8 (#1)                               2.5 8.8 (#1)                 46.5 (#1)                 
#    Shots.Trail.Per.Game.Full Minutes.Trail.Per.Game.Full Shots.DownMen.Per.Game.Full Minutes.DownMen.Per.Game.Full Shots.UpMen.Per.Game.Full Minutes.UpMen.Per.Game.Full
#    <chr>                     <chr>                       <chr>                       <chr>                         <chr>                     <chr>                      
#  1 0 (#21)                   0 (#21)                     0 (#20)                     0 (#20)                       0 (#21)                   0 (#21)                    
#  2 1.9 (#19)                 5.6 (#19)                   0.7 (#2)                    3.2 (#10)                     0.4 (#15)                 1.9 (#15)                  
#  3 1.4 (#19)                 6.7 (#18)                   0.5 (#5)                    2.8 (#13)                     0.3 (#14)                 0.9 (#17)                  
#  4 0.7 (#20)                 3.6 (#20)                   0.6 (#3)                    3.8 (#11)                     0.4 (#17)                 1.3 (#18)                  
#  5 2 (#18)                   11.3 (#18)                  0.3 (#9)                    1.5 (#15)                     0 (#20)                   0 (#20)                    
#  6 2.9 (#14)                 8.8 (#19)                   0.6 (#4)                    3.1 (#14)                     1.4 (#4)                  4.7 (#7)                   
#  7 1.9 (#18)                 6.6 (#19)                   0.6 (#2)                    4.1 (#7)                      2.3 (#1)                  9.4 (#1)                   
#  8 2.6 (#17)                 13.9 (#17)                  0.8 (#3)                    5.5 (#6)                      0.7 (#12)                 3.4 (#14)                  
#  9 2.1 (#18)                 10.2 (#18)                  0.3 (#7)                    1.6 (#18)                     1.1 (#4)                  4.1 (#9)                   
# 10 2.6 (#17)                 11.2 (#20)                  0.4 (#12)                   2.8 (#17)                     1.2 (#8)                  5 (#9)  




### LIGUE 1

#    Team                season n.games Shots.Per.Game.Full Shots.Adj.Per.Game.Full Shots.Diff.Per.Game Shots.Lead.Per.Game.Full Minutes.Lead.Per.Game.Full
#    <chr>                <int>   <int> <chr>               <chr>                                 <dbl> <chr>                    <chr>                     
#  1 Paris Saint-Germain   2018      38 16.1 (#2)           18.4 (#1)                               2.3 8.2 (#1)                 48 (#1)                   
#  2 AS Monaco             2017      38 14.5 (#3)           16.3 (#2)                               1.8 7.7 (#1)                 55.1 (#1)                 
#  3 Paris Saint-Germain   2016      38 14.7 (#1)           16.5 (#1)                               1.8 7.4 (#1)                 47.2 (#1)                 
#  4 Paris Saint-Germain   2023      38 14.9 (#1)           16.6 (#1)                               1.7 7.3 (#1)                 48.8 (#1)                 
#  5 Paris Saint-Germain   2020      27 16.4 (#1)           18 (#1)                                 1.6 8.4 (#1)                 53.4 (#1)                 
#  6 Paris Saint-Germain   2017      38 15.1 (#2)           16.6 (#1)                               1.5 6.9 (#2)                 45.2 (#2)                 
#  7 Paris Saint-Germain   2019      37 14.5 (#2)           16 (#2)                                 1.5 6.7 (#1)                 47.2 (#1)                 
#  8 Paris Saint-Germain   2022      38 14.7 (#2)           16.1 (#2)                               1.4 5.9 (#2)                 44.5 (#1)                 
#  9 Stade Rennais         2022      36 14.8 (#1)           16.2 (#1)                               1.4 6.4 (#1)                 39.6 (#2)                 
# 10 Lens                  2023      35 13.6 (#5)           14.9 (#3)                               1.3 4.5 (#5)                 39.1 (#3)                 
#    Shots.Trail.Per.Game.Full Minutes.Trail.Per.Game.Full Shots.DownMen.Per.Game.Full Minutes.DownMen.Per.Game.Full Shots.UpMen.Per.Game.Full Minutes.UpMen.Per.Game.Full
#    <chr>                     <chr>                       <chr>                       <chr>                         <chr>                     <chr>                      
#  1 1.7 (#20)                 8.5 (#20)                   0.8 (#1)                    5.6 (#4)                      0.5 (#10)                 1.8 (#14)                  
#  2 1.5 (#19)                 8.8 (#20)                   0.1 (#18)                   2.1 (#15)                     0.7 (#10)                 2.7 (#14)                  
#  3 0.6 (#20)                 4.3 (#20)                   0.1 (#17)                   1.9 (#18)                     0.6 (#11)                 2.3 (#15)                  
#  4 2.2 (#19)                 13.5 (#19)                  0.7 (#2)                    5.7 (#4)                      0.9 (#8)                  4.6 (#10)                  
#  5 1.7 (#20)                 10.4 (#20)                  0 (#20)                     0.1 (#19)                     1.7 (#3)                  7.3 (#5)                   
#  6 1.3 (#20)                 9.1 (#19)                   0.2 (#16)                   1.5 (#17)                     0 (#20)                   0 (#20)                    
#  7 1.7 (#20)                 10.2 (#20)                  0.1 (#16)                   2.5 (#17)                     0.6 (#13)                 2.6 (#16)                  
#  8 2.4 (#18)                 14.6 (#20)                  0.2 (#15)                   2.1 (#18)                     0.8 (#11)                 4 (#12)                    
#  9 2.4 (#19)                 16.1 (#18)                  0.2 (#13)                   2.5 (#15)                     0.9 (#9)                  2.8 (#14)                  
# 10 2 (#20)                   10.1 (#20)                  0.7 (#1)                    5.3 (#5)                      0.2 (#16)                 1.6 (#16) 



## PREMIER LEAGUE

#    Team              season n.games Shots.Per.Game.Full Shots.Adj.Per.Game.Full Shots.Diff.Per.Game Shots.Lead.Per.Game.Full Minutes.Lead.Per.Game.Full
#    <chr>              <int>   <int> <chr>               <chr>                                 <dbl> <chr>                    <chr>                     
#  1 Manchester City     2013      20 19.4 (#2)           22.1 (#2)                               2.7 7.3 (#3)                 33.6 (#5)                 
#  2 Manchester City     2019      38 17.9 (#1)           20.5 (#1)                               2.6 11.2 (#1)                60.8 (#1)                 
#  3 Manchester City     2018      38 17.4 (#1)           19.7 (#1)                               2.3 8.7 (#1)                 53.6 (#1)                 
#  4 Manchester United   2010      18 19.2 (#2)           21.5 (#2)                               2.3 7.9 (#3)                 40.2 (#2)                 
#  5 Chelsea             2010      19 24.9 (#1)           27.1 (#1)                               2.2 10.8 (#1)                40.8 (#1)                 
#  6 Manchester City     2023      37 15.7 (#3)           17.8 (#1)                               2.1 7.1 (#1)                 52.1 (#1)                 
#  7 Manchester City     2014      36 17.4 (#2)           19.4 (#1)                               2   8.9 (#1)                 55.8 (#1)                 
#  8 Manchester City     2020      37 19.4 (#1)           21.4 (#1)                               2   8.8 (#1)                 46.4 (#2)                 
#  9 Manchester City     2022      37 18.7 (#2)           20.7 (#2)                               2   9.6 (#1)                 51.5 (#1)                 
# 10 Manchester City     2012       7 20.4 (#2)           22.3 (#1)                               1.9 8.4 (#1)                 39.3 (#2)                 
#    Shots.Trail.Per.Game.Full Minutes.Trail.Per.Game.Full Shots.DownMen.Per.Game.Full Minutes.DownMen.Per.Game.Full Shots.UpMen.Per.Game.Full Minutes.UpMen.Per.Game.Full
#    <chr>                     <chr>                       <chr>                       <chr>                         <chr>                     <chr>                      
#  1 2 (#16)                   7.8 (#19)                   1.4 (#1)                    6.8 (#1)                      0 (#16)                   0 (#16)                    
#  2 0.8 (#20)                 4.3 (#20)                   0 (#17)                     0.2 (#17)                     0.6 (#7)                  2.4 (#8)                   
#  3 1 (#20)                   4.6 (#20)                   0.3 (#5)                    1.3 (#11)                     0.2 (#12)                 1.8 (#8)                   
#  4 1.7 (#20)                 7.9 (#20)                   0.6 (#1)                    3.4 (#3)                      0 (#20)                   0 (#20)                    
#  5 3.6 (#11)                 13.3 (#17)                  0.3 (#4)                    2.2 (#8)                      0.9 (#2)                  2.5 (#5)                   
#  6 1.5 (#20)                 7.6 (#20)                   0.3 (#3)                    2.1 (#7)                      0.3 (#8)                  2.1 (#6)                   
#  7 2.1 (#19)                 8.4 (#19)                   0.2 (#9)                    2.5 (#9)                      1.1 (#3)                  4 (#6)                     
#  8 3.7 (#8)                  16.8 (#19)                  0.3 (#4)                    4.3 (#3)                      0.3 (#11)                 1.5 (#11)                  
#  9 2.3 (#17)                 10.8 (#18)                  0.2 (#6)                    1.5 (#12)                     1.4 (#1)                  4.4 (#2)                   
# 10 2.4 (#15)                 7.7 (#19)                   0.3 (#9)                    2.6 (#9)                      0 (#18)                   0 (#18) 










######
######
### CORNERS
######
######


### BUNDESLIGA

#    Team              season n.games Corners.Per.Game.Full Corners.Adj.Per.Game.Full Corners.Diff.Per.Game Corners.Lead.Per.Game.Full Minutes.Lead.Per.Game.Full
#    <chr>              <int>   <int> <chr>                 <chr>                                     <dbl> <chr>                      <chr>                     
#  1 Bayern Munich       2015      34 6.5 (#1)              7.5 (#1)                                    1   3.1 (#1)                   44.9 (#1)                 
#  2 Bayern Munich       2019      34 8.5 (#1)              9.5 (#1)                                    1   4.4 (#1)                   50.7 (#1)                 
#  3 Bayern Munich       2020      34 7.1 (#1)              8.1 (#1)                                    1   3.6 (#1)                   52.2 (#1)                 
#  4 Bayern Munich       2012      33 6.4 (#1)              7.2 (#1)                                    0.8 2.6 (#1)                   39.2 (#2)                 
#  5 Bayern Munich       2014      34 7.1 (#1)              7.9 (#1)                                    0.8 3.4 (#1)                   47.9 (#1)                 
#  6 Bayern Munich       2016      34 6.9 (#1)              7.7 (#1)                                    0.8 2.9 (#1)                   46.7 (#1)                 
#  7 Bayern Munich       2023      34 6.7 (#1)              7.5 (#1)                                    0.8 3.4 (#1)                   54.2 (#1)                 
#  8 Bayern Munich       2013      34 7.6 (#1)              8.3 (#1)                                    0.7 3.9 (#1)                   51 (#1)                   
#  9 Bayern Munich       2021      34 7 (#1)                7.7 (#1)                                    0.7 3.4 (#1)                   47.4 (#1)                 
# 10 Borussia Dortmund   2012      32 5 (#7)                5.6 (#3)                                    0.6 2.1 (#2)                   42.9 (#1)                 
#    Corners.Trail.Per.Game.Full Minutes.Trail.Per.Game.Full Corners.DownMen.Per.Game.Full Minutes.DownMen.Per.Game.Full Corners.UpMen.Per.Game.Full Minutes.UpMen.Per.Game.Full
#    <chr>                       <chr>                       <chr>                         <chr>                         <chr>                       <chr>                      
#  1 0.5 (#18)                   7.9 (#18)                   0.2 (#3)                      4.7 (#2)                      0.1 (#7)                    1 (#12)                    
#  2 0.7 (#17)                   9.1 (#18)                   0 (#10)                       1.1 (#11)                     0.4 (#1)                    3.2 (#3)                   
#  3 0.6 (#18)                   8.9 (#18)                   0.1 (#3)                      3.2 (#8)                      0.2 (#6)                    2.1 (#9)                   
#  4 0.8 (#16)                   11.4 (#17)                  0.2 (#2)                      2.9 (#5)                      0.1 (#11)                   2.2 (#10)                  
#  5 0.8 (#18)                   8.1 (#18)                   0 (#14)                       0.3 (#17)                     0.1 (#10)                   0.4 (#14)                  
#  6 0.6 (#18)                   6.3 (#18)                   0.1 (#4)                      1 (#12)                       0 (#14)                     0 (#17)                    
#  7 0.8 (#17)                   9.2 (#18)                   0 (#11)                       4.2 (#5)                      0 (#15)                     0 (#16)                    
#  8 0.7 (#18)                   5.5 (#18)                   0 (#13)                       0.8 (#17)                     0.4 (#2)                    5.2 (#5)                   
#  9 1.5 (#5)                    19.8 (#13)                  0.1 (#2)                      3.4 (#3)                      0 (#13)                     0 (#14)                    
# 10 0.4 (#18)                   6.3 (#18)                   0.1 (#8)                      1.4 (#13)                     0.1 (#12)                   0.7 (#14)


### SERIE A

#    Team           season n.games Corners.Per.Game.Full Corners.Adj.Per.Game.Full Corners.Diff.Per.Game Corners.Lead.Per.Game.Full Minutes.Lead.Per.Game.Full
#    <chr>           <int>   <int> <chr>                 <chr>                                     <dbl> <chr>                      <chr>                     
#  1 AC Milan         2012      33 6.7 (#2)              8.2 (#2)                                    1.5 2.8 (#1)                   42.4 (#1)                 
#  2 Internazionale   2022      37 6.5 (#1)              7.9 (#1)                                    1.4 2.8 (#1)                   43.6 (#3)                 
#  3 Juventus         2014      37 5.7 (#4)              7 (#1)                                      1.3 2.5 (#1)                   47.5 (#1)                 
#  4 Juventus         2017      38 5.8 (#8)              7.1 (#4)                                    1.3 2.6 (#2)                   47.1 (#1)                 
#  5 Napoli           2017      38 7 (#2)                8.3 (#2)                                    1.3 2.8 (#1)                   46.9 (#2)                 
#  6 Juventus         2015      38 5.8 (#7)              7 (#2)                                      1.2 2.3 (#1)                   44.6 (#1)                 
#  7 Juventus         2019      38 6.4 (#4)              7.6 (#2)                                    1.2 2.9 (#1)                   44.7 (#1)                 
#  8 AS Roma          2018      38 6.9 (#2)              8 (#2)                                      1.1 2.5 (#1)                   40.5 (#2)                 
#  9 Atalanta         2021      37 5.4 (#6)              6.5 (#3)                                    1.1 2.4 (#1)                   41.9 (#3)                 
# 10 Internazionale   2009      33 5.8 (#5)              6.9 (#1)                                    1.1 2.2 (#2)                   41.8 (#1)                 
#    Corners.Trail.Per.Game.Full Minutes.Trail.Per.Game.Full Corners.DownMen.Per.Game.Full Minutes.DownMen.Per.Game.Full Corners.UpMen.Per.Game.Full Minutes.UpMen.Per.Game.Full
#    <chr>                       <chr>                       <chr>                         <chr>                         <chr>                       <chr>                      
#  1 0.5 (#20)                   7.5 (#20)                   0.2 (#4)                      2 (#10)                       0 (#18)                     1.2 (#14)                  
#  2 0.7 (#20)                   12.5 (#19)                  0 (#18)                       0 (#20)                       0.2 (#6)                    1.7 (#14)                  
#  3 0.4 (#19)                   5.2 (#20)                   0.1 (#10)                     2.3 (#14)                     0.2 (#12)                   4 (#8)                     
#  4 0.8 (#20)                   9.6 (#20)                   0 (#20)                       0 (#20)                       0.1 (#11)                   2.2 (#14)                  
#  5 1.3 (#13)                   12.7 (#19)                  0.2 (#6)                      1.9 (#13)                     0.4 (#3)                    2.9 (#8)                   
#  6 0.2 (#20)                   3.8 (#20)                   0.1 (#12)                     1.3 (#16)                     0.1 (#18)                   1.4 (#17)                  
#  7 0.7 (#20)                   10.6 (#19)                  0.1 (#12)                     1.3 (#14)                     0.1 (#16)                   2 (#12)                    
#  8 1.1 (#14)                   14.2 (#17)                  0.1 (#14)                     3.7 (#6)                      0.5 (#1)                    5.3 (#4)                   
#  9 0.6 (#20)                   14.4 (#19)                  0 (#15)                       2.4 (#11)                     0.1 (#8)                    1.5 (#12)                  
# 10 0.2 (#20)                   3.5 (#20)                   0.2 (#6)                      3.3 (#12)                     0.3 (#8)                    1.7 (#15)


### LA LIGA

#    Team           season n.games Corners.Per.Game.Full Corners.Adj.Per.Game.Full Corners.Diff.Per.Game Corners.Lead.Per.Game.Full Minutes.Lead.Per.Game.Full
#    <chr>           <int>   <int> <chr>                 <chr>                                     <dbl> <chr>                      <chr>                     
#  1 Atltico Madrid   2011       1 10 (#1)               14.7 (#1)                                   4.7 9 (#1)                     79 (#1)                   
#  2 Barcelona        2012      29 7.3 (#1)              10 (#1)                                     2.7 4 (#1)                     53.3 (#1)                 
#  3 Real Madrid      2011      33 7.2 (#3)              9.6 (#3)                                    2.4 3.2 (#3)                   48.9 (#3)                 
#  4 Barcelona        2011      32 7.3 (#2)              9.6 (#2)                                    2.3 3.7 (#2)                   50.1 (#2)                 
#  5 Barcelona        2010      34 6.7 (#2)              8.9 (#1)                                    2.2 3.2 (#1)                   50.2 (#1)                 
#  6 Real Madrid      2014      38 6.7 (#3)              8.8 (#2)                                    2.1 3 (#2)                     47.3 (#1)                 
#  7 Real Madrid      2016      37 6.9 (#1)              9 (#1)                                      2.1 3.2 (#1)                   50.8 (#2)                 
#  8 Barcelona        2013      36 5.9 (#7)              7.9 (#3)                                    2   3.3 (#1)                   52.2 (#1)                 
#  9 Barcelona        2009      35 6.7 (#1)              8.6 (#1)                                    1.9 3 (#1)                     46.5 (#1)                 
# 10 Barcelona        2014      38 6.9 (#1)              8.8 (#1)                                    1.9 3.1 (#1)                   46.7 (#2)                 
#    Corners.Trail.Per.Game.Full Minutes.Trail.Per.Game.Full Corners.DownMen.Per.Game.Full Minutes.DownMen.Per.Game.Full Corners.UpMen.Per.Game.Full Minutes.UpMen.Per.Game.Full
#    <chr>                       <chr>                       <chr>                         <chr>                         <chr>                       <chr>                      
#  1 0 (#21)                     0 (#21)                     0 (#20)                       0 (#20)                       0 (#20)                     0 (#21)                    
#  2 0.8 (#18)                   6.7 (#18)                   0.3 (#1)                      2.8 (#13)                     0 (#19)                     0.9 (#17)                  
#  3 0.5 (#19)                   5.6 (#19)                   0.3 (#4)                      3.2 (#10)                     0.1 (#17)                   1.9 (#15)                  
#  4 0.5 (#20)                   4.2 (#20)                   0 (#19)                       0.4 (#19)                     0.4 (#6)                    4.8 (#6)                   
#  5 0.3 (#20)                   3.6 (#20)                   0.1 (#10)                     3.8 (#11)                     0.1 (#17)                   1.3 (#18)                  
#  6 0.8 (#19)                   12.2 (#18)                  0 (#17)                       2.3 (#13)                     0.1 (#17)                   1.5 (#20)                  
#  7 0.9 (#18)                   10.2 (#18)                  0.3 (#2)                      1.6 (#18)                     0.4 (#6)                    4.1 (#9)                   
#  8 0.5 (#19)                   8.6 (#19)                   0 (#18)                       2.3 (#16)                     0.3 (#9)                    3.6 (#12)                  
#  9 1.1 (#18)                   11.2 (#20)                  0.1 (#15)                     2.8 (#17)                     0.3 (#9)                    5 (#9)                     
# 10 0.9 (#16)                   10.8 (#19)                  0 (#18)                       0.4 (#19)                     0.1 (#18)                   1.9 (#19)  


### LIGUE 1

#    Team                season n.games Corners.Per.Game.Full Corners.Adj.Per.Game.Full Corners.Diff.Per.Game Corners.Lead.Per.Game.Full Minutes.Lead.Per.Game.Full
#    <chr>                <int>   <int> <chr>                 <chr>                                     <dbl> <chr>                      <chr>                     
#  1 Paris Saint-Germain   2018      38 6.7 (#1)              8.3 (#1)                                    1.6 3 (#1)                     48 (#1)                   
#  2 AS Monaco             2017      38 6.1 (#3)              7.4 (#2)                                    1.3 2.7 (#2)                   55.1 (#1)                 
#  3 Paris Saint-Germain   2017      38 7 (#1)                8.3 (#1)                                    1.3 2.8 (#1)                   45.2 (#2)                 
#  4 Paris Saint-Germain   2016      38 5.4 (#4)              6.6 (#1)                                    1.2 2.7 (#1)                   47.2 (#1)                 
#  5 Paris Saint-Germain   2020      27 6.7 (#1)              7.9 (#1)                                    1.2 3.2 (#1)                   53.4 (#1)                 
#  6 Paris Saint-Germain   2019      37 6.4 (#1)              7.5 (#1)                                    1.1 2.7 (#1)                   47.2 (#1)                 
#  7 Paris Saint-Germain   2023      38 5.4 (#3)              6.5 (#1)                                    1.1 2.6 (#1)                   48.8 (#1)                 
#  8 AS Monaco             2023      38 4.4 (#13)             5.3 (#8)                                    0.9 1.6 (#2)                   37.2 (#4)                 
#  9 Marseille             2018      38 6.4 (#2)              7.3 (#2)                                    0.9 2.2 (#2)                   39.1 (#3)                 
# 10 Paris Saint-Germain   2011      37 6.5 (#1)              7.4 (#1)                                    0.9 1.8 (#2)                   29.4 (#3)                 
#    Corners.Trail.Per.Game.Full Minutes.Trail.Per.Game.Full Corners.DownMen.Per.Game.Full Minutes.DownMen.Per.Game.Full Corners.UpMen.Per.Game.Full Minutes.UpMen.Per.Game.Full
#    <chr>                       <chr>                       <chr>                         <chr>                         <chr>                       <chr>                      
#  1 0.6 (#20)                   8.5 (#20)                   0.3 (#1)                      5.6 (#4)                      0.1 (#11)                   1.8 (#14)                  
#  2 0.7 (#18)                   8.8 (#20)                   0.1 (#10)                     2.1 (#15)                     0.2 (#12)                   2.7 (#14)                  
#  3 0.7 (#19)                   9.1 (#19)                   0.1 (#14)                     1.5 (#17)                     0 (#20)                     0 (#20)                    
#  4 0.3 (#20)                   4.3 (#20)                   0 (#19)                       1.9 (#18)                     0.2 (#12)                   2.3 (#15)                  
#  5 0.9 (#20)                   10.4 (#20)                  0 (#19)                       0.1 (#19)                     0.5 (#5)                    7.3 (#5)                   
#  6 0.8 (#19)                   10.2 (#20)                  0.1 (#16)                     2.5 (#17)                     0.3 (#6)                    2.6 (#16)                  
#  7 0.8 (#19)                   13.5 (#19)                  0.2 (#4)                      5.7 (#4)                      0.2 (#10)                   4.6 (#10)                  
#  8 0.7 (#20)                   17.9 (#16)                  0.2 (#1)                      7 (#2)                        0.2 (#12)                   3.2 (#12)                  
#  9 1.3 (#16)                   18.9 (#17)                  0.2 (#3)                      1.8 (#16)                     0.1 (#10)                   2.6 (#12)                  
# 10 1.4 (#8)                    17.1 (#14)                  0.4 (#2)                      4.9 (#2)                      0.2 (#11)                   1.8 (#13)  

### PREMIER LEAGUE

#    Team              season n.games Corners.Per.Game.Full Corners.Adj.Per.Game.Full Corners.Diff.Per.Game Corners.Lead.Per.Game.Full Minutes.Lead.Per.Game.Full
#    <chr>              <int>   <int> <chr>                 <chr>                                     <dbl> <chr>                      <chr>                     
#  1 Tottenham Hotspur   2012       8 10.1 (#2)             12.1 (#1)                                   2   3.4 (#2)                   35.5 (#4)                 
#  2 Manchester City     2019      38 7.9 (#1)              9.6 (#1)                                    1.7 4.5 (#1)                   60.8 (#1)                 
#  3 Manchester City     2018      38 7.5 (#1)              9 (#1)                                      1.5 3.6 (#1)                   53.6 (#1)                 
#  4 Manchester United   2010      18 8.7 (#2)              10.2 (#1)                                   1.5 3.6 (#1)                   40.2 (#2)                 
#  5 Manchester City     2013      20 7.2 (#4)              8.6 (#3)                                    1.4 2.7 (#4)                   33.6 (#5)                 
#  6 Manchester City     2022      37 8.2 (#1)              9.6 (#1)                                    1.4 4.2 (#1)                   51.5 (#1)                 
#  7 Liverpool           2019      38 6.6 (#2)              7.9 (#2)                                    1.3 3.7 (#2)                   49.7 (#2)                 
#  8 Manchester City     2012       7 9 (#3)                10.3 (#3)                                   1.3 3.6 (#1)                   39.3 (#2)                 
#  9 Manchester City     2023      37 6.3 (#2)              7.6 (#2)                                    1.3 3.1 (#1)                   52.1 (#1)                 
# 10 Liverpool           2022      36 7.5 (#2)              8.7 (#2)                                    1.2 3.6 (#2)                   48.7 (#2)                 
#    Corners.Trail.Per.Game.Full Minutes.Trail.Per.Game.Full Corners.DownMen.Per.Game.Full Minutes.DownMen.Per.Game.Full Corners.UpMen.Per.Game.Full Minutes.UpMen.Per.Game.Full
#    <chr>                       <chr>                       <chr>                         <chr>                         <chr>                       <chr>                      
#  1 1.2 (#12)                   8.9 (#16)                   1.1 (#1)                      5.1 (#5)                      0.2 (#6)                    3.6 (#5)                   
#  2 0.5 (#19)                   4.3 (#20)                   0 (#19)                       0.2 (#17)                     0.3 (#4)                    2.4 (#8)                   
#  3 0.4 (#20)                   4.6 (#20)                   0.1 (#7)                      1.3 (#11)                     0.1 (#9)                    1.8 (#8)                   
#  4 0.7 (#20)                   7.9 (#20)                   0.3 (#1)                      3.4 (#3)                      0 (#19)                     0 (#20)                    
#  5 0.7 (#19)                   7.8 (#19)                   0.5 (#1)                      6.8 (#1)                      0 (#15)                     0 (#16)                    
#  6 1.2 (#14)                   10.8 (#18)                  0.1 (#5)                      1.5 (#12)                     0.6 (#1)                    4.4 (#2)                   
#  7 0.2 (#20)                   4.8 (#19)                   0 (#18)                       0.6 (#16)                     0.1 (#14)                   0.6 (#16)                  
#  8 1.3 (#10)                   7.7 (#19)                   0 (#13)                       2.6 (#9)                      0 (#18)                     0 (#18)                    
#  9 0.5 (#20)                   7.6 (#20)                   0.2 (#3)                      2.1 (#7)                      0.2 (#7)                    2.1 (#6)                   
# 10 0.6 (#20)                   7.3 (#20)                   0 (#16)                       0.6 (#16)                     0.4 (#2)                    3.6 (#4)  