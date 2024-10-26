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
include.minute <- FALSE

league.name <- c("Bundesliga", "SerieA", "LaLiga", "Ligue1", "PremierLeague")

# load(file="gam_nb_obj_Shots.Robj")

load(file=paste0("gam_nb_obj",  
                 ifelse(remove.extra, "_NO_EXTRA_TIME", ""), 
                 "_Shots.Robj"))


for (league in league.name[2]){
  
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
    
    
    ## Calculating std. errors of all differences between predictions
    all.diff.SE <- c()
    for (score.diff in all.score.diffs){
      
      Xp <-  predict(gam.nb.obj[[league]],
                     newdata = data.frame(Minute.clean=45, Weighted.Win.Prob = 0, HomeAway = "Home", RedCard.Diff =0,
                                          Score.Diff = c(score.diff,0)),
                     type="lpmatrix")
      
      a <- c(1,-1)
      Xs <- t(a) %*% Xp
      all.diff.SE <- c(all.diff.SE,
                       sqrt(Xs %*% gam.nb.obj[[league]]$Vp %*% t(Xs)))
    }
    
    
    ### Getting the min-max of the difference via +-1.96xSE approach
    log.score.diff.effects.min <- log.score.diff.effects - qnorm(0.975)*all.diff.SE
    log.score.diff.effects.max <- log.score.diff.effects + qnorm(0.975)*all.diff.SE
    
    
    # Multiplicative effects
    exp(log.score.diff.effects)
    # Their 95% CIs:
    cbind(exp(log.score.diff.effects.min), 
          exp(log.score.diff.effects.max))
    
    # Doing the multiplication
    our.df.pred$Adj.Coef.ScoreDiff <- 
      exp(-log.score.diff.effects)[sapply(our.df.pred$Score.Diff, function(x) which(names(log.score.diff.effects) == x))]
    
    our.df.pred$Adj.Coef.ScoreDiff.Max <- 
      exp(-log.score.diff.effects.min)[sapply(our.df.pred$Score.Diff, function(x) which(names(log.score.diff.effects) == x))]
    
    our.df.pred$Adj.Coef.ScoreDiff.Min <- 
      exp(-log.score.diff.effects.max)[sapply(our.df.pred$Score.Diff, function(x) which(names(log.score.diff.effects) == x))]
    
    
    
    
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
    
    ## Calculating std. errors of all differences between predictions
    all.diff.SE <- c()
    for (redcard.diff in all.redcard.diffs){
      
      Xp <-  predict(gam.nb.obj[[league]],
                     newdata = data.frame(Minute.clean=45, Weighted.Win.Prob = 0, HomeAway = "Home", Score.Diff =0,
                                          RedCard.Diff = c(redcard.diff,0)),
                     type="lpmatrix")
      
      a <- c(1,-1)
      Xs <- t(a) %*% Xp
      all.diff.SE <- c(all.diff.SE,
                       sqrt(Xs %*% gam.nb.obj[[league]]$Vp %*% t(Xs)))
    }
    
    
    ### Getting the min-max of the difference via +-1.96xSE approach
    log.redcard.diff.effects.min <- log.redcard.diff.effects - qnorm(0.975)*all.diff.SE
    log.redcard.diff.effects.max <- log.redcard.diff.effects + qnorm(0.975)*all.diff.SE
    
    
    
    
    
    # Multiplicative effects
    exp(log.redcard.diff.effects)
    # Their 95% CIs:
    cbind(exp(log.redcard.diff.effects.min), 
          exp(log.redcard.diff.effects.max))
    
    # Doing the multiplication
    our.df.pred$Adj.Coef.RedCardDiff <- 
      exp(-log.redcard.diff.effects)[sapply(our.df.pred$RedCard.Diff, function(x) which(names(log.redcard.diff.effects) == x))]
    
    our.df.pred$Adj.Coef.RedCardDiff.Max <- 
      exp(-log.redcard.diff.effects.min)[sapply(our.df.pred$RedCard.Diff, function(x) which(names(log.redcard.diff.effects) == x))]
    
    our.df.pred$Adj.Coef.RedCardDiff.Min <- 
      exp(-log.redcard.diff.effects.max)[sapply(our.df.pred$RedCard.Diff, function(x) which(names(log.redcard.diff.effects) == x))]
    
    
    
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
    
    
    ## Calculating std. errors of all differences between predictions
    all.diff.SE <- c()
    for (homeaway in all.homeaway){
      
      Xp <-  predict(gam.nb.obj[[league]],
                     newdata = data.frame(Minute.clean=45, Weighted.Win.Prob = 0,  Score.Diff =0, RedCard.Diff=0,
                                          HomeAway = c(homeaway,"Home")),
                     type="lpmatrix")
      
      a <- c(1,-1)
      Xs <- t(a) %*% Xp
      all.diff.SE <- c(all.diff.SE,
                       sqrt(Xs %*% gam.nb.obj[[league]]$Vp %*% t(Xs)))
    }
    
    
    ### Getting the min-max of the difference via +-1.96xSE approach
    log.homeaway.effects.min <- log.homeaway.effects - qnorm(0.975)*all.diff.SE
    log.homeaway.effects.max <- log.homeaway.effects + qnorm(0.975)*all.diff.SE
    
    
    
    
    
    # Multiplicative effects
    exp(log.homeaway.effects)
    # Their 95% CIs:
    cbind(exp(log.homeaway.effects.min), 
          exp(log.homeaway.effects.max))
    
    # Doing the multiplication
    our.df.pred$Adj.Coef.HomeAway <- 
      exp(-log.homeaway.effects)[sapply(our.df.pred$HomeAway, function(x) which(names(log.homeaway.effects) == x))]
    
    our.df.pred$Adj.Coef.HomeAway.Max <- 
      exp(-log.homeaway.effects.min)[sapply(our.df.pred$HomeAway, function(x) which(names(log.homeaway.effects) == x))]
    
    our.df.pred$Adj.Coef.HomeAway.Min <- 
      exp(-log.homeaway.effects.max)[sapply(our.df.pred$HomeAway, function(x) which(names(log.homeaway.effects) == x))]
    
    
    
    
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
    
    
    ## Calculating std. errors of all differences between predictions
    all.diff.SE <- c()
    for (minute in all.minutes){
      
      Xp <-  predict(gam.nb.obj[[league]],
                     newdata = data.frame(HomeAway="Home", Weighted.Win.Prob = 0,  Score.Diff =0, RedCard.Diff=0,
                                          Minute.clean = c(minute,45)),
                     type="lpmatrix")
      
      a <- c(1,-1)
      Xs <- t(a) %*% Xp
      all.diff.SE <- c(all.diff.SE,
                       sqrt(Xs %*% gam.nb.obj[[league]]$Vp %*% t(Xs)))
    }
    
    
    ### Getting the min-max of the difference via +-1.96xSE approach
    log.minute.effects.min <- log.minute.effects - qnorm(0.975)*all.diff.SE
    log.minute.effects.max <- log.minute.effects + qnorm(0.975)*all.diff.SE
    
    
    
    # Multiplicative effects
    exp(log.minute.effects)
    # Their 95% CIs:
    cbind(exp(log.minute.effects.min), 
          exp(log.minute.effects.max))
    
    # Doing the multiplication
    our.df.pred$Adj.Coef.Minute <- 
      exp(-log.minute.effects)[sapply(our.df.pred$Minute.clean, function(x) which(names(log.minute.effects) == x))]
    
    our.df.pred$Adj.Coef.HomeAway.Max <- 
      exp(-log.minute.effects.min)[sapply(our.df.pred$Minute.clean, function(x) which(names(log.minute.effects) == x))]
    
    our.df.pred$Adj.Coef.HomeAway.Min <- 
      exp(-log.minute.effects.max)[sapply(our.df.pred$Minute.clean, function(x) which(names(log.minute.effects) == x))]
    
    
    
    
    
    our.df.pred <- our.df.pred %>%
      mutate(Shots.Adj = Shots*Adj.Coef.ScoreDiff*Adj.Coef.RedCardDiff*Adj.Coef.HomeAway*Adj.Coef.Minute
             #,Shots.Adj.No.Min = Shots*Adj.Coef.ScoreDiff*Adj.Coef.RedCardDiff*Adj.Coef.HomeAway
      )
    
  } else {
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
    
    
    ## Calculating std. errors of all differences between predictions
    all.diff.SE <- c()
    for (score.diff in all.score.diffs){
      
      Xp <-  predict(gam.nb.obj[[league]],
                     newdata = data.frame(Minute.clean=45, Weighted.Win.Prob = 0, HomeAway = "Home", RedCard.Diff =0,
                                          Score.Diff = c(score.diff,0)),
                     type="lpmatrix")
      
      a <- c(1,-1)
      Xs <- t(a) %*% Xp
      all.diff.SE <- c(all.diff.SE,
                       sqrt(Xs %*% gam.nb.obj[[league]]$Vp %*% t(Xs)))
    }
    
    
    ### Getting the min-max of the difference via +-1.96xSE approach
    log.score.diff.effects.min <- log.score.diff.effects - qnorm(0.975)*all.diff.SE
    log.score.diff.effects.max <- log.score.diff.effects + qnorm(0.975)*all.diff.SE
    
    
    # Multiplicative effects
    exp(log.score.diff.effects)
    # Their 95% CIs:
    cbind(exp(log.score.diff.effects.min), 
          exp(log.score.diff.effects.max))
    
    # Doing the multiplication
    our.df.pred$Adj.Coef.ScoreDiff <- 
      exp(-log.score.diff.effects)[sapply(our.df.pred$Score.Diff, function(x) which(names(log.score.diff.effects) == x))]
    
    our.df.pred$Adj.Coef.ScoreDiff.Max <- 
      exp(-log.score.diff.effects.min)[sapply(our.df.pred$Score.Diff, function(x) which(names(log.score.diff.effects) == x))]
    
    our.df.pred$Adj.Coef.ScoreDiff.Min <- 
      exp(-log.score.diff.effects.max)[sapply(our.df.pred$Score.Diff, function(x) which(names(log.score.diff.effects) == x))]
    
    
    
    
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
    
    ## Calculating std. errors of all differences between predictions
    all.diff.SE <- c()
    for (redcard.diff in all.redcard.diffs){
      
      Xp <-  predict(gam.nb.obj[[league]],
                     newdata = data.frame(Minute.clean=45, Weighted.Win.Prob = 0, HomeAway = "Home", Score.Diff =0,
                                          RedCard.Diff = c(redcard.diff,0)),
                     type="lpmatrix")
      
      a <- c(1,-1)
      Xs <- t(a) %*% Xp
      all.diff.SE <- c(all.diff.SE,
                       sqrt(Xs %*% gam.nb.obj[[league]]$Vp %*% t(Xs)))
    }
    
    
    ### Getting the min-max of the difference via +-1.96xSE approach
    log.redcard.diff.effects.min <- log.redcard.diff.effects - qnorm(0.975)*all.diff.SE
    log.redcard.diff.effects.max <- log.redcard.diff.effects + qnorm(0.975)*all.diff.SE
    
    
    
    
    
    # Multiplicative effects
    exp(log.redcard.diff.effects)
    # Their 95% CIs:
    cbind(exp(log.redcard.diff.effects.min), 
          exp(log.redcard.diff.effects.max))
    
    # Doing the multiplication
    our.df.pred$Adj.Coef.RedCardDiff <- 
      exp(-log.redcard.diff.effects)[sapply(our.df.pred$RedCard.Diff, function(x) which(names(log.redcard.diff.effects) == x))]
    
    our.df.pred$Adj.Coef.RedCardDiff.Max <- 
      exp(-log.redcard.diff.effects.min)[sapply(our.df.pred$RedCard.Diff, function(x) which(names(log.redcard.diff.effects) == x))]
    
    our.df.pred$Adj.Coef.RedCardDiff.Min <- 
      exp(-log.redcard.diff.effects.max)[sapply(our.df.pred$RedCard.Diff, function(x) which(names(log.redcard.diff.effects) == x))]
    
    
    
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
    
    
    ## Calculating std. errors of all differences between predictions
    all.diff.SE <- c()
    for (homeaway in all.homeaway){
      
      Xp <-  predict(gam.nb.obj[[league]],
                     newdata = data.frame(Minute.clean=45, Weighted.Win.Prob = 0,  Score.Diff =0, RedCard.Diff=0,
                                          HomeAway = c(homeaway,"Home")),
                     type="lpmatrix")
      
      a <- c(1,-1)
      Xs <- t(a) %*% Xp
      all.diff.SE <- c(all.diff.SE,
                       sqrt(Xs %*% gam.nb.obj[[league]]$Vp %*% t(Xs)))
    }
    
    
    ### Getting the min-max of the difference via +-1.96xSE approach
    log.homeaway.effects.min <- log.homeaway.effects - qnorm(0.975)*all.diff.SE
    log.homeaway.effects.max <- log.homeaway.effects + qnorm(0.975)*all.diff.SE
    
    
    
    
    
    # Multiplicative effects
    exp(log.homeaway.effects)
    # Their 95% CIs:
    cbind(exp(log.redcard.diff.effects.min), 
          exp(log.redcard.diff.effects.max))
    
    # Doing the multiplication
    our.df.pred$Adj.Coef.HomeAway <- 
      exp(-log.homeaway.effects)[sapply(our.df.pred$HomeAway, function(x) which(names(log.homeaway.effects) == x))]
    
    our.df.pred$Adj.Coef.HomeAway.Max <- 
      exp(-log.homeaway.effects.min)[sapply(our.df.pred$HomeAway, function(x) which(names(log.homeaway.effects) == x))]
    
    our.df.pred$Adj.Coef.HomeAway.Min <- 
      exp(-log.homeaway.effects.max)[sapply(our.df.pred$HomeAway, function(x) which(names(log.homeaway.effects) == x))]
    
    
    our.df.pred <- our.df.pred %>%
      mutate(Shots.Adj = Shots*Adj.Coef.ScoreDiff*Adj.Coef.RedCardDiff*Adj.Coef.HomeAway,
             Shots.Adj.Min = Shots*Adj.Coef.ScoreDiff.Min*Adj.Coef.RedCardDiff.Min*Adj.Coef.HomeAway.Min,
             Shots.Adj.Max = Shots*Adj.Coef.ScoreDiff.Max*Adj.Coef.RedCardDiff.Max*Adj.Coef.HomeAway.Max
             #,Shots.Adj.No.Min = Shots*Adj.Coef.ScoreDiff*Adj.Coef.RedCardDiff*Adj.Coef.HomeAway
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
    summarise(Shots = sum(Shots),
              Shots.Adj= sum(Shots.Adj),
              Shots.Adj.Min = sum(Shots.Adj.Min),
              Shots.Adj.Max = sum(Shots.Adj.Max),
              Shots.Diff = Shots.Adj - Shots,
              # Shots.Adj.No.Min = sum(Shots.Adj.No.Min),
              # Shots.Diff.No.Min = Shots.Adj.No.Min - Shots
    ) %>%
    ungroup() %>%
    group_by(Team, season) %>%
    summarise(Shots.Per.Game = round(mean(Shots),1),
              Shots.Adj.Per.Game = round(mean(Shots.Adj),1),
              Shots.Adj.Min.Per.Game = round(mean(Shots.Adj.Min),1),
              Shots.Adj.Max.Per.Game = round(mean(Shots.Adj.Max),1),
              Shots.Diff.Per.Game = round(Shots.Adj.Per.Game - Shots.Per.Game,1),
              n.games = length(unique(gameId))) %>%
    ungroup() %>%
    group_by(season) %>%
    mutate(Shots.Per.Game.Full = paste0(round(Shots.Per.Game,1), " (\\#", rank(-Shots.Per.Game, ties="first"), ")"),
           Shots.Adj.Per.Game.Full = paste0(round(Shots.Adj.Per.Game,1), ", [", Shots.Adj.Min.Per.Game, ", ", Shots.Adj.Max.Per.Game,  "]", " (\\#", rank(-Shots.Adj.Per.Game, ties="first"), ")")) %>%
    dplyr::select(Team, season, n.games, 
                  Shots.Per.Game.Full, Shots.Adj.Per.Game.Full, Shots.Diff.Per.Game) %>%
    arrange(desc(abs(Shots.Diff.Per.Game)))
  
  
  
  
  
  
  
  # print(head(final.df %>% filter(Shots.Diff.Per.Game > 0), 10))
  # print(head(final.df  %>% filter(Shots.Diff.Per.Game < 0), 10))
  
  # Shots in the lead (Minutes in the lead), Shots shorthanded (Minutes shorthanded)
  
  
  ## Getting Shots in the lead, Shots shorthanded
  
  scorediff.diff.ranks <- our.df.pred %>%
    # filter(Score.Diff > 0) %>%
    group_by(gameId, Team, season) %>%
    summarise(Shots.Lead = sum(Shots[Score.Diff > 0]),
              Minutes.Lead = sum(Score.Diff > 0),
              Shots.Trail = sum(Shots[Score.Diff < 0]),
              Minutes.Trail = sum(Score.Diff < 0)) %>%
    ungroup() %>%
    group_by(Team, season) %>%
    summarise(Shots.Lead.Per.Game = mean(Shots.Lead),
              Minutes.Lead.Per.Game = mean(Minutes.Lead),
              Shots.Trail.Per.Game = mean(Shots.Trail),
              Minutes.Trail.Per.Game = mean(Minutes.Trail),
              n.games = length(unique(gameId))) %>%
    ungroup() %>%
    #filter(season==2020)
    group_by(season) %>%
    mutate(Shots.Lead.Per.Game.Full = paste0(round(Shots.Lead.Per.Game,1), " (\\#", rank(-Shots.Lead.Per.Game, ties="first"), ")"),
           Minutes.Lead.Per.Game.Full = paste0(round(Minutes.Lead.Per.Game,1), " (\\#", rank(-Minutes.Lead.Per.Game, ties="first"), ")"),
           Shots.Trail.Per.Game.Full = paste0(round(Shots.Trail.Per.Game,1), " (\\#", rank(-Shots.Trail.Per.Game, ties="first"), ")"),
           Minutes.Trail.Per.Game.Full = paste0(round(Minutes.Trail.Per.Game,1), " (\\#", rank(-Minutes.Trail.Per.Game, ties="first"), ")")) %>%
    dplyr::select(Team, season, n.games, 
                  Shots.Lead.Per.Game.Full, Minutes.Lead.Per.Game.Full,
                  Shots.Trail.Per.Game.Full, Minutes.Trail.Per.Game.Full)
  # arrange(desc(Shots.Lead.Per.Game))
  
  
  redcard.diff.ranks <- our.df.pred %>%
    #  filter(RedCard.Diff > 0) %>%
    group_by(gameId, Team, season) %>%
    summarise(Shots.DownMen = sum(Shots[RedCard.Diff > 0]),
              Minutes.DownMen = sum(RedCard.Diff > 0),
              Shots.UpMen = sum(Shots[RedCard.Diff < 0]),
              Minutes.UpMen = sum(RedCard.Diff < 0)) %>%
    ungroup() %>%
    group_by(Team, season) %>%
    summarise(Shots.DownMen.Per.Game = mean(Shots.DownMen),
              Minutes.DownMen.Per.Game = mean(Minutes.DownMen),
              Shots.UpMen.Per.Game = mean(Shots.UpMen),
              Minutes.UpMen.Per.Game = mean(Minutes.UpMen),
              n.games = length(unique(gameId))) %>%
    ungroup() %>%
    #filter(season==2020)
    group_by(season) %>%
    mutate(Shots.DownMen.Per.Game.Full = paste0(round(Shots.DownMen.Per.Game,1), " (\\#", rank(-Shots.DownMen.Per.Game, ties="first"), ")"),
           Minutes.DownMen.Per.Game.Full = paste0(round(Minutes.DownMen.Per.Game,1), " (\\#", rank(-Minutes.DownMen.Per.Game, ties="first"), ")"),
           Shots.UpMen.Per.Game.Full = paste0(round(Shots.UpMen.Per.Game,1), " (\\#", rank(-Shots.UpMen.Per.Game, ties="first"), ")"),
           Minutes.UpMen.Per.Game.Full = paste0(round(Minutes.UpMen.Per.Game,1), " (\\#", rank(-Minutes.UpMen.Per.Game, ties="first"), ")")) %>%
    dplyr::select(Team, season, n.games, 
                  Shots.DownMen.Per.Game.Full, Minutes.DownMen.Per.Game.Full,
                  Shots.UpMen.Per.Game.Full, Minutes.UpMen.Per.Game.Full)
  #  arrange(desc(Shots.DownMen.Per.Game))
  
  
  options(tibble.width=Inf)
  print(head(final.df %>% left_join(scorediff.diff.ranks) %>% left_join(redcard.diff.ranks) %>%
               filter(n.games > 30) %>%
               filter(Shots.Diff.Per.Game > 0), 10))
  
  print(final.df %>%
          filter(n.games > 30) %>%
          left_join(scorediff.diff.ranks) %>% left_join(redcard.diff.ranks) %>%
         filter(Shots.Diff.Per.Game > 0) %>%
    mutate(latex.entry = paste(league, Team, season, Shots.Per.Game.Full, Shots.Adj.Per.Game.Full,
                               Shots.Lead.Per.Game.Full, Shots.Trail.Per.Game.Full,
                               Shots.DownMen.Per.Game.Full, Shots.UpMen.Per.Game.Full, sep = " & "))%>%
   .[["latex.entry"]] %>%
    head(10))
  
  # print(head(final.df, 50))
  
  # our.df %>%
  #   filter(gameId == head(final.df$gameId, 1), Team == head(final.df$Team, 1))
  
  
  
  
  cat("\n")
  cat("\n")
}


#####
## SHOTS, NICE LATEX ENTRIES
##### 

# [1] Bundesliga & Bayern Munich & 2015 & 17.1 (\#1) & 19.5, [19, 20] (\#1) & 8.8 (\#1) & 1.5 (\#17) & 0.5 (\#2) & 0.3 (\#7)
# [2] Bundesliga & Bayern Munich & 2023 & 18.4 (\#1) & 20.8, [20.3, 21.3] (\#1) & 9.6 (\#1) & 2.2 (\#17) & 0.4 (\#2) & 0 (\#16)
# [3] Bundesliga & Bayern Munich & 2020 & 18 (\#1) & 20.3, [19.8, 20.8] (\#1) & 10.2 (\#1) & 1.8 (\#18) & 0.3 (\#6) & 0.6 (\#7)
# [4] Bundesliga & Bayern Munich & 2022 & 19.5 (\#1) & 21.8, [21.3, 22.3] (\#1) & 9.2 (\#1) & 2.4 (\#17) & 0.1 (\#6) & 0 (\#12)
# [5] Bundesliga & Bayern Munich & 2021 & 17 (\#1) & 19.2, [18.6, 19.7] (\#1) & 8.3 (\#1) & 3.8 (\#7) & 0.6 (\#1) & 0 (\#14)
# [6] Bundesliga & Bayern Munich & 2014 & 18.5 (\#1) & 20.6, [20.2, 21] (\#1) & 9.3 (\#1) & 1.8 (\#18) & 0 (\#16) & 0.2 (\#12)
# [7] Bundesliga & Borussia Dortmund & 2013 & 15.9 (\#3) & 18, [17.6, 18.4] (\#2) & 7.7 (\#2) & 2.8 (\#13) & 0.9 (\#1) & 0.4 (\#10)
# [8] Bundesliga & Bayern Munich & 2016 & 18.3 (\#1) & 20.3, [20, 20.7] (\#1) & 9 (\#1) & 1.6 (\#18) & 0.2 (\#6) & 0 (\#17)
# [9] Bundesliga & Bayern Munich & 2019 & 18.5 (\#1) & 20.4, [19.9, 20.8] (\#1) & 9.5 (\#1) & 2 (\#18) & 0.1 (\#12) & 0.9 (\#3)
# [10] Bundesliga & Bayern Munich & 2012 & 15.7 (\#2) & 17.5, [17, 18] (\#2) & 6.8 (\#2) & 2.5 (\#16) & 0.5 (\#3) & 0.4 (\#10)
# 
# 
# [1] SerieA & Napoli & 2017 & 17.6 (\#2) & 20.2, [19.8, 20.6] (\#1) & 8.1 (\#1) & 2.5 (\#18) & 0.2 (\#7) & 0.7 (\#5)
# [2] SerieA & AC Milan & 2012 & 15.8 (\#3) & 18.1, [17.8, 18.5] (\#3) & 6.4 (\#2) & 2 (\#19) & 0.5 (\#4) & 0.2 (\#15)
# [3] SerieA & Atalanta & 2021 & 16.1 (\#2) & 18.4, [18.1, 18.8] (\#1) & 7.6 (\#1) & 2.3 (\#20) & 0.2 (\#11) & 0.3 (\#12)
# [4] SerieA & Napoli & 2018 & 17.1 (\#1) & 19.4, [19, 19.8] (\#1) & 6.8 (\#1) & 2.4 (\#18) & 0.2 (\#12) & 0 (\#20)
# [5] SerieA & AS Roma & 2017 & 17.6 (\#1) & 19.8, [19.5, 20.2] (\#2) & 7.8 (\#2) & 2.6 (\#17) & 0 (\#17) & 0.1 (\#20)
# [6] SerieA & Internazionale & 2009 & 14.7 (\#7) & 16.9, [16.6, 17.2] (\#3) & 6.1 (\#1) & 0.6 (\#20) & 0.5 (\#8) & 0.5 (\#13)
# [7] SerieA & Internazionale & 2022 & 17.5 (\#1) & 19.7, [19.2, 20.1] (\#1) & 7.5 (\#1) & 2.8 (\#18) & 0 (\#19) & 0.5 (\#8)
# [8] SerieA & Internazionale & 2020 & 16.1 (\#6) & 18.2, [17.8, 18.5] (\#5) & 6.9 (\#1) & 2.5 (\#19) & 0.2 (\#13) & 0.6 (\#8)
# [9] SerieA & Juventus & 2012 & 19.1 (\#1) & 21.1, [20.8, 21.4] (\#1) & 6.6 (\#1) & 1.5 (\#20) & 0.4 (\#7) & 0.7 (\#8)
# [10] SerieA & Juventus & 2014 & 15.5 (\#3) & 17.5, [17.2, 17.8] (\#1) & 6.5 (\#1) & 0.9 (\#19) & 0.2 (\#12) & 0.7 (\#11)
# 
# 
# 
# [1] LaLiga & Real Madrid & 2011 & 19.9 (\#2) & 24.2, [23.5, 24.9] (\#2) & 10.2 (\#2) & 1.9 (\#19) & 0.7 (\#2) & 0.4 (\#15)
# [2] LaLiga & Real Madrid & 2015 & 17.7 (\#1) & 21.3, [20.7, 21.9] (\#1) & 8.9 (\#1) & 2 (\#18) & 0.3 (\#9) & 0 (\#20)
# [3] LaLiga & Barcelona & 2010 & 15.9 (\#3) & 19.4, [18.9, 19.8] (\#2) & 8.6 (\#2) & 0.7 (\#20) & 0.6 (\#3) & 0.4 (\#17)
# [4] LaLiga & Real Madrid & 2010 & 21.6 (\#1) & 25, [24.4, 25.5] (\#1) & 9.9 (\#1) & 2.9 (\#14) & 0.6 (\#4) & 1.4 (\#4)
# [5] LaLiga & Real Madrid & 2013 & 18.1 (\#1) & 21.2, [20.8, 21.7] (\#1) & 8.9 (\#1) & 2.6 (\#17) & 0.8 (\#3) & 0.7 (\#12)
# [6] LaLiga & Barcelona & 2009 & 18.3 (\#1) & 21.3, [20.7, 21.8] (\#1) & 8.8 (\#1) & 2.6 (\#17) & 0.4 (\#12) & 1.2 (\#8)
# [7] LaLiga & Barcelona & 2015 & 16.1 (\#2) & 19.1, [18.5, 19.6] (\#2) & 8.2 (\#2) & 1.5 (\#19) & 0.3 (\#11) & 1 (\#3)
# [8] LaLiga & Real Madrid & 2014 & 19.3 (\#1) & 22.3, [21.8, 22.8] (\#1) & 9.1 (\#1) & 2.8 (\#15) & 0.1 (\#16) & 0.5 (\#15)
# [9] LaLiga & Real Madrid & 2016 & 18.6 (\#1) & 21.6, [21, 22.3] (\#1) & 8.9 (\#1) & 2.1 (\#18) & 0.3 (\#7) & 1.1 (\#4)
# [10] LaLiga & Real Madrid & 2017 & 17.3 (\#1) & 20.3, [19.8, 20.8] (\#1) & 8.2 (\#1) & 1.8 (\#18) & 0.5 (\#3) & 0.4 (\#11)
# 
# 
# [1] Ligue1 & Paris Saint-Germain & 2018 & 16.1 (\#2) & 19.2, [18.6, 19.8] (\#1) & 8.2 (\#1) & 1.7 (\#20) & 0.8 (\#1) & 0.5 (\#10)
# [2] Ligue1 & Paris Saint-Germain & 2016 & 14.7 (\#1) & 17, [16.6, 17.6] (\#1) & 7.4 (\#1) & 0.6 (\#20) & 0.1 (\#17) & 0.6 (\#11)
# [3] Ligue1 & Paris Saint-Germain & 2017 & 15.1 (\#2) & 17.4, [17, 17.8] (\#1) & 6.9 (\#2) & 1.3 (\#20) & 0.2 (\#16) & 0 (\#20)
# [4] Ligue1 & Paris Saint-Germain & 2023 & 14.9 (\#1) & 17.2, [16.7, 17.6] (\#1) & 7.3 (\#1) & 2.2 (\#19) & 0.7 (\#2) & 0.9 (\#8)
# [5] Ligue1 & AS Monaco & 2017 & 14.5 (\#3) & 16.7, [16.3, 17.2] (\#2) & 7.7 (\#1) & 1.5 (\#19) & 0.1 (\#18) & 0.7 (\#10)
# [6] Ligue1 & Paris Saint-Germain & 2019 & 14.5 (\#2) & 16.5, [16.1, 16.9] (\#2) & 6.7 (\#1) & 1.7 (\#20) & 0.1 (\#16) & 0.6 (\#13)
# [7] Ligue1 & Stade Rennais & 2022 & 14.8 (\#1) & 16.6, [16.2, 17] (\#1) & 6.4 (\#1) & 2.4 (\#19) & 0.2 (\#13) & 0.9 (\#9)
# [8] Ligue1 & Lens & 2023 & 13.6 (\#5) & 15.3, [15, 15.6] (\#3) & 4.5 (\#5) & 2 (\#20) & 0.7 (\#1) & 0.2 (\#16)
# [9] Ligue1 & Lyon & 2009 & 15.2 (\#3) & 16.9, [16.6, 17.2] (\#2) & 4.7 (\#1) & 2.4 (\#17) & 0.6 (\#1) & 0.3 (\#11)
# [10] Ligue1 & Lyon & 2011 & 14.5 (\#4) & 16.2, [15.9, 16.5] (\#1) & 4.9 (\#1) & 2.1 (\#18) & 0.5 (\#3) & 0.1 (\#18)
# 
# 
# [1] PremierLeague & Manchester City & 2019 & 17.9 (\#1) & 21, [20.5, 21.4] (\#1) & 11.2 (\#1) & 0.8 (\#20) & 0 (\#17) & 0.6 (\#7)
# [2] PremierLeague & Manchester City & 2018 & 17.4 (\#1) & 20.2, [19.8, 20.6] (\#1) & 8.7 (\#1) & 1 (\#20) & 0.3 (\#5) & 0.2 (\#12)
# [3] PremierLeague & Manchester City & 2020 & 19.4 (\#1) & 22, [21.4, 22.6] (\#1) & 8.8 (\#1) & 3.7 (\#8) & 0.3 (\#4) & 0.3 (\#11)
# [4] PremierLeague & Manchester City & 2022 & 18.7 (\#2) & 21.1, [20.6, 21.7] (\#2) & 9.6 (\#1) & 2.3 (\#17) & 0.2 (\#6) & 1.4 (\#1)
# [5] PremierLeague & Liverpool & 2019 & 14.9 (\#3) & 17.2, [16.9, 17.5] (\#3) & 7.6 (\#2) & 0.8 (\#19) & 0.1 (\#10) & 0.2 (\#13)
# [6] PremierLeague & Liverpool & 2022 & 18.9 (\#1) & 21.2, [20.8, 21.6] (\#1) & 9.6 (\#2) & 1.8 (\#19) & 0 (\#17) & 0.9 (\#4)
# [7] PremierLeague & Manchester City & 2014 & 17.4 (\#2) & 19.6, [19.2, 20.1] (\#1) & 8.9 (\#1) & 2.1 (\#19) & 0.2 (\#9) & 1.1 (\#3)
# [8] PremierLeague & Manchester City & 2023 & 15.7 (\#3) & 17.9, [17.6, 18.3] (\#1) & 7.1 (\#1) & 1.5 (\#20) & 0.3 (\#3) & 0.3 (\#8)
# [9] PremierLeague & Tottenham Hotspur & 2017 & 17.3 (\#1) & 19.5, [19.2, 19.9] (\#1) & 7.6 (\#1) & 1.7 (\#20) & 0 (\#19) & 0.3 (\#10)
# [10] PremierLeague & Liverpool & 2018 & 16.7 (\#2) & 18.8, [18.4, 19.1] (\#2) & 7.2 (\#2) & 2.2 (\#19) & 0 (\#17) & 0 (\#17)




# #####
# ## CORNERS, NICE LATEX ENTRIES
# ##### 


# [1] Bundesliga & Bayern Munich & 2015 & 6.5 (\#1) & 7.7, [7.4, 8] (\#1) & 3.1 (\#1) & 0.5 (\#18) & 0.2 (\#3) & 0.1 (\#7)
# [2] Bundesliga & Bayern Munich & 2019 & 8.5 (\#1) & 9.7, [9.4, 10.1] (\#1) & 4.4 (\#1) & 0.7 (\#17) & 0 (\#10) & 0.4 (\#1)
# [3] Bundesliga & Bayern Munich & 2020 & 7.1 (\#1) & 8.3, [8, 8.7] (\#1) & 3.6 (\#1) & 0.6 (\#18) & 0.1 (\#3) & 0.2 (\#6)
# [4] Bundesliga & Bayern Munich & 2014 & 7.1 (\#1) & 8.2, [7.9, 8.4] (\#1) & 3.4 (\#1) & 0.8 (\#18) & 0 (\#14) & 0.1 (\#10)
# [5] Bundesliga & Bayern Munich & 2023 & 6.7 (\#1) & 7.8, [7.6, 8.1] (\#1) & 3.4 (\#1) & 0.8 (\#17) & 0 (\#11) & 0 (\#15)
# [6] Bundesliga & Bayern Munich & 2012 & 6.4 (\#1) & 7.4, [7, 7.8] (\#1) & 2.6 (\#1) & 0.8 (\#16) & 0.2 (\#2) & 0.1 (\#11)
# [7] Bundesliga & Bayern Munich & 2013 & 7.6 (\#1) & 8.6, [8.3, 9] (\#1) & 3.9 (\#1) & 0.7 (\#18) & 0 (\#13) & 0.4 (\#2)
# [8] Bundesliga & Bayern Munich & 2021 & 7 (\#1) & 8, [7.7, 8.4] (\#1) & 3.4 (\#1) & 1.5 (\#5) & 0.1 (\#2) & 0 (\#13)
# [9] Bundesliga & Bayern Munich & 2016 & 6.9 (\#1) & 7.8, [7.6, 8] (\#1) & 2.9 (\#1) & 0.6 (\#18) & 0.1 (\#4) & 0 (\#14)
# [10] Bundesliga & Bayern Munich & 2017 & 7.2 (\#1) & 8, [7.7, 8.3] (\#1) & 3.1 (\#1) & 1.1 (\#12) & 0 (\#12) & 0.2 (\#6)
# 
# 
# [1] SerieA & AC Milan & 2012 & 6.7 (\#2) & 8.4, [8.1, 8.7] (\#2) & 2.8 (\#1) & 0.5 (\#20) & 0.2 (\#4) & 0 (\#18)
# [2] SerieA & Internazionale & 2022 & 6.5 (\#1) & 8, [7.7, 8.4] (\#1) & 2.8 (\#1) & 0.7 (\#20) & 0 (\#18) & 0.2 (\#6)
# [3] SerieA & Napoli & 2017 & 7 (\#2) & 8.5, [8.2, 8.8] (\#2) & 2.8 (\#1) & 1.3 (\#13) & 0.2 (\#6) & 0.4 (\#3)
# [4] SerieA & Juventus & 2017 & 5.8 (\#8) & 7.2, [7, 7.4] (\#4) & 2.6 (\#2) & 0.8 (\#20) & 0 (\#20) & 0.1 (\#11)
# [5] SerieA & Atalanta & 2021 & 5.4 (\#6) & 6.7, [6.4, 6.9] (\#1) & 2.4 (\#1) & 0.6 (\#20) & 0 (\#15) & 0.1 (\#8)
# [6] SerieA & Juventus & 2014 & 5.7 (\#4) & 7, [6.8, 7.3] (\#1) & 2.5 (\#1) & 0.4 (\#19) & 0.1 (\#10) & 0.2 (\#12)
# [7] SerieA & Juventus & 2015 & 5.8 (\#7) & 7.1, [6.9, 7.3] (\#1) & 2.3 (\#1) & 0.2 (\#20) & 0.1 (\#12) & 0.1 (\#18)
# [8] SerieA & Juventus & 2019 & 6.4 (\#4) & 7.7, [7.5, 7.9] (\#2) & 2.9 (\#1) & 0.7 (\#20) & 0.1 (\#12) & 0.1 (\#16)
# [9] SerieA & Internazionale & 2018 & 7.7 (\#1) & 8.9, [8.6, 9.2] (\#1) & 2.5 (\#2) & 1.7 (\#8) & 0.1 (\#13) & 0.2 (\#13)
# [10] SerieA & Juventus & 2012 & 7.4 (\#1) & 8.6, [8.4, 8.9] (\#1) & 2.4 (\#2) & 0.7 (\#19) & 0.3 (\#2) & 0.1 (\#11)
# 
# 
# [1] LaLiga & Real Madrid & 2011 & 7.2 (\#3) & 9.3, [8.9, 9.7] (\#3) & 3.2 (\#3) & 0.5 (\#19) & 0.3 (\#4) & 0.1 (\#17)
# [2] LaLiga & Barcelona & 2011 & 7.3 (\#2) & 9.3, [8.9, 9.7] (\#2) & 3.7 (\#2) & 0.5 (\#20) & 0 (\#19) & 0.4 (\#6)
# [3] LaLiga & Barcelona & 2010 & 6.7 (\#2) & 8.5, [8.3, 8.8] (\#1) & 3.2 (\#1) & 0.3 (\#20) & 0.1 (\#10) & 0.1 (\#17)
# [4] LaLiga & Barcelona & 2013 & 5.9 (\#7) & 7.7, [7.4, 8] (\#3) & 3.3 (\#1) & 0.5 (\#19) & 0 (\#18) & 0.3 (\#9)
# [5] LaLiga & Real Madrid & 2016 & 6.9 (\#1) & 8.7, [8.3, 9.2] (\#1) & 3.2 (\#1) & 0.9 (\#18) & 0.3 (\#2) & 0.4 (\#6)
# [6] LaLiga & Barcelona & 2014 & 6.9 (\#1) & 8.6, [8.2, 8.9] (\#1) & 3.1 (\#1) & 0.9 (\#16) & 0 (\#18) & 0.1 (\#18)
# [7] LaLiga & Real Madrid & 2013 & 6.4 (\#3) & 8.1, [7.8, 8.3] (\#1) & 3.1 (\#2) & 0.9 (\#18) & 0.2 (\#6) & 0.3 (\#11)
# [8] LaLiga & Real Madrid & 2014 & 6.7 (\#3) & 8.4, [8.1, 8.7] (\#2) & 3 (\#2) & 0.8 (\#19) & 0 (\#17) & 0.1 (\#17)
# [9] LaLiga & Barcelona & 2009 & 6.7 (\#1) & 8.3, [8, 8.6] (\#1) & 3 (\#1) & 1.1 (\#18) & 0.1 (\#15) & 0.3 (\#9)
# [10] LaLiga & Real Madrid & 2010 & 6 (\#6) & 7.5, [7.2, 7.7] (\#4) & 2.7 (\#2) & 0.8 (\#19) & 0.1 (\#15) & 0.4 (\#6)
# 
# 
# [1] Ligue1 & Paris Saint-Germain & 2018 & 6.7 (\#1) & 8.6, [8.2, 9] (\#1) & 3 (\#1) & 0.6 (\#20) & 0.3 (\#1) & 0.1 (\#11)
# [2] Ligue1 & Paris Saint-Germain & 2017 & 7 (\#1) & 8.5, [8.2, 8.7] (\#1) & 2.8 (\#1) & 0.7 (\#19) & 0.1 (\#14) & 0 (\#20)
# [3] Ligue1 & AS Monaco & 2017 & 6.1 (\#3) & 7.4, [7.1, 7.7] (\#2) & 2.7 (\#2) & 0.7 (\#18) & 0.1 (\#10) & 0.2 (\#12)
# [4] Ligue1 & Paris Saint-Germain & 2016 & 5.4 (\#4) & 6.7, [6.4, 7] (\#1) & 2.7 (\#1) & 0.3 (\#20) & 0 (\#19) & 0.2 (\#12)
# [5] Ligue1 & Paris Saint-Germain & 2019 & 6.4 (\#1) & 7.7, [7.4, 8] (\#1) & 2.7 (\#1) & 0.8 (\#19) & 0.1 (\#16) & 0.3 (\#6)
# [6] Ligue1 & Paris Saint-Germain & 2023 & 5.4 (\#3) & 6.7, [6.4, 6.9] (\#1) & 2.6 (\#1) & 0.8 (\#19) & 0.2 (\#4) & 0.2 (\#10)
# [7] Ligue1 & Marseille & 2018 & 6.4 (\#2) & 7.5, [7.2, 7.7] (\#2) & 2.2 (\#2) & 1.3 (\#16) & 0.2 (\#3) & 0.1 (\#10)
# [8] Ligue1 & Paris Saint-Germain & 2015 & 4.5 (\#12) & 5.5, [5.4, 5.7] (\#5) & 2.1 (\#1) & 0.4 (\#20) & 0 (\#19) & 0 (\#19)
# [9] Ligue1 & Paris Saint-Germain & 2021 & 5.8 (\#2) & 6.8, [6.6, 7] (\#2) & 2.4 (\#1) & 0.4 (\#20) & 0.1 (\#15) & 0.7 (\#1)
# [10] Ligue1 & AS Monaco & 2021 & 5.7 (\#3) & 6.6, [6.4, 6.8] (\#3) & 2.2 (\#2) & 1.2 (\#17) & 0.1 (\#11) & 0.5 (\#4)
# 
# 
# [1] PremierLeague & Manchester City & 2019 & 7.9 (\#1) & 9.6, [9.3, 9.9] (\#1) & 4.5 (\#1) & 0.5 (\#19) & 0 (\#19) & 0.3 (\#4)
# [2] PremierLeague & Manchester City & 2018 & 7.5 (\#1) & 9.1, [8.8, 9.4] (\#1) & 3.6 (\#1) & 0.4 (\#20) & 0.1 (\#7) & 0.1 (\#9)
# [3] PremierLeague & Liverpool & 2019 & 6.6 (\#2) & 8.1, [7.8, 8.3] (\#2) & 3.7 (\#2) & 0.2 (\#20) & 0 (\#18) & 0.1 (\#14)
# [4] PremierLeague & Manchester City & 2022 & 8.2 (\#1) & 9.7, [9.3, 10] (\#1) & 4.2 (\#1) & 1.2 (\#14) & 0.1 (\#5) & 0.6 (\#1)
# [5] PremierLeague & Manchester City & 2023 & 6.3 (\#2) & 7.6, [7.4, 7.9] (\#2) & 3.1 (\#1) & 0.5 (\#20) & 0.2 (\#3) & 0.2 (\#7)
# [6] PremierLeague & Liverpool & 2020 & 6.8 (\#2) & 8, [7.8, 8.3] (\#2) & 3.3 (\#1) & 0.9 (\#19) & 0 (\#18) & 0 (\#18)
# [7] PremierLeague & Liverpool & 2022 & 7.5 (\#2) & 8.7, [8.4, 8.9] (\#2) & 3.6 (\#2) & 0.6 (\#20) & 0 (\#16) & 0.4 (\#2)
# [8] PremierLeague & Manchester City & 2014 & 7.3 (\#1) & 8.5, [8.2, 8.9] (\#1) & 3.5 (\#1) & 1 (\#16) & 0.1 (\#5) & 0.5 (\#2)
# [9] PremierLeague & Manchester City & 2021 & 6.4 (\#2) & 7.5, [7.3, 7.8] (\#1) & 2.9 (\#1) & 0.8 (\#20) & 0.1 (\#6) & 0.2 (\#10)
# [10] PremierLeague & Tottenham Hotspur & 2017 & 7.2 (\#2) & 8.3, [8.1, 8.5] (\#1) & 2.7 (\#1) & 0.7 (\#20) & 0 (\#19) & 0.1 (\#11)



######
######
### SHOTS
######
######


### BUNDESLIGA

# Team              season n.games Shots.Per.Game.Full Shots.Adj.Per.Game.Full     Shots.Diff.Per.Game Shots.Lead.Per.Game.Full Minutes.Lead.Per.Game.Full
# <chr>              <int>   <int> <chr>               <chr>                                     <dbl> <chr>                    <chr>                     
#   1 Bayern Munich       2015      34 "17.1 (\\#1)"       "19.5, [19, 20] (\\#1)"                     2.4 "8.8 (\\#1)"             "44.9 (\\#1)"             
# 2 Bayern Munich       2023      34 "18.4 (\\#1)"       "20.8, [20.3, 21.3] (\\#1)"                 2.4 "9.6 (\\#1)"             "54.2 (\\#1)"             
# 3 Bayern Munich       2020      34 "18 (\\#1)"         "20.3, [19.8, 20.8] (\\#1)"                 2.3 "10.2 (\\#1)"            "52.2 (\\#1)"             
# 4 Bayern Munich       2022      34 "19.5 (\\#1)"       "21.8, [21.3, 22.3] (\\#1)"                 2.3 "9.2 (\\#1)"             "45.7 (\\#1)"             
# 5 Bayern Munich       2021      34 "17 (\\#1)"         "19.2, [18.6, 19.7] (\\#1)"                 2.2 "8.3 (\\#1)"             "47.4 (\\#1)"             
# 6 Bayern Munich       2014      34 "18.5 (\\#1)"       "20.6, [20.2, 21] (\\#1)"                   2.1 "9.3 (\\#1)"             "47.9 (\\#1)"             
# 7 Borussia Dortmund   2013      34 "15.9 (\\#3)"       "18, [17.6, 18.4] (\\#2)"                   2.1 "7.7 (\\#2)"             "48.2 (\\#2)"             
# 8 Bayern Munich       2016      34 "18.3 (\\#1)"       "20.3, [20, 20.7] (\\#1)"                   2   "9 (\\#1)"               "46.7 (\\#1)"             
# 9 Bayern Munich       2019      34 "18.5 (\\#1)"       "20.4, [19.9, 20.8] (\\#1)"                 1.9 "9.5 (\\#1)"             "50.7 (\\#1)"             
# 10 Bayern Munich       2012      33 "15.7 (\\#2)"       "17.5, [17, 18] (\\#2)"                     1.8 "6.8 (\\#2)"             "39.2 (\\#2)"             
# Shots.Trail.Per.Game.Full Minutes.Trail.Per.Game.Full Shots.DownMen.Per.Game.Full Minutes.DownMen.Per.Game.Full Shots.UpMen.Per.Game.Full Minutes.UpMen.Per.Game.Full
# <chr>                     <chr>                       <chr>                       <chr>                         <chr>                     <chr>                      
#   1 "1.5 (\\#17)"             "7.9 (\\#18)"               "0.5 (\\#2)"                "4.7 (\\#2)"                  "0.3 (\\#7)"              "1 (\\#12)"                
# 2 "2.2 (\\#17)"             "9.2 (\\#18)"               "0.4 (\\#2)"                "4.2 (\\#5)"                  "0 (\\#16)"               "0 (\\#16)"                
# 3 "1.8 (\\#18)"             "8.9 (\\#18)"               "0.3 (\\#6)"                "3.2 (\\#8)"                  "0.6 (\\#7)"              "2.1 (\\#9)"               
# 4 "2.4 (\\#17)"             "12.2 (\\#18)"              "0.1 (\\#6)"                "1.5 (\\#7)"                  "0 (\\#12)"               "0 (\\#12)"                
# 5 "3.8 (\\#7)"              "19.8 (\\#13)"              "0.6 (\\#1)"                "3.4 (\\#3)"                  "0 (\\#14)"               "0 (\\#14)"                
# 6 "1.8 (\\#18)"             "8.1 (\\#18)"               "0 (\\#16)"                 "0.3 (\\#17)"                 "0.2 (\\#12)"             "0.4 (\\#14)"              
# 7 "2.8 (\\#13)"             "14.6 (\\#16)"              "0.9 (\\#1)"                "4.6 (\\#3)"                  "0.4 (\\#10)"             "1.7 (\\#13)"              
# 8 "1.6 (\\#18)"             "6.3 (\\#18)"               "0.2 (\\#6)"                "1 (\\#12)"                   "0 (\\#17)"               "0 (\\#17)"                
# 9 "2 (\\#18)"               "9.1 (\\#18)"               "0.1 (\\#12)"               "1.1 (\\#11)"                 "0.9 (\\#3)"              "3.2 (\\#3)"               
# 10 "2.5 (\\#16)"             "11.4 (\\#17)"              "0.5 (\\#3)"                "2.9 (\\#5)"                  "0.4 (\\#10)"             "2.2 (\\#10)" 



### SERIE A

# Team           season n.games Shots.Per.Game.Full Shots.Adj.Per.Game.Full     Shots.Diff.Per.Game Shots.Lead.Per.Game.Full Minutes.Lead.Per.Game.Full Shots.Trail.Per.Game.Full
# <chr>           <int>   <int> <chr>               <chr>                                     <dbl> <chr>                    <chr>                      <chr>                    
#   1 Napoli           2017      38 "17.6 (\\#2)"       "20.2, [19.8, 20.6] (\\#1)"                 2.6 "8.1 (\\#1)"             "46.9 (\\#2)"              "2.5 (\\#18)"            
# 2 AC Milan         2012      33 "15.8 (\\#3)"       "18.1, [17.8, 18.5] (\\#3)"                 2.3 "6.4 (\\#2)"             "42.4 (\\#1)"              "2 (\\#19)"              
# 3 Atalanta         2021      37 "16.1 (\\#2)"       "18.4, [18.1, 18.8] (\\#1)"                 2.3 "7.6 (\\#1)"             "41.9 (\\#3)"              "2.3 (\\#20)"            
# 4 Napoli           2018      38 "17.1 (\\#1)"       "19.4, [19, 19.8] (\\#1)"                   2.3 "6.8 (\\#1)"             "40.1 (\\#3)"              "2.4 (\\#18)"            
# 5 AS Roma          2017      38 "17.6 (\\#1)"       "19.8, [19.5, 20.2] (\\#2)"                 2.2 "7.8 (\\#2)"             "44.8 (\\#3)"              "2.6 (\\#17)"            
# 6 Internazionale   2009      33 "14.7 (\\#7)"       "16.9, [16.6, 17.2] (\\#3)"                 2.2 "6.1 (\\#1)"             "41.8 (\\#1)"              "0.6 (\\#20)"            
# 7 Internazionale   2022      37 "17.5 (\\#1)"       "19.7, [19.2, 20.1] (\\#1)"                 2.2 "7.5 (\\#1)"             "43.6 (\\#3)"              "2.8 (\\#18)"            
# 8 Internazionale   2020      36 "16.1 (\\#6)"       "18.2, [17.8, 18.5] (\\#5)"                 2.1 "6.9 (\\#1)"             "49 (\\#1)"                "2.5 (\\#19)"            
# 9 Juventus         2012      34 "19.1 (\\#1)"       "21.1, [20.8, 21.4] (\\#1)"                 2   "6.6 (\\#1)"             "34.5 (\\#3)"              "1.5 (\\#20)"            
# 10 Juventus         2014      37 "15.5 (\\#3)"       "17.5, [17.2, 17.8] (\\#1)"                 2   "6.5 (\\#1)"             "47.5 (\\#1)"              "0.9 (\\#19)"            
# Minutes.Trail.Per.Game.Full Shots.DownMen.Per.Game.Full Minutes.DownMen.Per.Game.Full Shots.UpMen.Per.Game.Full Minutes.UpMen.Per.Game.Full
# <chr>                       <chr>                       <chr>                         <chr>                     <chr>                      
#   1 "12.7 (\\#19)"              "0.2 (\\#7)"                "1.9 (\\#13)"                 "0.7 (\\#5)"              "2.9 (\\#8)"               
# 2 "7.5 (\\#20)"               "0.5 (\\#4)"                "2 (\\#10)"                   "0.2 (\\#15)"             "1.2 (\\#14)"              
# 3 "14.4 (\\#19)"              "0.2 (\\#11)"               "2.4 (\\#11)"                 "0.3 (\\#12)"             "1.5 (\\#12)"              
# 4 "12 (\\#19)"                "0.2 (\\#12)"               "3.3 (\\#10)"                 "0 (\\#20)"               "0 (\\#20)"                
# 5 "13.4 (\\#18)"              "0 (\\#17)"                 "0.1 (\\#19)"                 "0.1 (\\#20)"             "1.1 (\\#17)"              
# 6 "3.5 (\\#20)"               "0.5 (\\#8)"                "3.3 (\\#12)"                 "0.5 (\\#13)"             "1.7 (\\#15)"              
# 7 "12.5 (\\#19)"              "0 (\\#19)"                 "0 (\\#20)"                   "0.5 (\\#8)"              "1.7 (\\#14)"              
# 8 "11.7 (\\#19)"              "0.2 (\\#13)"               "2.5 (\\#13)"                 "0.6 (\\#8)"              "2.8 (\\#12)"              
# 9 "7.9 (\\#19)"               "0.4 (\\#7)"                "1.7 (\\#13)"                 "0.7 (\\#8)"              "2.4 (\\#9)"               
# 10 "5.2 (\\#20)"               "0.2 (\\#12)"               "2.3 (\\#14)"                 "0.7 (\\#11)"             "4 (\\#8)"              



### LA LIGA

# Team        season n.games Shots.Per.Game.Full Shots.Adj.Per.Game.Full     Shots.Diff.Per.Game Shots.Lead.Per.Game.Full Minutes.Lead.Per.Game.Full Shots.Trail.Per.Game.Full
# <chr>        <int>   <int> <chr>               <chr>                                     <dbl> <chr>                    <chr>                      <chr>                    
#   1 Real Madrid   2011      33 "19.9 (\\#2)"       "24.2, [23.5, 24.9] (\\#2)"                 4.3 "10.2 (\\#2)"            "48.9 (\\#3)"              "1.9 (\\#19)"            
# 2 Real Madrid   2015      38 "17.7 (\\#1)"       "21.3, [20.7, 21.9] (\\#1)"                 3.6 "8.9 (\\#1)"             "51.2 (\\#1)"              "2 (\\#18)"              
# 3 Barcelona     2010      34 "15.9 (\\#3)"       "19.4, [18.9, 19.8] (\\#2)"                 3.5 "8.6 (\\#2)"             "50.2 (\\#1)"              "0.7 (\\#20)"            
# 4 Real Madrid   2010      36 "21.6 (\\#1)"       "25, [24.4, 25.5] (\\#1)"                   3.4 "9.9 (\\#1)"             "45.9 (\\#2)"              "2.9 (\\#14)"            
# 5 Real Madrid   2013      36 "18.1 (\\#1)"       "21.2, [20.8, 21.7] (\\#1)"                 3.1 "8.9 (\\#1)"             "46.2 (\\#2)"              "2.6 (\\#17)"            
# 6 Barcelona     2009      35 "18.3 (\\#1)"       "21.3, [20.7, 21.8] (\\#1)"                 3   "8.8 (\\#1)"             "46.5 (\\#1)"              "2.6 (\\#17)"            
# 7 Barcelona     2015      38 "16.1 (\\#2)"       "19.1, [18.5, 19.6] (\\#2)"                 3   "8.2 (\\#2)"             "48.8 (\\#2)"              "1.5 (\\#19)"            
# 8 Real Madrid   2014      38 "19.3 (\\#1)"       "22.3, [21.8, 22.8] (\\#1)"                 3   "9.1 (\\#1)"             "47.3 (\\#1)"              "2.8 (\\#15)"            
# 9 Real Madrid   2016      37 "18.6 (\\#1)"       "21.6, [21, 22.3] (\\#1)"                   3   "8.9 (\\#1)"             "50.8 (\\#2)"              "2.1 (\\#18)"            
# 10 Real Madrid   2017      38 "17.3 (\\#1)"       "20.3, [19.8, 20.8] (\\#1)"                 3   "8.2 (\\#1)"             "49.6 (\\#1)"              "1.8 (\\#18)"            
# Minutes.Trail.Per.Game.Full Shots.DownMen.Per.Game.Full Minutes.DownMen.Per.Game.Full Shots.UpMen.Per.Game.Full Minutes.UpMen.Per.Game.Full
# <chr>                       <chr>                       <chr>                         <chr>                     <chr>                      
#   1 "5.6 (\\#19)"               "0.7 (\\#2)"                "3.2 (\\#10)"                 "0.4 (\\#15)"             "1.9 (\\#15)"              
# 2 "11.3 (\\#18)"              "0.3 (\\#9)"                "1.5 (\\#15)"                 "0 (\\#20)"               "0 (\\#20)"                
# 3 "3.6 (\\#20)"               "0.6 (\\#3)"                "3.8 (\\#11)"                 "0.4 (\\#17)"             "1.3 (\\#18)"              
# 4 "8.8 (\\#19)"               "0.6 (\\#4)"                "3.1 (\\#14)"                 "1.4 (\\#4)"              "4.7 (\\#7)"               
# 5 "13.9 (\\#17)"              "0.8 (\\#3)"                "5.5 (\\#6)"                  "0.7 (\\#12)"             "3.4 (\\#14)"              
# 6 "11.2 (\\#20)"              "0.4 (\\#12)"               "2.8 (\\#17)"                 "1.2 (\\#8)"              "5 (\\#9)"                 
# 7 "9.2 (\\#19)"               "0.3 (\\#11)"               "2.4 (\\#14)"                 "1 (\\#3)"                "4.7 (\\#5)"               
# 8 "12.2 (\\#18)"              "0.1 (\\#16)"               "2.3 (\\#13)"                 "0.5 (\\#15)"             "1.5 (\\#20)"              
# 9 "10.2 (\\#18)"              "0.3 (\\#7)"                "1.6 (\\#18)"                 "1.1 (\\#4)"              "4.1 (\\#9)"               
# 10 "7.3 (\\#20)"               "0.5 (\\#3)"                "1.7 (\\#17)"                 "0.4 (\\#11)"             "2.2 (\\#11)"               




### LIGUE 1

# Team                season n.games Shots.Per.Game.Full Shots.Adj.Per.Game.Full     Shots.Diff.Per.Game Shots.Lead.Per.Game.Full Minutes.Lead.Per.Game.Full
# <chr>                <int>   <int> <chr>               <chr>                                     <dbl> <chr>                    <chr>                     
#   1 Paris Saint-Germain   2018      38 "16.1 (\\#2)"       "19.2, [18.6, 19.8] (\\#1)"                 3.1 "8.2 (\\#1)"             "48 (\\#1)"               
# 2 Paris Saint-Germain   2016      38 "14.7 (\\#1)"       "17, [16.6, 17.6] (\\#1)"                   2.3 "7.4 (\\#1)"             "47.2 (\\#1)"             
# 3 Paris Saint-Germain   2017      38 "15.1 (\\#2)"       "17.4, [17, 17.8] (\\#1)"                   2.3 "6.9 (\\#2)"             "45.2 (\\#2)"             
# 4 Paris Saint-Germain   2023      38 "14.9 (\\#1)"       "17.2, [16.7, 17.6] (\\#1)"                 2.3 "7.3 (\\#1)"             "48.8 (\\#1)"             
# 5 AS Monaco             2017      38 "14.5 (\\#3)"       "16.7, [16.3, 17.2] (\\#2)"                 2.2 "7.7 (\\#1)"             "55.1 (\\#1)"             
# 6 Paris Saint-Germain   2019      37 "14.5 (\\#2)"       "16.5, [16.1, 16.9] (\\#2)"                 2   "6.7 (\\#1)"             "47.2 (\\#1)"             
# 7 Stade Rennais         2022      36 "14.8 (\\#1)"       "16.6, [16.2, 17] (\\#1)"                   1.8 "6.4 (\\#1)"             "39.6 (\\#2)"             
# 8 Lens                  2023      35 "13.6 (\\#5)"       "15.3, [15, 15.6] (\\#3)"                   1.7 "4.5 (\\#5)"             "39.1 (\\#3)"             
# 9 Lyon                  2009      34 "15.2 (\\#3)"       "16.9, [16.6, 17.2] (\\#2)"                 1.7 "4.7 (\\#1)"             "31.7 (\\#2)"             
# 10 Lyon                  2011      37 "14.5 (\\#4)"       "16.2, [15.9, 16.5] (\\#1)"                 1.7 "4.9 (\\#1)"             "32.7 (\\#1)"             
# Shots.Trail.Per.Game.Full Minutes.Trail.Per.Game.Full Shots.DownMen.Per.Game.Full Minutes.DownMen.Per.Game.Full Shots.UpMen.Per.Game.Full Minutes.UpMen.Per.Game.Full
# <chr>                     <chr>                       <chr>                       <chr>                         <chr>                     <chr>                      
#   1 "1.7 (\\#20)"             "8.5 (\\#20)"               "0.8 (\\#1)"                "5.6 (\\#4)"                  "0.5 (\\#10)"             "1.8 (\\#14)"              
# 2 "0.6 (\\#20)"             "4.3 (\\#20)"               "0.1 (\\#17)"               "1.9 (\\#18)"                 "0.6 (\\#11)"             "2.3 (\\#15)"              
# 3 "1.3 (\\#20)"             "9.1 (\\#19)"               "0.2 (\\#16)"               "1.5 (\\#17)"                 "0 (\\#20)"               "0 (\\#20)"                
# 4 "2.2 (\\#19)"             "13.5 (\\#19)"              "0.7 (\\#2)"                "5.7 (\\#4)"                  "0.9 (\\#8)"              "4.6 (\\#10)"              
# 5 "1.5 (\\#19)"             "8.8 (\\#20)"               "0.1 (\\#18)"               "2.1 (\\#15)"                 "0.7 (\\#10)"             "2.7 (\\#14)"              
# 6 "1.7 (\\#20)"             "10.2 (\\#20)"              "0.1 (\\#16)"               "2.5 (\\#17)"                 "0.6 (\\#13)"             "2.6 (\\#16)"              
# 7 "2.4 (\\#19)"             "16.1 (\\#18)"              "0.2 (\\#13)"               "2.5 (\\#15)"                 "0.9 (\\#9)"              "2.8 (\\#14)"              
# 8 "2 (\\#20)"               "10.1 (\\#20)"              "0.7 (\\#1)"                "5.3 (\\#5)"                  "0.2 (\\#16)"             "1.6 (\\#16)"              
# 9 "2.4 (\\#17)"             "12.6 (\\#17)"              "0.6 (\\#1)"                "4.5 (\\#1)"                  "0.3 (\\#11)"             "1.4 (\\#11)"              
# 10 "2.1 (\\#18)"             "13 (\\#18)"                "0.5 (\\#3)"                "4.4 (\\#3)"                  "0.1 (\\#18)"             "0.7 (\\#18)"  



## PREMIER LEAGUE

# Team              season n.games Shots.Per.Game.Full Shots.Adj.Per.Game.Full     Shots.Diff.Per.Game Shots.Lead.Per.Game.Full Minutes.Lead.Per.Game.Full
# <chr>              <int>   <int> <chr>               <chr>                                     <dbl> <chr>                    <chr>                     
#   1 Manchester City     2019      38 "17.9 (\\#1)"       "21, [20.5, 21.4] (\\#1)"                   3.1 "11.2 (\\#1)"            "60.8 (\\#1)"             
# 2 Manchester City     2018      38 "17.4 (\\#1)"       "20.2, [19.8, 20.6] (\\#1)"                 2.8 "8.7 (\\#1)"             "53.6 (\\#1)"             
# 3 Manchester City     2020      37 "19.4 (\\#1)"       "22, [21.4, 22.6] (\\#1)"                   2.6 "8.8 (\\#1)"             "46.4 (\\#2)"             
# 4 Manchester City     2022      37 "18.7 (\\#2)"       "21.1, [20.6, 21.7] (\\#2)"                 2.4 "9.6 (\\#1)"             "51.5 (\\#1)"             
# 5 Liverpool           2019      38 "14.9 (\\#3)"       "17.2, [16.9, 17.5] (\\#3)"                 2.3 "7.6 (\\#2)"             "49.7 (\\#2)"             
# 6 Liverpool           2022      36 "18.9 (\\#1)"       "21.2, [20.8, 21.6] (\\#1)"                 2.3 "9.6 (\\#2)"             "48.7 (\\#2)"             
# 7 Manchester City     2014      36 "17.4 (\\#2)"       "19.6, [19.2, 20.1] (\\#1)"                 2.2 "8.9 (\\#1)"             "55.8 (\\#1)"             
# 8 Manchester City     2023      37 "15.7 (\\#3)"       "17.9, [17.6, 18.3] (\\#1)"                 2.2 "7.1 (\\#1)"             "52.1 (\\#1)"             
# 9 Tottenham Hotspur   2017      38 "17.3 (\\#1)"       "19.5, [19.2, 19.9] (\\#1)"                 2.2 "7.6 (\\#1)"             "39.9 (\\#4)"             
# 10 Liverpool           2018      38 "16.7 (\\#2)"       "18.8, [18.4, 19.1] (\\#2)"                 2.1 "7.2 (\\#2)"             "43.5 (\\#2)"             
# Shots.Trail.Per.Game.Full Minutes.Trail.Per.Game.Full Shots.DownMen.Per.Game.Full Minutes.DownMen.Per.Game.Full Shots.UpMen.Per.Game.Full Minutes.UpMen.Per.Game.Full
# <chr>                     <chr>                       <chr>                       <chr>                         <chr>                     <chr>                      
#   1 "0.8 (\\#20)"             "4.3 (\\#20)"               "0 (\\#17)"                 "0.2 (\\#17)"                 "0.6 (\\#7)"              "2.4 (\\#8)"               
# 2 "1 (\\#20)"               "4.6 (\\#20)"               "0.3 (\\#5)"                "1.3 (\\#11)"                 "0.2 (\\#12)"             "1.8 (\\#8)"               
# 3 "3.7 (\\#8)"              "16.8 (\\#19)"              "0.3 (\\#4)"                "4.3 (\\#3)"                  "0.3 (\\#11)"             "1.5 (\\#11)"              
# 4 "2.3 (\\#17)"             "10.8 (\\#18)"              "0.2 (\\#6)"                "1.5 (\\#12)"                 "1.4 (\\#1)"              "4.4 (\\#2)"               
# 5 "0.8 (\\#19)"             "4.8 (\\#19)"               "0.1 (\\#10)"               "0.6 (\\#16)"                 "0.2 (\\#13)"             "0.6 (\\#16)"              
# 6 "1.8 (\\#19)"             "7.3 (\\#20)"               "0 (\\#17)"                 "0.6 (\\#16)"                 "0.9 (\\#4)"              "3.6 (\\#4)"               
# 7 "2.1 (\\#19)"             "8.4 (\\#19)"               "0.2 (\\#9)"                "2.5 (\\#9)"                  "1.1 (\\#3)"              "4 (\\#6)"                 
# 8 "1.5 (\\#20)"             "7.6 (\\#20)"               "0.3 (\\#3)"                "2.1 (\\#7)"                  "0.3 (\\#8)"              "2.1 (\\#6)"               
# 9 "1.7 (\\#20)"             "13.7 (\\#19)"              "0 (\\#19)"                 "0 (\\#19)"                   "0.3 (\\#10)"             "1.3 (\\#11)"              
# 10 "2.2 (\\#19)"             "14.2 (\\#19)"              "0 (\\#17)"                 "1.8 (\\#9)"                  "0 (\\#17)"               "0 (\\#17)"










######
######
### CORNERS
######
######


### BUNDESLIGA

# Team          season n.games Corners.Per.Game.Full Corners.Adj.Per.Game.Full Corners.Diff.Per.Game Corners.Lead.Per.Game.Full Minutes.Lead.Per.Game.Full
# <chr>          <int>   <int> <chr>                 <chr>                                     <dbl> <chr>                      <chr>                     
#   1 Bayern Munich   2015      34 "6.5 (\\#1)"          "7.7, [7.4, 8] (\\#1)"                      1.2 "3.1 (\\#1)"               "44.9 (\\#1)"             
# 2 Bayern Munich   2019      34 "8.5 (\\#1)"          "9.7, [9.4, 10.1] (\\#1)"                   1.2 "4.4 (\\#1)"               "50.7 (\\#1)"             
# 3 Bayern Munich   2020      34 "7.1 (\\#1)"          "8.3, [8, 8.7] (\\#1)"                      1.2 "3.6 (\\#1)"               "52.2 (\\#1)"             
# 4 Bayern Munich   2014      34 "7.1 (\\#1)"          "8.2, [7.9, 8.4] (\\#1)"                    1.1 "3.4 (\\#1)"               "47.9 (\\#1)"             
# 5 Bayern Munich   2023      34 "6.7 (\\#1)"          "7.8, [7.6, 8.1] (\\#1)"                    1.1 "3.4 (\\#1)"               "54.2 (\\#1)"             
# 6 Bayern Munich   2012      33 "6.4 (\\#1)"          "7.4, [7, 7.8] (\\#1)"                      1   "2.6 (\\#1)"               "39.2 (\\#2)"             
# 7 Bayern Munich   2013      34 "7.6 (\\#1)"          "8.6, [8.3, 9] (\\#1)"                      1   "3.9 (\\#1)"               "51 (\\#1)"               
# 8 Bayern Munich   2021      34 "7 (\\#1)"            "8, [7.7, 8.4] (\\#1)"                      1   "3.4 (\\#1)"               "47.4 (\\#1)"             
# 9 Bayern Munich   2016      34 "6.9 (\\#1)"          "7.8, [7.6, 8] (\\#1)"                      0.9 "2.9 (\\#1)"               "46.7 (\\#1)"             
# 10 Bayern Munich   2017      34 "7.2 (\\#1)"          "8, [7.7, 8.3] (\\#1)"                      0.8 "3.1 (\\#1)"               "42.9 (\\#1)"             
# Corners.Trail.Per.Game.Full Minutes.Trail.Per.Game.Full Corners.DownMen.Per.Game.Full Minutes.DownMen.Per.Game.Full Corners.UpMen.Per.Game.Full Minutes.UpMen.Per.Game.Full
# <chr>                       <chr>                       <chr>                         <chr>                         <chr>                       <chr>                      
#   1 "0.5 (\\#18)"               "7.9 (\\#18)"               "0.2 (\\#3)"                  "4.7 (\\#2)"                  "0.1 (\\#7)"                "1 (\\#12)"                
# 2 "0.7 (\\#17)"               "9.1 (\\#18)"               "0 (\\#10)"                   "1.1 (\\#11)"                 "0.4 (\\#1)"                "3.2 (\\#3)"               
# 3 "0.6 (\\#18)"               "8.9 (\\#18)"               "0.1 (\\#3)"                  "3.2 (\\#8)"                  "0.2 (\\#6)"                "2.1 (\\#9)"               
# 4 "0.8 (\\#18)"               "8.1 (\\#18)"               "0 (\\#14)"                   "0.3 (\\#17)"                 "0.1 (\\#10)"               "0.4 (\\#14)"              
# 5 "0.8 (\\#17)"               "9.2 (\\#18)"               "0 (\\#11)"                   "4.2 (\\#5)"                  "0 (\\#15)"                 "0 (\\#16)"                
# 6 "0.8 (\\#16)"               "11.4 (\\#17)"              "0.2 (\\#2)"                  "2.9 (\\#5)"                  "0.1 (\\#11)"               "2.2 (\\#10)"              
# 7 "0.7 (\\#18)"               "5.5 (\\#18)"               "0 (\\#13)"                   "0.8 (\\#17)"                 "0.4 (\\#2)"                "5.2 (\\#5)"               
# 8 "1.5 (\\#5)"                "19.8 (\\#13)"              "0.1 (\\#2)"                  "3.4 (\\#3)"                  "0 (\\#13)"                 "0 (\\#14)"                
# 9 "0.6 (\\#18)"               "6.3 (\\#18)"               "0.1 (\\#4)"                  "1 (\\#12)"                   "0 (\\#14)"                 "0 (\\#17)"                
# 10 "1.1 (\\#12)"               "12.4 (\\#18)"              "0 (\\#12)"                   "0 (\\#18)"                   "0.2 (\\#6)"                "4.3 (\\#3)"               


### SERIE A

# Team           season n.games Corners.Per.Game.Full Corners.Adj.Per.Game.Full Corners.Diff.Per.Game Corners.Lead.Per.Game.Full Minutes.Lead.Per.Game.Full
# <chr>           <int>   <int> <chr>                 <chr>                                     <dbl> <chr>                      <chr>                     
#   1 AC Milan         2012      33 "6.7 (\\#2)"          "8.4, [8.1, 8.7] (\\#2)"                    1.7 "2.8 (\\#1)"               "42.4 (\\#1)"             
# 2 Internazionale   2022      37 "6.5 (\\#1)"          "8, [7.7, 8.4] (\\#1)"                      1.5 "2.8 (\\#1)"               "43.6 (\\#3)"             
# 3 Napoli           2017      38 "7 (\\#2)"            "8.5, [8.2, 8.8] (\\#2)"                    1.5 "2.8 (\\#1)"               "46.9 (\\#2)"             
# 4 Juventus         2017      38 "5.8 (\\#8)"          "7.2, [7, 7.4] (\\#4)"                      1.4 "2.6 (\\#2)"               "47.1 (\\#1)"             
# 5 Atalanta         2021      37 "5.4 (\\#6)"          "6.7, [6.4, 6.9] (\\#1)"                    1.3 "2.4 (\\#1)"               "41.9 (\\#3)"             
# 6 Juventus         2014      37 "5.7 (\\#4)"          "7, [6.8, 7.3] (\\#1)"                      1.3 "2.5 (\\#1)"               "47.5 (\\#1)"             
# 7 Juventus         2015      38 "5.8 (\\#7)"          "7.1, [6.9, 7.3] (\\#1)"                    1.3 "2.3 (\\#1)"               "44.6 (\\#1)"             
# 8 Juventus         2019      38 "6.4 (\\#4)"          "7.7, [7.5, 7.9] (\\#2)"                    1.3 "2.9 (\\#1)"               "44.7 (\\#1)"             
# 9 Internazionale   2018      37 "7.7 (\\#1)"          "8.9, [8.6, 9.2] (\\#1)"                    1.2 "2.5 (\\#2)"               "33.1 (\\#6)"             
# 10 Juventus         2012      34 "7.4 (\\#1)"          "8.6, [8.4, 8.9] (\\#1)"                    1.2 "2.4 (\\#2)"               "34.5 (\\#3)"             
# Corners.Trail.Per.Game.Full Minutes.Trail.Per.Game.Full Corners.DownMen.Per.Game.Full Minutes.DownMen.Per.Game.Full Corners.UpMen.Per.Game.Full Minutes.UpMen.Per.Game.Full
# <chr>                       <chr>                       <chr>                         <chr>                         <chr>                       <chr>                      
#   1 "0.5 (\\#20)"               "7.5 (\\#20)"               "0.2 (\\#4)"                  "2 (\\#10)"                   "0 (\\#18)"                 "1.2 (\\#14)"              
# 2 "0.7 (\\#20)"               "12.5 (\\#19)"              "0 (\\#18)"                   "0 (\\#20)"                   "0.2 (\\#6)"                "1.7 (\\#14)"              
# 3 "1.3 (\\#13)"               "12.7 (\\#19)"              "0.2 (\\#6)"                  "1.9 (\\#13)"                 "0.4 (\\#3)"                "2.9 (\\#8)"               
# 4 "0.8 (\\#20)"               "9.6 (\\#20)"               "0 (\\#20)"                   "0 (\\#20)"                   "0.1 (\\#11)"               "2.2 (\\#14)"              
# 5 "0.6 (\\#20)"               "14.4 (\\#19)"              "0 (\\#15)"                   "2.4 (\\#11)"                 "0.1 (\\#8)"                "1.5 (\\#12)"              
# 6 "0.4 (\\#19)"               "5.2 (\\#20)"               "0.1 (\\#10)"                 "2.3 (\\#14)"                 "0.2 (\\#12)"               "4 (\\#8)"                 
# 7 "0.2 (\\#20)"               "3.8 (\\#20)"               "0.1 (\\#12)"                 "1.3 (\\#16)"                 "0.1 (\\#18)"               "1.4 (\\#17)"              
# 8 "0.7 (\\#20)"               "10.6 (\\#19)"              "0.1 (\\#12)"                 "1.3 (\\#14)"                 "0.1 (\\#16)"               "2 (\\#12)"                
# 9 "1.7 (\\#8)"                "17.4 (\\#15)"              "0.1 (\\#13)"                 "2.4 (\\#14)"                 "0.2 (\\#13)"               "3.1 (\\#11)"              
# 10 "0.7 (\\#19)"               "7.9 (\\#19)"               "0.3 (\\#2)"                  "1.7 (\\#13)"                 "0.1 (\\#11)"               "2.4 (\\#9)" 



### LA LIGA

# Team        season n.games Corners.Per.Game.Full Corners.Adj.Per.Game.Full Corners.Diff.Per.Game Corners.Lead.Per.Game.Full Minutes.Lead.Per.Game.Full
# <chr>        <int>   <int> <chr>                 <chr>                                     <dbl> <chr>                      <chr>                     
#   1 Real Madrid   2011      33 "7.2 (\\#3)"          "9.3, [8.9, 9.7] (\\#3)"                    2.1 "3.2 (\\#3)"               "48.9 (\\#3)"             
# 2 Barcelona     2011      32 "7.3 (\\#2)"          "9.3, [8.9, 9.7] (\\#2)"                    2   "3.7 (\\#2)"               "50.1 (\\#2)"             
# 3 Barcelona     2010      34 "6.7 (\\#2)"          "8.5, [8.3, 8.8] (\\#1)"                    1.8 "3.2 (\\#1)"               "50.2 (\\#1)"             
# 4 Barcelona     2013      36 "5.9 (\\#7)"          "7.7, [7.4, 8] (\\#3)"                      1.8 "3.3 (\\#1)"               "52.2 (\\#1)"             
# 5 Real Madrid   2016      37 "6.9 (\\#1)"          "8.7, [8.3, 9.2] (\\#1)"                    1.8 "3.2 (\\#1)"               "50.8 (\\#2)"             
# 6 Barcelona     2014      38 "6.9 (\\#1)"          "8.6, [8.2, 8.9] (\\#1)"                    1.7 "3.1 (\\#1)"               "46.7 (\\#2)"             
# 7 Real Madrid   2013      36 "6.4 (\\#3)"          "8.1, [7.8, 8.3] (\\#1)"                    1.7 "3.1 (\\#2)"               "46.2 (\\#2)"             
# 8 Real Madrid   2014      38 "6.7 (\\#3)"          "8.4, [8.1, 8.7] (\\#2)"                    1.7 "3 (\\#2)"                 "47.3 (\\#1)"             
# 9 Barcelona     2009      35 "6.7 (\\#1)"          "8.3, [8, 8.6] (\\#1)"                      1.6 "3 (\\#1)"                 "46.5 (\\#1)"             
# 10 Real Madrid   2010      36 "6 (\\#6)"            "7.5, [7.2, 7.7] (\\#4)"                    1.5 "2.7 (\\#2)"               "45.9 (\\#2)"             
# Corners.Trail.Per.Game.Full Minutes.Trail.Per.Game.Full Corners.DownMen.Per.Game.Full Minutes.DownMen.Per.Game.Full Corners.UpMen.Per.Game.Full Minutes.UpMen.Per.Game.Full
# <chr>                       <chr>                       <chr>                         <chr>                         <chr>                       <chr>                      
#   1 "0.5 (\\#19)"               "5.6 (\\#19)"               "0.3 (\\#4)"                  "3.2 (\\#10)"                 "0.1 (\\#17)"               "1.9 (\\#15)"              
# 2 "0.5 (\\#20)"               "4.2 (\\#20)"               "0 (\\#19)"                   "0.4 (\\#19)"                 "0.4 (\\#6)"                "4.8 (\\#6)"               
# 3 "0.3 (\\#20)"               "3.6 (\\#20)"               "0.1 (\\#10)"                 "3.8 (\\#11)"                 "0.1 (\\#17)"               "1.3 (\\#18)"              
# 4 "0.5 (\\#19)"               "8.6 (\\#19)"               "0 (\\#18)"                   "2.3 (\\#16)"                 "0.3 (\\#9)"                "3.6 (\\#12)"              
# 5 "0.9 (\\#18)"               "10.2 (\\#18)"              "0.3 (\\#2)"                  "1.6 (\\#18)"                 "0.4 (\\#6)"                "4.1 (\\#9)"               
# 6 "0.9 (\\#16)"               "10.8 (\\#19)"              "0 (\\#18)"                   "0.4 (\\#19)"                 "0.1 (\\#18)"               "1.9 (\\#19)"              
# 7 "0.9 (\\#18)"               "13.9 (\\#17)"              "0.2 (\\#6)"                  "5.5 (\\#6)"                  "0.3 (\\#11)"               "3.4 (\\#14)"              
# 8 "0.8 (\\#19)"               "12.2 (\\#18)"              "0 (\\#17)"                   "2.3 (\\#13)"                 "0.1 (\\#17)"               "1.5 (\\#20)"              
# 9 "1.1 (\\#18)"               "11.2 (\\#20)"              "0.1 (\\#15)"                 "2.8 (\\#17)"                 "0.3 (\\#9)"                "5 (\\#9)"                 
# 10 "0.8 (\\#19)"               "8.8 (\\#19)"               "0.1 (\\#15)"                 "3.1 (\\#14)"                 "0.4 (\\#6)"                "4.7 (\\#7)"             


### LIGUE 1

# Team                season n.games Corners.Per.Game.Full Corners.Adj.Per.Game.Full Corners.Diff.Per.Game Corners.Lead.Per.Game.Full Minutes.Lead.Per.Game.Full
# <chr>                <int>   <int> <chr>                 <chr>                                     <dbl> <chr>                      <chr>                     
#   1 Paris Saint-Germain   2018      38 "6.7 (\\#1)"          "8.6, [8.2, 9] (\\#1)"                      1.9 "3 (\\#1)"                 "48 (\\#1)"               
# 2 Paris Saint-Germain   2017      38 "7 (\\#1)"            "8.5, [8.2, 8.7] (\\#1)"                    1.5 "2.8 (\\#1)"               "45.2 (\\#2)"             
# 3 AS Monaco             2017      38 "6.1 (\\#3)"          "7.4, [7.1, 7.7] (\\#2)"                    1.3 "2.7 (\\#2)"               "55.1 (\\#1)"             
# 4 Paris Saint-Germain   2016      38 "5.4 (\\#4)"          "6.7, [6.4, 7] (\\#1)"                      1.3 "2.7 (\\#1)"               "47.2 (\\#1)"             
# 5 Paris Saint-Germain   2019      37 "6.4 (\\#1)"          "7.7, [7.4, 8] (\\#1)"                      1.3 "2.7 (\\#1)"               "47.2 (\\#1)"             
# 6 Paris Saint-Germain   2023      38 "5.4 (\\#3)"          "6.7, [6.4, 6.9] (\\#1)"                    1.3 "2.6 (\\#1)"               "48.8 (\\#1)"             
# 7 Marseille             2018      38 "6.4 (\\#2)"          "7.5, [7.2, 7.7] (\\#2)"                    1.1 "2.2 (\\#2)"               "39.1 (\\#3)"             
# 8 Paris Saint-Germain   2015      37 "4.5 (\\#12)"         "5.5, [5.4, 5.7] (\\#5)"                    1   "2.1 (\\#1)"               "47.6 (\\#1)"             
# 9 Paris Saint-Germain   2021      38 "5.8 (\\#2)"          "6.8, [6.6, 7] (\\#2)"                      1   "2.4 (\\#1)"               "47.3 (\\#1)"             
# 10 AS Monaco             2021      38 "5.7 (\\#3)"          "6.6, [6.4, 6.8] (\\#3)"                    0.9 "2.2 (\\#2)"               "41.6 (\\#2)"             
# Corners.Trail.Per.Game.Full Minutes.Trail.Per.Game.Full Corners.DownMen.Per.Game.Full Minutes.DownMen.Per.Game.Full Corners.UpMen.Per.Game.Full Minutes.UpMen.Per.Game.Full
# <chr>                       <chr>                       <chr>                         <chr>                         <chr>                       <chr>                      
#   1 "0.6 (\\#20)"               "8.5 (\\#20)"               "0.3 (\\#1)"                  "5.6 (\\#4)"                  "0.1 (\\#11)"               "1.8 (\\#14)"              
# 2 "0.7 (\\#19)"               "9.1 (\\#19)"               "0.1 (\\#14)"                 "1.5 (\\#17)"                 "0 (\\#20)"                 "0 (\\#20)"                
# 3 "0.7 (\\#18)"               "8.8 (\\#20)"               "0.1 (\\#10)"                 "2.1 (\\#15)"                 "0.2 (\\#12)"               "2.7 (\\#14)"              
# 4 "0.3 (\\#20)"               "4.3 (\\#20)"               "0 (\\#19)"                   "1.9 (\\#18)"                 "0.2 (\\#12)"               "2.3 (\\#15)"              
# 5 "0.8 (\\#19)"               "10.2 (\\#20)"              "0.1 (\\#16)"                 "2.5 (\\#17)"                 "0.3 (\\#6)"                "2.6 (\\#16)"              
# 6 "0.8 (\\#19)"               "13.5 (\\#19)"              "0.2 (\\#4)"                  "5.7 (\\#4)"                  "0.2 (\\#10)"               "4.6 (\\#10)"              
# 7 "1.3 (\\#16)"               "18.9 (\\#17)"              "0.2 (\\#3)"                  "1.8 (\\#16)"                 "0.1 (\\#10)"               "2.6 (\\#12)"              
# 8 "0.4 (\\#20)"               "9.3 (\\#20)"               "0 (\\#19)"                   "1.2 (\\#16)"                 "0 (\\#19)"                 "2.1 (\\#13)"              
# 9 "0.4 (\\#20)"               "11 (\\#20)"                "0.1 (\\#15)"                 "1.6 (\\#17)"                 "0.7 (\\#1)"                "7.9 (\\#2)"               
# 10 "1.2 (\\#17)"               "17.8 (\\#17)"              "0.1 (\\#11)"                 "5.9 (\\#4)"                  "0.5 (\\#4)"                "6.8 (\\#3)"             



### PREMIER LEAGUE

# Team              season n.games Corners.Per.Game.Full Corners.Adj.Per.Game.Full Corners.Diff.Per.Game Corners.Lead.Per.Game.Full Minutes.Lead.Per.Game.Full
# <chr>              <int>   <int> <chr>                 <chr>                                     <dbl> <chr>                      <chr>                     
#   1 Manchester City     2019      38 "7.9 (\\#1)"          "9.6, [9.3, 9.9] (\\#1)"                    1.7 "4.5 (\\#1)"               "60.8 (\\#1)"             
# 2 Manchester City     2018      38 "7.5 (\\#1)"          "9.1, [8.8, 9.4] (\\#1)"                    1.6 "3.6 (\\#1)"               "53.6 (\\#1)"             
# 3 Liverpool           2019      38 "6.6 (\\#2)"          "8.1, [7.8, 8.3] (\\#2)"                    1.5 "3.7 (\\#2)"               "49.7 (\\#2)"             
# 4 Manchester City     2022      37 "8.2 (\\#1)"          "9.7, [9.3, 10] (\\#1)"                     1.5 "4.2 (\\#1)"               "51.5 (\\#1)"             
# 5 Manchester City     2023      37 "6.3 (\\#2)"          "7.6, [7.4, 7.9] (\\#2)"                    1.3 "3.1 (\\#1)"               "52.1 (\\#1)"             
# 6 Liverpool           2020      38 "6.8 (\\#2)"          "8, [7.8, 8.3] (\\#2)"                      1.2 "3.3 (\\#1)"               "50.3 (\\#1)"             
# 7 Liverpool           2022      36 "7.5 (\\#2)"          "8.7, [8.4, 8.9] (\\#2)"                    1.2 "3.6 (\\#2)"               "48.7 (\\#2)"             
# 8 Manchester City     2014      36 "7.3 (\\#1)"          "8.5, [8.2, 8.9] (\\#1)"                    1.2 "3.5 (\\#1)"               "55.8 (\\#1)"             
# 9 Manchester City     2021      37 "6.4 (\\#2)"          "7.5, [7.3, 7.8] (\\#1)"                    1.1 "2.9 (\\#1)"               "51.6 (\\#1)"             
# 10 Tottenham Hotspur   2017      38 "7.2 (\\#2)"          "8.3, [8.1, 8.5] (\\#1)"                    1.1 "2.7 (\\#1)"               "39.9 (\\#4)"             
# Corners.Trail.Per.Game.Full Minutes.Trail.Per.Game.Full Corners.DownMen.Per.Game.Full Minutes.DownMen.Per.Game.Full Corners.UpMen.Per.Game.Full Minutes.UpMen.Per.Game.Full
# <chr>                       <chr>                       <chr>                         <chr>                         <chr>                       <chr>                      
#   1 "0.5 (\\#19)"               "4.3 (\\#20)"               "0 (\\#19)"                   "0.2 (\\#17)"                 "0.3 (\\#4)"                "2.4 (\\#8)"               
# 2 "0.4 (\\#20)"               "4.6 (\\#20)"               "0.1 (\\#7)"                  "1.3 (\\#11)"                 "0.1 (\\#9)"                "1.8 (\\#8)"               
# 3 "0.2 (\\#20)"               "4.8 (\\#19)"               "0 (\\#18)"                   "0.6 (\\#16)"                 "0.1 (\\#14)"               "0.6 (\\#16)"              
# 4 "1.2 (\\#14)"               "10.8 (\\#18)"              "0.1 (\\#5)"                  "1.5 (\\#12)"                 "0.6 (\\#1)"                "4.4 (\\#2)"               
# 5 "0.5 (\\#20)"               "7.6 (\\#20)"               "0.2 (\\#3)"                  "2.1 (\\#7)"                  "0.2 (\\#7)"                "2.1 (\\#6)"               
# 6 "0.9 (\\#19)"               "11.4 (\\#20)"              "0 (\\#18)"                   "0.5 (\\#17)"                 "0 (\\#18)"                 "0 (\\#18)"                
# 7 "0.6 (\\#20)"               "7.3 (\\#20)"               "0 (\\#16)"                   "0.6 (\\#16)"                 "0.4 (\\#2)"                "3.6 (\\#4)"               
# 8 "1 (\\#16)"                 "8.4 (\\#19)"               "0.1 (\\#5)"                  "2.5 (\\#9)"                  "0.5 (\\#2)"                "4 (\\#6)"                 
# 9 "0.8 (\\#20)"               "10.6 (\\#20)"              "0.1 (\\#6)"                  "3 (\\#6)"                    "0.2 (\\#10)"               "1.6 (\\#13)"              
# 10 "0.7 (\\#20)"               "13.7 (\\#19)"              "0 (\\#19)"                   "0 (\\#19)"                   "0.1 (\\#11)"               "1.3 (\\#11)"  
