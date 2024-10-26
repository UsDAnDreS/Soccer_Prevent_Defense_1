#########
## !!!! CHECK THE CODE EXAMPLES for "predict.gam"{mgcv} !!!!
#########

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

# load(file="gam_nb_obj_Corners.Robj")

load(file=paste0("gam_nb_obj",  
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
  
  #########
  ## CHECK THE CODE EXAMPLES for "predict.gam"{mgcv}
  #########
  
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
      mutate(Corners.Adj = Corners*Adj.Coef.ScoreDiff*Adj.Coef.RedCardDiff*Adj.Coef.HomeAway*Adj.Coef.Minute
             #,Corners.Adj.No.Min = Corners*Adj.Coef.ScoreDiff*Adj.Coef.RedCardDiff*Adj.Coef.HomeAway
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
      mutate(Corners.Adj = Corners*Adj.Coef.ScoreDiff*Adj.Coef.RedCardDiff*Adj.Coef.HomeAway,
             Corners.Adj.Min = Corners*Adj.Coef.ScoreDiff.Min*Adj.Coef.RedCardDiff.Min*Adj.Coef.HomeAway.Min,
             Corners.Adj.Max = Corners*Adj.Coef.ScoreDiff.Max*Adj.Coef.RedCardDiff.Max*Adj.Coef.HomeAway.Max
             #,Corners.Adj.No.Min = Corners*Adj.Coef.ScoreDiff*Adj.Coef.RedCardDiff*Adj.Coef.HomeAway
      )
    
  }
  
  
  # ### 
  # cat("\n")
  # print(paste0("Top-10 single game shifts, ", ifelse(include.minute, "WITH MINUTE:", "NO MINUTE:")))
  # 
  final.df <- our.df.pred %>%
    group_by(gameId, Team) %>%
    summarise(Corners = sum(Corners),
              Corners.Adj= sum(Corners.Adj),
              Corners.Adj.Min = sum(Corners.Adj.Min),
              Corners.Adj.Max = sum(Corners.Adj.Max),
              Corners.Diff = Corners.Adj - Corners,
              # Corners.Adj.No.Min = sum(Corners.Adj.No.Min),
              # Corners.Diff.No.Min = Corners.Adj.No.Min - Corners
              ) %>%
    arrange(desc(abs(Corners.Diff)))
  
  # print(head(final.df, 50))
  
  # our.df %>%
  #   filter(gameId == head(final.df$gameId, 1), Team == head(final.df$Team, 1))
  
  
  
  
  #######
  ## IF WE WANT THE TABLE ENTRIES FOR TOP MOVEMENTS
  #######
  
  biggest.jump <- final.df %>% filter(Corners.Diff > 0) %>% dplyr::select(gameId, Team) %>% head(.,1)
  biggest.fall <- final.df %>% filter(Corners.Diff < 0) %>% dplyr::select(gameId, Team) %>% head(.,1)
  
  cat("\n")
  print("Biggest jump and fall, breakdown:")
  biggest.jump.fall <- our.df.pred %>%
    filter(gameId %in% c(biggest.jump$gameId, biggest.fall$gameId),
           Team %in% c(biggest.jump$Team, biggest.fall$Team)) %>%
    group_by(gameId, Team, gamedDate, HomeAway, Score.Diff, RedCard.Diff) %>%
    summarise(Corners = sum(Corners),
              Corners.Adj = sum(Corners.Adj),
              Corners.Adj.Min = sum(Corners.Adj.Min),
              Corners.Adj.Max = sum(Corners.Adj.Max),
              # minutes.spent = sum(minutes.spent)
              minutes.spent=n()
    ) %>%
    mutate(Score.Diff.Categ = ifelse(Score.Diff < 0,
                                     "Trail",
                                     ifelse(Score.Diff > 0,
                                            "Lead",
                                            "0")),
           Score.2.plus.Categ = ifelse(Score.Diff < -1,
                                       "Trail.2.plus",
                                       ifelse(Score.Diff > 1,
                                              "Lead.2.plus",
                                              "0")),
           RedCard.Diff.Categ = ifelse(RedCard.Diff < 0,
                                       "UpMen",
                                       ifelse(RedCard.Diff > 0,
                                              "DownMen",
                                              "0")),
           RedCard.2.plus.Categ = ifelse(RedCard.Diff < -1,
                                         "UpMen.2.plus",
                                         ifelse(RedCard.Diff > 1,
                                                "DownMen.2.plus",
                                                "0"))) %>%
    ungroup()
  # print(biggest.jump.fall)
  
  score.diffs.lead.trail <- biggest.jump.fall %>%
    dplyr::select(-RedCard.Diff) %>%
    group_by(gameId, Team, gamedDate, Score.Diff.Categ) %>%
    summarise(Corners = sum(Corners),
              minutes = sum(minutes.spent),
    ) %>%
    pivot_wider(names_from = c(Score.Diff.Categ),
                values_from = c(Corners, minutes)) %>%
    replace(is.na(.), 0 ) %>%
    mutate(Corners.Total.Actual = Corners_0 + 
             ifelse("Corners_Lead" %in% colnames(.), Corners_Lead, 0)
           + ifelse("Corners_Trail" %in% colnames(.), Corners_Trail, 0),
           #Corners.Total.Adj = sum(Corners.Adj),
           Corners.Min.Lead = paste0(ifelse("Corners_Lead" %in% colnames(.), Corners_Lead, 0), 
                                     " (", 
                                     ifelse("minutes_Lead" %in% colnames(.), minutes_Lead, 0), 
                                     ")"),
           Corners.Min.Trail = paste0(ifelse("Corners_Trail" %in% colnames(.), Corners_Trail, 0), 
                                      " (", 
                                      ifelse("minutes_Trail" %in% colnames(.), minutes_Trail, 0), 
                                      ")")) %>%
    dplyr::select(Corners.Total.Actual,
                  #Corners.Total.Adj,
                  Corners.Min.Lead,# Minutes.Lead,
                  Corners.Min.Trail #, Minutes.Trail
    )
  
  
  score.diffs.2.plus <- biggest.jump.fall %>%
    dplyr::select(-RedCard.Diff) %>%
    group_by(gameId, Team, gamedDate, Score.2.plus.Categ) %>%
    summarise(Corners = sum(Corners),
              minutes = sum(minutes.spent),
    ) %>%
    pivot_wider(names_from = c(Score.2.plus.Categ),
                values_from = c(Corners, minutes)) %>%
    replace(is.na(.), 0 ) %>%
    mutate(Corners.Total.Actual = Corners_0 + 
             ifelse("Corners_Lead.2.plus" %in% colnames(.), Corners_Lead.2.plus, 0)
           + ifelse("Corners_Trail.2.plus" %in% colnames(.), Corners_Trail.2.plus, 0),
           #Corners.Total.Adj = sum(Corners.Adj),
           Corners.Min.Lead.2.plus = paste0(ifelse("Corners_Lead.2.plus" %in% colnames(.), Corners_Lead.2.plus, 0), 
                                            " (", 
                                            ifelse("minutes_Lead.2.plus" %in% colnames(.), minutes_Lead.2.plus, 0), 
                                            ")"),
           Corners.Min.Trail.2.plus = paste0(ifelse("Corners_Trail.2.plus" %in% colnames(.), Corners_Trail.2.plus, 0), 
                                             " (", 
                                             ifelse("minutes_Trail.2.plus" %in% colnames(.), minutes_Trail.2.plus, 0), 
                                             ")")) %>%
    dplyr::select(Corners.Total.Actual,
                  #Corners.Total.Adj,
                  Corners.Min.Lead.2.plus,# Minutes.Lead,
                  Corners.Min.Trail.2.plus #, Minutes.Trail
    )
  
  
  
  
  
  redcard.diffs.lead.trail  <- biggest.jump.fall %>%
    dplyr::select(-Score.Diff) %>%
    group_by(gameId, Team, gamedDate, RedCard.Diff.Categ) %>%
    summarise(Corners = sum(Corners),
              # Corners.Adj = sum(Corners.Adj),
              # minutes.spent = sum(minutes.spent)
              minutes = sum(minutes.spent)
    ) %>%
    pivot_wider(names_from = c(RedCard.Diff.Categ),
                values_from = c(Corners, minutes)) %>%
    replace(is.na(.), 0 ) %>%
    mutate(Corners.Total.Actual = Corners_0 + 
             ifelse("Corners_UpMen" %in% colnames(.), Corners_UpMen, 0)
           + ifelse("Corners_DownMen" %in% colnames(.), Corners_DownMen, 0),
           #Corners.Total.Adj = sum(Corners.Adj),
           Corners.Min.UpMen = paste0(ifelse("Corners_UpMen" %in% colnames(.), Corners_UpMen, 0), 
                                      " (", 
                                      ifelse("minutes_UpMen" %in% colnames(.), minutes_UpMen, 0), 
                                      ")"),
           Corners.Min.DownMen = paste0(ifelse("Corners_DownMen" %in% colnames(.), Corners_DownMen, 0), 
                                        " (", 
                                        ifelse("minutes_DownMen" %in% colnames(.), minutes_DownMen, 0), 
                                        ")")) %>%
    dplyr::select(Corners.Total.Actual,
                  #Corners.Total.Adj,
                  Corners.Min.UpMen,# Minutes.Lead,
                  Corners.Min.DownMen #, Minutes.Trail
    )
  
  
  redcard.diffs.2.plus  <- biggest.jump.fall %>%
    dplyr::select(-Score.Diff) %>%
    group_by(gameId, Team, gamedDate, RedCard.2.plus.Categ) %>%
    summarise(Corners = sum(Corners),
              # Corners.Adj = sum(Corners.Adj),
              # minutes.spent = sum(minutes.spent)
              minutes = sum(minutes.spent)
    ) %>%
    pivot_wider(names_from = c(RedCard.2.plus.Categ),
                values_from = c(Corners, minutes)) %>%
    replace(is.na(.), 0 ) %>%
    mutate(Corners.Total.Actual = Corners_0 + 
             ifelse("Corners_UpMen.2.plus" %in% colnames(.), Corners_UpMen.2.plus, 0)
           + ifelse("Corners_DownMen.2.plus" %in% colnames(.), Corners_DownMen.2.plus, 0),
           #Corners.Total.Adj = sum(Corners.Adj),
           Corners.Min.UpMen.2.plus = paste0(ifelse("Corners_UpMen.2.plus" %in% colnames(.), Corners_UpMen.2.plus, 0), 
                                             " (", 
                                             ifelse("minutes_UpMen.2.plus" %in% colnames(.), minutes_UpMen.2.plus, 0), 
                                             ")"),
           Corners.Min.DownMen.2.plus = paste0(ifelse("Corners_DownMen.2.plus" %in% colnames(.), Corners_DownMen.2.plus, 0), 
                                               " (", 
                                               ifelse("minutes_DownMen.2.plus" %in% colnames(.), minutes_DownMen.2.plus, 0), 
                                               ")")) %>%
    dplyr::select(Corners.Total.Actual,
                  #Corners.Total.Adj,
                  Corners.Min.UpMen.2.plus,# Minutes.Lead,
                  Corners.Min.DownMen.2.plus #, Minutes.Trail
    )
  
  
  
  
  cat("\n")
  # print(score.diffs.lead.trail %>% left_join(redcard.diffs.lead.trail))
  
  
  all.stuff <- score.diffs.lead.trail %>% left_join(redcard.diffs.lead.trail)
  
  all.stuff.lead.trail <- all.stuff %>%
    left_join(biggest.jump.fall %>% group_by(Team, gameId) %>% 
                summarise(Corners.Adj = round(sum(Corners.Adj), 1),
                          Corners.Adj.Min = round(sum(Corners.Adj.Min), 1),
                          Corners.Adj.Max = round(sum(Corners.Adj.Max), 1),
                          Corners.Adj.w.CI = paste0(Corners.Adj, ", ", "[", Corners.Adj.Min, ", ", Corners.Adj.Max, "]")),
              by=c("Team", "gameId"))
  
  
  
  all.stuff.lead.trail <- all.stuff.lead.trail[, c("gameId", "Team", "gamedDate",
                                                   "Corners.Total.Actual", 
                                                   "Corners.Adj.w.CI",
                                                   # "Corners.Adj", "Corners.Adj.Min", "Corners.Adj.Max",
                                                   "Corners.Min.Lead", "Corners.Min.Trail",
                                                   "Corners.Min.UpMen", "Corners.Min.DownMen")]
  
  
  # print(colnames(all.stuff.lead.trail))
  for (j in 1:nrow(all.stuff)){
    print(paste0(all.stuff.lead.trail[j,-1], collapse=" & "))
  }
  
  
  
  ####
  ## If we also want to add the 2+ scenarios
  ####
  
  all.stuff.lead.trail.plus.2 <- all.stuff.lead.trail %>% left_join(score.diffs.2.plus %>% left_join(redcard.diffs.2.plus))
  all.stuff.lead.trail.plus.2 <- all.stuff.lead.trail.plus.2[, c("gameId", "Team", "gamedDate",
                                                                 "Corners.Total.Actual",
                                                                 "Corners.Adj.w.CI",
                                                                 # "Corners.Adj", "Corners.Adj.Min", "Corners.Adj.Max",
                                                                 "Corners.Min.Lead", "Corners.Min.Trail",
                                                                 "Corners.Min.UpMen", "Corners.Min.DownMen",
                                                                 "Corners.Min.Lead.2.plus", "Corners.Min.Trail.2.plus",
                                                                 "Corners.Min.UpMen.2.plus", "Corners.Min.DownMen.2.plus")]
  
  # print(colnames(all.stuff.lead.trail.plus.2))
  for (j in 1:nrow(all.stuff)){
    print(paste0(all.stuff.lead.trail.plus.2[j,-1], collapse=" & "))
  }
  
  
  ##
  # biggest.jump.fall %>%
  #   group_by(gameId, Team) %>%
  #   summarise(Corners = sum(Corners),
  #             Corners.Adj = sum(Corners.Adj),
  #             minutes.spent=sum(minutes.spent))
  #
  # biggest.jump.fall %>%
  #   group_by(gameId, Team, Score.Diff) %>%
  #   summarise(minutes.spent=sum(minutes.spent))
  
  
  
  cat("\n")
  cat("\n")
}



######
## SHOTS
######

# [1] "Eintracht Frankfurt & 2011-02-27 & 30 & 22 [21.3, 22.9] & 0 (0) & 12 (26) & 26 (76) & 0 (0) & 0 (0) & 12 (22) & 0 (0) & 0 (0)"
# [1] "Bayern Munich & 2021-03-20 & 15 & 29.9 [27.5, 32.6] & 12 (76) & 0 (0) & 0 (0) & 14 (82) & 11 (71) & 0 (0) & 0 (0) & 0 (0)"
# 
# [1] "AS Roma & 2010-01-09 & 17 & 30.8 [29.5, 32.3] & 15 (90) & 0 (0) & 0 (0) & 13 (80) & 0 (0) & 0 (0) & 0 (0) & 0 (0)"
# [1] "AS Roma & 2022-05-14 & 43 & 28.1 [27.2, 29] & 0 (0) & 29 (79) & 38 (70) & 0 (0) & 0 (0) & 0 (0) & 0 (0) & 0 (0)"
# 
# [1] "Getafe & 2009-11-07 & 22 & 12.6 [11.8, 13.5] & 0 (0) & 22 (79) & 19 (66) & 0 (0) & 0 (0) & 14 (32) & 9 (19) & 0 (0)"
# [1] "Real Madrid & 2011-02-13 & 18 & 36.2 [34, 38.4] & 13 (66) & 0 (0) & 0 (0) & 18 (89) & 0 (0) & 0 (0) & 0 (0) & 0 (0)"
# 
# [1] "Caen & 2015-09-12 & 15 & 28.9 [27, 31] & 13 (78) & 0 (0) & 0 (0) & 9 (38) & 11 (61) & 0 (0) & 0 (0) & 0 (0)"
# [1] "Bordeaux & 2022-03-20 & 31 & 14.7 [13.1, 16.4] & 0 (0) & 31 (94) & 31 (66) & 0 (0) & 0 (0) & 31 (89) & 27 (54) & 0 (0)"
# 
# [1] "Blackpool & 2010-11-01 & 26 & 13.2 [11.1, 15.7] & 25 (79) & 0 (0) & 26 (81) & 0 (0) & 6 (23) & 0 (0) & 25 (62) & 0 (0)"
# [1] "Arsenal & 2017-05-21 & 17 & 34.8 [32.6, 37.1] & 15 (91) & 0 (0) & 0 (0) & 15 (85) & 6 (38) & 0 (0) & 0 (0) & 0 (0)"




######
## CORNERS
######



# [1] "Eintracht Frankfurt & 2011-02-27 & 13 & 9, [8.4, 9.7] & 0 (0) & 5 (26) & 13 (76) & 0 (0) & 0 (0) & 5 (22) & 0 (0) & 0 (0)"
# [1] "Eintracht Frankfurt & 2013-05-18 & 6 & 3.9, [3.7, 4.2] & 0 (0) & 6 (87) & 5 (62) & 0 (0) & 0 (0) & 1 (18) & 0 (0) & 0 (0)"
# [1] "VfL Wolfsburg & 2013-05-18 & 9 & 17.9, [16, 20] & 8 (87) & 0 (0) & 0 (0) & 7 (62) & 1 (18) & 0 (0) & 0 (0) & 0 (0)"
# 
# [1] "AS Roma & 2012-11-04 & 15 & 25.7, [24.3, 27.1] & 13 (85) & 0 (0) & 0 (0) & 2 (14) & 13 (65) & 0 (0) & 0 (0) & 0 (0)"
# [1] "AS Roma & 2022-05-14 & 20 & 13, [12.4, 13.7] & 0 (0) & 14 (79) & 18 (70) & 0 (0) & 0 (0) & 0 (0) & 0 (0) & 0 (0)"
# 
# [1] "Getafe & 2009-11-07 & 14 & 8.5, [7.7, 9.6] & 0 (0) & 13 (79) & 11 (66) & 0 (0) & 0 (0) & 8 (32) & 7 (19) & 0 (0)"
# [1] "Barcelona & 2020-10-01 & 8 & 21.8, [19.5, 24.4] & 8 (91) & 0 (0) & 0 (0) & 7 (60) & 6 (45) & 0 (0) & 0 (0) & 0 (0)"
# 
# [1] "Paris Saint-Germain & 2012-11-17 & 18 & 9.5, [8.6, 10.6] & 0 (0) & 17 (71) & 16 (72) & 0 (0) & 0 (0) & 0 (0) & 11 (41) & 0 (0)"
# [1] "Marseille & 2018-05-11 & 11 & 18.9, [17.6, 20.2] & 2 (53) & 0 (11) & 0 (0) & 8 (31) & 1 (28) & 0 (0) & 0 (0) & 0 (0)"
# 
# [1] "Manchester City & 2012-12-29 & 9 & 17.2, [15.7, 18.8] & 9 (101) & 0 (0) & 0 (0) & 4 (59) & 4 (30) & 0 (0) & 0 (0) & 0 (0)"
# [1] "AFC Bournemouth & 2019-05-04 & 10 & 5.9, [5.1, 6.8] & 0 (5) & 0 (0) & 9 (58) & 0 (0) & 0 (0) & 0 (0) & 7 (49) & 0 (0)"
