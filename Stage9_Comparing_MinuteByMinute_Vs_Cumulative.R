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

gam.nb.obj <- list()
gam.cumul.nb.obj <- gam.cumul.ziP.obj <- list()

for (league in league.name){
  
  print(league)
  
  # our.df <- read.csv(paste0(league, "_Sami_final_df_w_red_cards_WITH_BOOKING_ODDS.csv"))
  
  ## To be used for modeling (excluding bad minute entries)
  our.df <- read.csv(paste0(league, "_NO_EXTRA_TIME_Sami_final_df_w_red_cards_WITH_BOOKING_ODDS.csv"))
  print(length(unique(our.df$gameId)))
  
  ## REMOVING NA betting data
  our.df <- our.df %>% filter(!is.na(Weighted.Win.Prob))
  dim(our.df)
  
  ## For EXTRA TIME: JUST FORCE IT TO BE EQUAL TO THE LAST MINUTE (e.g. for extra time in 1st half - make it =45, in 2nd half - =90)
  
  # our.df$Minute.clean[our.df$half_id == 1 & our.df$Minute.clean > 45] <- 45
  # our.df$Minute.clean[our.df$Minute.clean > 90] <- 90
  
  
  our.df  <- our.df  %>%
    mutate(abs.Score.Diff = abs(Score.Diff),
           abs.RedCard.Diff = abs(RedCard.Diff))
  
  our.df  <- our.df  %>%
    mutate(Period.ID = group_indices(our.df, .dots=c("ID", "abs.Score.Diff", "abs.RedCard.Diff"))) %>%
    arrange(Period.ID) %>%
    mutate(Period.ID = factor(Period.ID))
  
  our.df <- our.df %>% mutate(Period.ID = factor(Period.ID), ID = factor(ID))
  
  
  ######
  ### Fitting the models
  ######
  
  gam.nb.obj[[league]] <- gam( Shots ~
                                 s(Score.Diff) + s(Weighted.Win.Prob)  + HomeAway + s(RedCard.Diff, k=5) + log(minutes.spent + 1),
                               family = "nb", data= our.df)
  
  
  head(gam.nb.obj[[league]]$model)
  dim(gam.nb.obj[[league]]$model)
  
  pred.vals.cumul <- predict(gam.nb.obj[[league]], type="response")
  our.df.cumul <- data.frame(our.df, pred.vals.cumul)
  
  
  
  ## To be used for prediction (including everything, so that totals correspond to full data)
  # our.df.pred <- read.csv(paste0(league, "_Sami_final_df_w_red_cards_WITH_BOOKING_ODDS.csv"))
  our.df.mbm <- read.csv(paste0(league, "_NO_EXTRA_TIME_Sami_minute_by_minute_df_w_red_cards_WITH_BOOKING_ODDS.csv"))
  
  ## REMOVING NA betting data
  our.df.mbm <- our.df.mbm %>% filter(!is.na(Weighted.Win.Prob))
  dim(our.df.mbm)
  
  
  load(file=paste0(ifelse(include.minute,
                          "",
                          "NO_MINUTES_TEST_"),
                   "gam_ziP_obj",  
                   ifelse(remove.extra, "_NO_EXTRA_TIME", ""), 
                   "_Shots.Robj"))

  if (include.minute){
    pred.vals.MBM <- predict(gam.ziP.obj[[league]], type="response")
  } else {
    pred.vals.MBM <- predict(TEST_gam.ziP.obj[[league]], type="response")
  }
  
  
  MBM.pred.df <- data.frame(our.df.mbm, pred.vals.MBM)
  head(MBM.pred.df)
  
  MBM.grouped.df <- MBM.pred.df %>%
    group_by(gameId, Team, half_id, Score.Diff, RedCard.Diff, Weighted.Win.Prob, HomeAway) %>%
    summarise(Shots = sum(Shots),
              Shots.Pred = sum(pred.vals.MBM)) %>%
    group_by(gameId, Team,  Score.Diff, RedCard.Diff, Weighted.Win.Prob, HomeAway) %>%
      summarise(Shots=sum(Shots),
                Shots.Pred = sum(Shots.Pred))
  
  dim(MBM.grouped.df)
  
  
  
  
  ####
  ## COMBINING
  ####
  
  full.df <- our.df.cumul %>%
    left_join(MBM.grouped.df)
  
  mean(abs(full.df$Shots - full.df$Shots.Pred), na.rm=T)
  mean(abs(full.df$Shots - full.df$pred.vals.cumul), na.rm=T)
  
  mean((full.df$Shots - full.df$Shots.Pred)^2, na.rm=T)
  mean((full.df$Shots - full.df$pred.vals.cumul)^2, na.rm=T)
    
  
  
  
  # print(league)
  # print(BIC(#glm.obj[[league]], glm.nb.obj[[league]], glm.ziP.obj[[league]],
  #   gam.cumul.nb.obj[[league]], gam.cumul.ziP.obj[[league]]))
  # 
  # gam.cumul.nb.obj[[league]]$null.deviance
  # 
  # 
  # BIC.obj <-  print(BIC(#glm.obj[[league]], glm.nb.obj[[league]], glm.ziP.obj[[league]],
  #   gam.cumul.nb.obj[[league]], gam.cumul.ziP.obj[[league]]))
  # BIC.obj
  # BIC.obj$df[1]*log(gam.cumul.nb.obj[[league]]$df.null)
  # 
  # ## Figuring out BIC calculation:
  # -2*logLik.gam(gam.cumul.nb.obj[[league]]) + BIC.obj$df[1]*log(gam.cumul.nb.obj[[league]]$df.null)
  # 
  # 
  # # BIC.obj <- BIC(#glm.obj[[league]], glm.nb.obj[[league]], glm.ziP.obj[[league]],
  # #   gam.cumul.obj[[league]])
  # 
  # cat("\n")
}