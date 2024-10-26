######
## "LeagueName_Sami_minute_by_minute_df_w_red_cards_WITH_BOOKING_ODDS.csv"
######

require(mgcv)
# require(tidyverse)
require(dplyr)


# library(splines)
# library(MASS)
# library(DHARMa)


## Whether to remove extra time, to align with minute-by-minute approach
## (plus take care of the issue where events get stacked up on 90th minute, instead of going into 91+)
remove.extra <- TRUE

league.name <- c("Bundesliga", "SerieA", "LaLiga", "Ligue1", "PremierLeague")

## Covered: "BASELINE", "NO_REDCARDDIFF", "NO_WINPROBDIFF", "NO_SCOREDIFF", "NO_MINUTE", "NO_HOMEAWAY",
##          "SCOREDIFF_MINUTE_INT", "REDCARDDIFF_MINUTE_INT",
##          "WINPROBEDIFF_SCOREDIFF_INT", "SCOREDIFF_REDCARDDIFF_INT",
##          "WINPROBEDIFF_REDCARDDIFF_INT", "WINPROBEDIFF_MINUTE_INT"

string_add <- "NO_WINPROBDIFF"

pred.list <- list()

load(paste0("LEAVE_SEASON_OUT_PREDICTIONS_gam_nb_obj", "_", string_add, "_Corners.Robj"))


for (league in league.name[4]){
  print(string_add)
  
  print(league)
  
  #our.df <- read.csv(paste0(league, "_Sami_minute_by_minute_df_w_red_cards_WITH_BOOKING_ODDS.csv"))
  
  our.df.cut <- read.csv(paste0(league, ifelse(remove.extra, "_NO_EXTRA_TIME", ""), "_Sami_minute_by_minute_df_w_red_cards_WITH_BOOKING_ODDS.csv"))
  
  
  ## REMOVING NA betting data
  our.df.cut <- our.df.cut %>% filter(!is.na(Weighted.Win.Prob))
  
  
  ## Remove extra time data from first half (so anything with half_id=1, minute>45)
  ## => TO AVOID "POINT MASS"/"OUT-OF-ORDER MINUTE" ISSUES...
  ##
  ## !!! BUUUT... the >90 IS ALSO AN ISSUE... data is way too sporadic over there !!!
  # our.df <- our.df %>% filter(!(half_id == 1 & Minute.clean > 45) & Minute.clean <90)
  # print(dim(our.df))
  
  
  
  ############
  ############
  
  our.df.cut  <- our.df.cut  %>%
    mutate(abs.Score.Diff = abs(Score.Diff),
           abs.RedCard.Diff = abs(RedCard.Diff))
  
  our.df.cut  <- our.df.cut  %>%
    mutate(Period.ID = group_indices(our.df.cut, .dots=c("ID", "abs.Score.Diff", "abs.RedCard.Diff"))) %>%
    arrange(Period.ID) %>%
    mutate(Period.ID = factor(Period.ID))
  
  our.df.cut <- our.df.cut %>% mutate(Period.ID = factor(Period.ID), ID = factor(ID))
  
  
  
  ######
  ## Fitting SPLINE models
  ######
  
  for (year in 2023){
    if (league == "LaLiga" & year %in% c(2016, 2022)) next;
    if (league %in% c("SerieA") & year == 2016) next;
    if (league == "Ligue1" & year == 2022) next;
    if (league == "PremierLeague" & year == 2009) next;
    

    print(year)
  gam.obj <- gam(Corners ~ 
                                     s(Score.Diff) 
                                #  +  s(Weighted.Win.Prob)  
                                   + HomeAway  
                                    + s(RedCard.Diff, k=5) 
                                   + s(Minute.clean)
                 
                                   # + te(Score.Diff, Minute.clean)
                                   # + te(RedCard.Diff, Minute.clean)
                 
                                 #  +te(Score.Diff, Weighted.Win.Prob)
                                   # +te(Score.Diff, RedCard.Diff)
    
                                    #+te(RedCard.Diff, Weighted.Win.Prob)
                                 #  +te(Weighted.Win.Prob, Minute.clean)
                                   ,
                                   family="nb", data= our.df.cut,
                                  subset = (season != year))
  
  our.true <- our.df.cut$Corners[our.df.cut$season == year]
  our.pred <- predict(gam.obj,
                      our.df.cut[our.df.cut$season == year, ], type="response")
  print(length(our.true))
  print(length(our.pred))
  
 if (year == 2009){
   pred.list[[league]] <- data.frame(Year = year, 
                                     True = our.true,
                                     Pred = our.pred)
 } else {
   pred.list[[league]] <- rbind(pred.list[[league]],
                                data.frame(Year = year, 
                                           True = our.true,
                                           Pred = our.pred))
 }
  save(file=paste0("LEAVE_SEASON_OUT_PREDICTIONS_gam_nb_obj", "_", string_add, "_Corners.Robj"),
       pred.list)
  }
  

  # dim(pred.list[[1]])
  
 # print(league)
 # print(BIC(TEST_gam.nb.obj[[league]]))
  
  cat("\n")
  cat("\n")
}



