
library(mgcv)
library(tidyverse)
library(splines)
library(MASS)
library(DHARMa)


## Whether to remove extra time, to align with minute-by-minute approach
## (plus take care of the issue where events get stacked up on 90th minute, instead of going into 91+)
remove.extra <- TRUE

league.name <- c("Bundesliga", "SerieA", "LaLiga", "Ligue1", "PremierLeague")


TEST_gam.nb.obj <- list()

# "NO_MINUTE", "NO_SCOREDIFF", "NO_REDCARDDIFF", "NO_WINPROBDIFF", "NO_HOMEAWAY", 
# "SCOREDIFF_MINUTE_INT", "REDCARDDIFF_MINUTE_INT", "SCOREDIFF_REDCARDDIFF_INT", 
# "WINPROBEDIFF_SCOREDIFF_INT", "WINPROBEDIFF_REDCARDDIFF_INT", "WINPROBEDIFF_MINUTE_INT"
string_add <- "WINPROBEDIFF_MINUTE_INT"

# load("TEST_gam_nb_obj_Corners.Robj")

# load(file=paste0("TEST_gam_nb_obj", "_", string_add, "_Corners.Robj"))



for (league in league.name){
  
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
  
  TEST_gam.nb.obj[[league]] <- gam(Corners ~ 
                                     s(Score.Diff) 
                                   # + s(Weighted.Win.Prob)  
                                   + HomeAway  
                                   + s(RedCard.Diff, k=5) 
                                   # + s(Minute.clean)
                                   # + te(Score.Diff, Minute.clean)
                                   # + te(RedCard.Diff, Minute.clean)
                                   # +te(Score.Diff, RedCard.Diff)
                                   # +te(Score.Diff, Weighted.Win.Prob)
                                   # +te(RedCard.Diff, Weighted.Win.Prob)
                                   +te(Weighted.Win.Prob, Minute.clean)
                                   ,
                                   family="nb", data= our.df.cut)
  
  save(file=paste0("TEST_gam_nb_obj", "_", string_add, "_Corners.Robj"),
       TEST_gam.nb.obj)
  
  
  print(league)
  print(BIC(TEST_gam.nb.obj[[league]]))
  
  cat("\n")
}


#load(paste0("NO_MINUTES_TEST_gam_nb_obj_NO_EXTRA_TIME_Corners.Robj"))

# load(paste0("gam_nb_obj", "_", string_add, "_Corners.Robj"))

## 
print("BICs:")
for (league in league.name){
  print(BIC(gam.nb.obj[[league]],
            TEST_gam.nb.obj[[league]]))
}


print("AICs:")
for (league in league.name){
  print(AIC(gam.nb.obj[[league]],
            TEST_gam.nb.obj[[league]]))
}