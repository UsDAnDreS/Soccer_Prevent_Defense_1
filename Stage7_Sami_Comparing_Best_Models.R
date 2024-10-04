#######
## Getting the:
##    1. "COMMON DENOMINATOR" DUMMY VARIABLE MODELS (-4 to +5)
##    2. Include the EXTREME VERSIONS (Where stuff gets merged into extreme rather than baseline)
#######


library(mgcv)
library(tidyverse)
library(splines)
library(MASS)
library(mpath)
library(zic)
library(pscl)


league.name <- c("Bundesliga", "SerieA", "LaLiga", "Ligue1", "PremierLeague")


gam.dummy.baseline.obj <- gam.dummy.extreme.obj <- list()

load(file="gam_dummy_baseline_obj.Robj")
load(file="gam_dummy_extreme_obj.Robj")



# for (league in league.name){
#   
#   cat("\n")
#   cat("\n")
#   cat("\n")
#   print(league)
#   
#   our.df <- read.csv(paste0(league, "_Sami_minute_by_minute_df_w_red_cards_WITH_BOOKING_ODDS.csv"))
#   
#   
#   ## REMOVING NA betting data
#   our.df <- our.df %>% filter(!is.na(Weighted.Win.Prob))
#   dim(our.df)
#   
#   ## Remove extra time data from first half (so anything with half_id=1, minute>45)
#   ## => TO AVOID "POINT MASS"/"OUT-OF-ORDER MINUTE" ISSUES...
#   ##
#   ## !!! BUUUT... the >90 IS ALSO AN ISSUE... data is way too sporadic over there !!!
#   final.df <- our.df %>% filter(!(half_id == 1 & Minute.clean > 45) & Minute.clean < 90)
#   print(dim(final.df))
#   
# 
#   print("Baseline")
#   gam.dummy.baseline.obj[[league]] <- gam(Shots ~
#              # Corners ~
#              Score.Diff + Minute.clean + Weighted.Win.Prob  + HomeAway + RedCard.Diff,
#              family="ziP",
#                     data=final.df %>% mutate(ID = factor(ID), Score.Diff = relevel(factor(
#                       ifelse(Score.Diff > 5 | Score.Diff < -4,
#                              "0",
#                              Score.Diff)),
#                       ref="0")))
# 
#   print("Extreme")
#   gam.dummy.extreme.obj[[league]] <- gam(Shots ~
#                               # Corners ~
#                               Score.Diff + Minute.clean + Weighted.Win.Prob  + HomeAway + RedCard.Diff,
#                             family="ziP",
#                             data=final.df %>% mutate(ID = factor(ID), Score.Diff = relevel(factor(
#                               ifelse(Score.Diff > 4,
#                                      "+5.or.better",
#                                      ifelse(Score.Diff < -3, 
#                                             "-4.or.worse",
#                                             Score.Diff))
#                               ),
#                             ref="0")
#                             ))
#   
#   
#   save(gam.dummy.baseline.obj, file="gam_dummy_baseline_obj.Robj")
#   save(gam.dummy.extreme.obj, file="gam_dummy_extreme_obj.Robj")
# 
# }


for (league in league.name){
  print(league)
  print(BIC(gam.dummy.baseline.obj[[league]], gam.dummy.extreme.obj[[league]]))
}


########
### SHOTS:
########

## TAKEAWAYS:
##  1. Extreme is better 3/5 cases... but coin-flippy
##  2. GAMs are BETTER all the time... dummy var models actually have MORE parameters (higher complexity)


# [1] "Bundesliga"
# df      BIC
# gam.dummy.baseline.obj[[league]] 16 663199.8
# gam.dummy.extreme.obj[[league]]  16 663184.9
# [1] "SerieA"
# df      BIC
# gam.dummy.baseline.obj[[league]] 16 775458.5
# gam.dummy.extreme.obj[[league]]  16 775437.0
# [1] "LaLiga"
# df      BIC
# gam.dummy.baseline.obj[[league]] 16 670480.1
# gam.dummy.extreme.obj[[league]]  16 670464.0
# [1] "Ligue1"
# df      BIC
# gam.dummy.baseline.obj[[league]] 16 753324.9
# gam.dummy.extreme.obj[[league]]  16 753335.8
# [1] "PremierLeague"
# df      BIC
# gam.dummy.baseline.obj[[league]] 16 673922.7
# gam.dummy.extreme.obj[[league]]  16 673929.3


# [1] "Bundesliga"
# df      BIC
# gam.ziP.obj[[league]] 14.12755 663159.6
# 
# [1] "SerieA"
# df      BIC
# gam.ziP.obj[[league]] 14.23329 775420.6
# 
# [1] "LaLiga"
# df      BIC
# gam.ziP.obj[[league]] 14.16929 670444.4
# 
# [1] "Ligue1"
# df      BIC
# gam.ziP.obj[[league]] 14.32778 753330.4
# 
# [1] "PremierLeague"
# df      BIC
# gam.ziP.obj[[league]] 13.69991 673908.6