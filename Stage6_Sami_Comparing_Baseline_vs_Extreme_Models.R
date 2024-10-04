library(mgcv)
library(tidyverse)
library(splines)
library(MASS)


league.name <- c("Bundesliga", "SerieA", "LaLiga", "Ligue1", "PremierLeague")



##########
##########
### SHOTS
##########
##########


extr.score <- c(1, 2, 3, 4,5)[1]

for (league in league.name){
  
  print(league)
  
  # load("final_df_w_red_cards.csv")
  
  final.df <- read.csv(paste0(league, "_Sami_final_df_w_red_cards_WITH_BOOKING_ODDS.csv"))
  
  #########
  ## Fitting the Neg Bin regression with:
  ##    * Score.Diff as DUMMY variables with "0" as reference level 
  ##      (achieved via mutate(Score.Diff = relevel(factor(Score.Diff), ref="0")))
  ##    * Merge the RIGHT-HAND extremes into MOST RIGHTHAND CATEGORY, while LEFT-HAND - into MOST LEFTHAND
  ##    * Making effects of Weighted.Win.Prob & RedCard.Diff linear
  #########
  
  
  glm.extreme.obj <- glm.nb(Shots ~ Score.Diff + Weighted.Win.Prob  + HomeAway + RedCard.Diff + offset(log(minutes.spent+1)),
                                  data=final.df %>% mutate(ID = factor(ID),
                                                           Score.Diff = relevel(factor(
                                                             ifelse(Score.Diff >= extr.score,
                                                                    paste0(extr.score, ".or.better"),
                                                                    ifelse(Score.Diff <= -extr.score,
                                                                           paste0(-extr.score, ".or.worse"),
                                                                           Score.Diff))), ref="0")))
  glm.extreme.obj
  
  
  
  #########
  ## Fitting the Neg Bin regression with:
  ##    * Score.Diff as DUMMY variables with "0" as reference level 
  ##      (achieved via mutate(Score.Diff = relevel(factor(Score.Diff), ref="0")))
  ##    * MERGE THE EXTREMES into BASELINE
  ##    * Making effects of Weighted.Win.Prob & RedCard.Diff linear
  #########
  
  # glm.baseline.obj <- glm.nb(Shots ~ Score.Diff + Weighted.Win.Prob  + HomeAway + RedCard.Diff + offset(log(minutes.spent+1)),
  #                                 data=final.df %>% mutate(ID = factor(ID),
  #                                                          Score.Diff = relevel(factor(
  #                                                            ifelse(abs(Score.Diff) >= extr.score,
  #                                                                   "0",
  #                                                                   Score.Diff)),
  #                                                            ref="0")))
  # glm.baseline.obj
  
  
  
  
  #  print("BIC:")
  print(AIC(glm.extreme.obj, glm.baseline.obj, k=log(length(glm.extreme.obj$y))))
  
  # print(glm.no.int.offset.obj$coefficients)
  
}

## Merging -2.or.extreme, +2.or.extreme

# [1] "Bundesliga"
# df      AIC
# glm.extreme.obj   9 110345.5
# glm.baseline.obj  7 110746.7
# [1] "SerieA"
# df      AIC
# glm.extreme.obj   9 126344.9
# glm.baseline.obj  7 127935.4
# [1] "LaLiga"
# df      AIC
# glm.extreme.obj   9 114138.2
# glm.baseline.obj  7 115098.2
# [1] "Ligue1"
# df      AIC
# glm.extreme.obj   9 124455.3
# glm.baseline.obj  7 125212.5
# [1] "PremierLeague"
# df      AIC
# glm.extreme.obj   9 111313.6
# glm.baseline.obj  7 111886.6


## Merging -3.or.extreme, +3.or.extreme

# [1] "Bundesliga"
# df      AIC
# glm.extreme.obj  11 110349.1
# glm.baseline.obj  9 110440.1
# [1] "SerieA"
# df      AIC
# glm.extreme.obj  11 126353.6
# glm.baseline.obj  9 126930.1
# [1] "LaLiga"
# df      AIC
# glm.extreme.obj  11 114156.4
# glm.baseline.obj  9 114487.7
# [1] "Ligue1"
# df      AIC
# glm.extreme.obj  11 124468.3
# glm.baseline.obj  9 124754.1
# [1] "PremierLeague"
# df      AIC
# glm.extreme.obj  11 111329.7
# glm.baseline.obj  9 111552.6


## Mering -4.or.extreme, +4.or.extreme

# [1] "Bundesliga"
# df      AIC
# glm.extreme.obj  13 110356.5
# glm.baseline.obj 11 110365.4
# [1] "SerieA"
# df      AIC
# glm.extreme.obj  13 126370.7
# glm.baseline.obj 11 126532.9
# [1] "LaLiga"
# df      AIC
# glm.extreme.obj  13 114176.2
# glm.baseline.obj 11 114282.5
# [1] "Ligue1"
# df      AIC
# glm.extreme.obj  13 124485.5
# glm.baseline.obj 11 124545.8
# [1] "PremierLeague"
# df      AIC
# glm.extreme.obj  13 111347.5
# glm.baseline.obj 11 111384.4



######
## ALL-IN-ALL:
##    -4 to +4 


## Mering -5.or.extreme, +5.or.extreme

# [1] "Bundesliga"
# df      AIC
# glm.extreme.obj  15 110376.3
# glm.baseline.obj 13 110369.1
# [1] "SerieA"
# df      AIC
# glm.extreme.obj  15 126386.5
# glm.baseline.obj 13 126438.1
# [1] "LaLiga"
# df      AIC
# glm.extreme.obj  15 114196.4
# glm.baseline.obj 13 114210.3
# [1] "Ligue1"
# df      AIC
# glm.extreme.obj  15 124496.9
# glm.baseline.obj 13 124489.5
# [1] "PremierLeague"
# df      AIC
# glm.extreme.obj  15 111361.6
# glm.baseline.obj 13 111349.1


#####
## BEST SIMPLEST MODEL FOR SHOTS IS:
##    -2 to +2, EXTREMES
#####





##########
##########
### CORNERS
##########
##########


extr.score <- c(1, 2, 3, 4)[1]

for (league in league.name){
  
  print(league)
  
  # load("final_df_w_red_cards.csv")
  
  final.df <- read.csv(paste0(league, "_Sami_final_df_w_red_cards_WITH_BOOKING_ODDS.csv"))
  
  #########
  ## Fitting the Neg Bin regression with:
  ##    * Score.Diff as DUMMY variables with "0" as reference level 
  ##      (achieved via mutate(Score.Diff = relevel(factor(Score.Diff), ref="0")))
  ##    * Merge the RIGHT-HAND extremes into MOST RIGHTHAND CATEGORY, while LEFT-HAND - into MOST LEFTHAND
  ##    * Making effects of Weighted.Win.Prob & RedCard.Diff linear
  #########
  
  
  glm.extreme.obj <- glm.nb(Corners ~ Score.Diff + Weighted.Win.Prob  + HomeAway + RedCard.Diff + offset(log(minutes.spent+1)),
                            data=final.df %>% mutate(ID = factor(ID),
                                                     Score.Diff = relevel(factor(
                                                       ifelse(Score.Diff >= extr.score,
                                                              paste0(extr.score, ".or.better"),
                                                              ifelse(Score.Diff <= -extr.score,
                                                                     paste0(-extr.score, ".or.worse"),
                                                                     Score.Diff))), ref="0")))
  glm.extreme.obj
  
  
  
  #########
  ## Fitting the Neg Bin regression with:
  ##    * Score.Diff as DUMMY variables with "0" as reference level 
  ##      (achieved via mutate(Score.Diff = relevel(factor(Score.Diff), ref="0")))
  ##    * MERGE THE EXTREMES into BASELINE
  ##    * Making effects of Weighted.Win.Prob & RedCard.Diff linear
  #########
  
  # glm.baseline.obj <- glm.nb(Corners ~ Score.Diff + Weighted.Win.Prob  + HomeAway + RedCard.Diff + offset(log(minutes.spent+1)),
  #                                 data=final.df %>% mutate(ID = factor(ID),
  #                                                          Score.Diff = relevel(factor(
  #                                                            ifelse(abs(Score.Diff) >= extr.score,
  #                                                                   "0",
  #                                                                   Score.Diff)),
  #                                                            ref="0")))
  # glm.baseline.obj
  
  
  
  
  #  print("BIC:")
  print(AIC(glm.extreme.obj, #glm.baseline.obj, 
            k=log(length(glm.extreme.obj$y))))
  
  # print(glm.no.int.offset.obj$coefficients)
  
}


## -1.or.extr, +1.or.extr
# (just the extremes)

# [1] "Bundesliga"
# [1] 77437.78
# [1] "SerieA"
# [1] 90167.97
# [1] "LaLiga"
# [1] 82438.71
# [1] "Ligue1"
# [1] 89218.22
# [1] "PremierLeague"
# [1] 80636.05



### Merging -2.or.extreme, +2.or.extreme

# [1] "Bundesliga"
# df      AIC
# glm.extreme.obj   9 77430.75
# glm.baseline.obj  7 77820.62
# [1] "SerieA"
# df      AIC
# glm.extreme.obj   9 90098.57
# glm.baseline.obj  7 91194.12
# [1] "LaLiga"
# df      AIC
# glm.extreme.obj   9 82406.28
# glm.baseline.obj  7 83223.04
# [1] "Ligue1"
# df      AIC
# glm.extreme.obj   9 89203.74
# glm.baseline.obj  7 89954.08
# [1] "PremierLeague"
# df      AIC
# glm.extreme.obj   9 80647.61
# glm.baseline.obj  7 81117.70


### Mering -3.or.extreme, +3.or.extreme

# [1] "Bundesliga"
# df      AIC
# glm.extreme.obj  11 77430.23
# glm.baseline.obj  9 77513.61
# [1] "SerieA"
# df      AIC
# glm.extreme.obj  11 90113.16
# glm.baseline.obj  9 90522.62
# [1] "LaLiga"
# df      AIC
# glm.extreme.obj  11 82426.68
# glm.baseline.obj  9 82683.28
# [1] "Ligue1"
# df      AIC
# glm.extreme.obj  11 89222.92
# glm.baseline.obj  9 89456.46
# [1] "PremierLeague"
# df      AIC
# glm.extreme.obj  11 80663.29
# glm.baseline.obj  9 80844.87




### Mering -4.or.extreme, +4.or.extreme

# [1] "Bundesliga"
# df      AIC
# glm.extreme.obj  13 77437.16
# glm.baseline.obj 11 77450.22
# [1] "SerieA"
# df      AIC
# glm.extreme.obj  13 90123.03
# glm.baseline.obj 11 90224.07
# [1] "LaLiga"
# df      AIC
# glm.extreme.obj  13 82445.31
# glm.baseline.obj 11 82503.28
# [1] "Ligue1"
# df      AIC
# glm.extreme.obj  13 89236.46
# glm.baseline.obj 11 89259.10
# [1] "PremierLeague"
# df      AIC
# glm.extreme.obj  13 80681.25
# glm.baseline.obj 11 80708.43



#####
## BEST SIMPLEST MODEL FOR CORNERS IS:
##    -2 to +2, EXTREMES
#####